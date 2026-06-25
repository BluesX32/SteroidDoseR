# manual_review.R  (extras/)
# Generates a long-format CSV pairing gold-standard episodes with the
# computed drug-exposure records that overlap them, then launches a Shiny
# dashboard for patient-by-patient manual review.
#
# CSV structure (one row per gold episode OR computed record)
# ----------------------------------------------------------
#   source               "gold" | "computed_record"
#   match_id             integer — links a gold episode to its records
#   patient_id
#   drug_name_std
#   start_date           episode_start (gold) | drug_exposure_start (computed)
#   end_date
#   duration_days
#   daily_dose_mg        gold dose (gold) | computed record dose (computed)
#   computed_dose_for_ep algorithm's episode-level dose  (gold rows only)
#   agreement_category   Exact / Good / Moderate / Poor  (gold rows only)
#   absolute_error_mg                                     (gold rows only)
#   bias_error_mg                                         (gold rows only)
#   error_direction      Over- / Under-estimation         (gold rows only)
#   method               hierarchical_method              (computed rows only)
#   sig_type             steady/taper/prn/…               (computed rows only)
#   sig_text             raw SIG string                   (computed rows only)
#   sig_status           parser status                    (computed rows only)
#   bl_dose              baseline dose                    (computed rows only)
#   sig_dose_nlp         NLP dose                         (computed rows only)

devtools::install_local(getwd())
if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)

# ===========================================================================
# 0. Configuration
# ===========================================================================
USE_SYNTHETIC  <- FALSE
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
DIFF_THRESHOLD <- 5
MATCH_TOL      <- 0.01
GAP_DAYS       <- 30L
CONCURRENT_AGG <- "per_drug"

GOLD_STD_PATH     <- "/your/path/to/gold-standard"
COHORT_PERSON_IDS <- NULL
OUTPUT_DIR        <- file.path(getwd(), "output_manual_review")

CONFLICT_THR_MG <- 20   # |bl_dose - sig_dose| above this → "conflicting"

STEROID_CONCEPT_IDS <- as.integer(readr::read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

# ===========================================================================
# 1. Load data
# ===========================================================================
if (!USE_SYNTHETIC) {
  # ── Option B: DatabaseConnector ──────────────────────────────────────────
  # cdm_schema   <- "database.dbo"
  # vocab_schema <- "database.dbo"
  # connectionDetails <- DatabaseConnector::createConnectionDetails(...)
  # conn <- DatabaseConnector::connect(connectionDetails)

  # ── Option C: DBI / odbc ─────────────────────────────────────────────────
  # library(DBI); library(odbc)
  # DB_DIALECT <- "spark"
  # conn <- DBI::dbConnect(...)
}

read_pkg_sql <- function(f) {
  p <- system.file("sql", f, package = "SteroidDoseR")
  if (!nchar(p)) p <- file.path("inst", "sql", f)
  SqlRender::readSql(p)
}

if (!USE_SYNTHETIC) {
  query_omop <- function(sql, ...) {
    if (inherits(conn, "DatabaseConnectorConnection"))
      DatabaseConnector::renderTranslateQuerySql(conn, sql, ...,
                                                 snakeCaseToCamelCase = FALSE)
    else
      DBI::dbGetQuery(conn, SqlRender::translate(
        SqlRender::render(sql, ...), targetDialect = DB_DIALECT))
  }
}

if (USE_SYNTHETIC) {
  message("Using bundled synthetic data")
  drug_df <- readr::read_csv(
    system.file("extdata", "synthetic_drug_exposure.csv",
                package = "SteroidDoseR"),
    show_col_types = FALSE)
} else {
  message("Extracting from live OMOP CDM")
  sql <- read_pkg_sql("extract_drug_exposure.sql")
  drug_df <- query_omop(
    sql,
    cdm_schema     = cdm_schema,
    vocab_schema   = vocab_schema,
    start_date     = START_DATE,
    end_date       = END_DATE,
    concept_filter = paste(STEROID_CONCEPT_IDS, collapse = ","),
    person_filter  = if (!is.null(COHORT_PERSON_IDS))
                       paste(COHORT_PERSON_IDS, collapse = ",") else ""
  )
  names(drug_df) <- tolower(names(drug_df))
  drug_df$drug_exposure_start_date <- as.Date(drug_df$drug_exposure_start_date)
  drug_df$drug_exposure_end_date   <- as.Date(drug_df$drug_exposure_end_date)
}

drug_df <- drug_df |>
  dplyr::mutate(drug_name_std = standardize_drug_name(drug_concept_name))
if (!"drug_exposure_id" %in% names(drug_df))
  drug_df <- drug_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())

hier_df <- calc_daily_dose_hierarchical(
  drug_df, diff_threshold = DIFF_THRESHOLD, match_tol = MATCH_TOL,
  max_daily_dose_mg = 2000, filter_oral = TRUE)

hier_eq <- convert_pred_equiv(hier_df,
                              drug_col = "drug_name_std",
                              dose_col = "daily_dose_mg")
hier_episodes <- build_episodes(hier_eq,
                                end_col        = "drug_exposure_end_date",
                                dose_col       = "pred_equiv_mg",
                                gap_days       = GAP_DAYS,
                                concurrent_agg = CONCURRENT_AGG)

gold_std <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)
gold_std$episode_start <- as.Date(gold_std$episode_start)
gold_std$episode_end   <- as.Date(gold_std$episode_end)

# Gold pred-equiv conversion
gold_drug_map <- hier_df |>
  dplyr::select(person_id, drug_name_std,
                drug_exposure_start_date, drug_exposure_end_date) |>
  dplyr::rename(patient_id = person_id) |>
  dplyr::inner_join(
    gold_std |> dplyr::select(patient_id, episode_start, episode_end),
    by = "patient_id", relationship = "many-to-many") |>
  dplyr::filter(drug_exposure_start_date <= episode_end,
                drug_exposure_end_date   >= episode_start,
                !is.na(drug_name_std)) |>
  dplyr::group_by(patient_id, episode_start, episode_end, drug_name_std) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(patient_id, episode_start, episode_end) |>
  dplyr::slice_max(n, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(patient_id, episode_start, episode_end, drug_name_std)

gold_std <- gold_std |>
  dplyr::left_join(gold_drug_map,
                   by = c("patient_id", "episode_start", "episode_end")) |>
  convert_pred_equiv(drug_col = "drug_name_std", dose_col = "median_daily_dose",
                     out_col  = "gold_pred_equiv_mg") |>
  dplyr::mutate(median_daily_dose =
                  dplyr::coalesce(gold_pred_equiv_mg, median_daily_dose))

if (!USE_SYNTHETIC && exists("conn")) {
  if (inherits(conn, "DatabaseConnectorConnection"))
    DatabaseConnector::disconnect(conn) else DBI::dbDisconnect(conn)
}

message(sprintf(
  "Loaded: %d records | %d computed episodes | %d gold episodes | %d patients",
  nrow(hier_df), nrow(hier_episodes), nrow(gold_std),
  dplyr::n_distinct(hier_df$person_id)
))

# ===========================================================================
# 2. SIG type classification (adds context for reviewers)
# ===========================================================================
classify_sig_type <- function(sig_text, sig_status, sig_taper_flag,
                               bl_dose, sig_dose, conflict_thr) {
  taper_flag <- !is.na(sig_taper_flag) & sig_taper_flag
  both_avail <- !is.na(bl_dose) & !is.na(sig_dose)
  txt_lc     <- tolower(ifelse(is.na(sig_text), "", sig_text))
  dplyr::case_when(
    taper_flag | (!is.na(sig_status) & sig_status %in% c("taper", "taper_ok"))
      ~ "taper",
    both_avail & abs(bl_dose - sig_dose) > conflict_thr ~ "conflicting",
    (!is.na(sig_status) & sig_status == "prn") |
      grepl("\\bas needed\\b|\\bprn\\b", txt_lc) ~ "prn",
    grepl("\\bincrease|\\bescalate|\\btitrate|\\bup to|\\bup dose", txt_lc) &
      !grepl("\\btaper|\\bdecrease|\\breduce|\\bwean", txt_lc) ~ "escalation",
    is.na(sig_text) | sig_text == "" ~ "no_sig",
    !is.na(sig_status) & sig_status %in% c("no_parse", "free_text", "empty")
      ~ "ambiguous",
    !is.na(sig_status) & sig_status == "ok" ~ "steady",
    TRUE ~ "ambiguous"
  )
}

if (!"drug_exposure_id" %in% names(hier_df))
  hier_df <- hier_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())

hier_df <- hier_df |>
  dplyr::mutate(
    sig_type = classify_sig_type(sig, sig_status, sig_taper_flag,
                                  bl_dose, sig_dose, CONFLICT_THR_MG)
  )

# ===========================================================================
# 3. Evaluate against gold (to get agreement stats for gold rows)
# ===========================================================================
message("Evaluating against gold standard ...")
ev <- evaluate_against_gold(hier_episodes, gold_std, gold_id_col = "patient_id")
message(sprintf(
  "Coverage: %.1f%% | MAE: %.2f mg",
  ev$summary$coverage_pct, ev$summary$MAE
))

# ===========================================================================
# 4. Build manual_review data frame
# ===========================================================================
message("Building manual review table ...")

# Stable ID per gold episode
gold_indexed <- gold_std |>
  dplyr::mutate(pt_id_int = as.integer(patient_id),
                match_id  = dplyr::row_number())

# Best-matching gold episode for each computed record (max day-overlap)
record_match <- hier_df |>
  dplyr::mutate(pt_id_int = as.integer(person_id)) |>
  dplyr::left_join(
    gold_indexed |>
      dplyr::transmute(pt_id_int, match_id,
                       g_start = episode_start, g_end = episode_end),
    by = "pt_id_int", relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    overlap_d = as.integer(
      pmin(drug_exposure_end_date, g_end,    na.rm = FALSE) -
      pmax(drug_exposure_start_date, g_start, na.rm = FALSE)
    ) + 1L,
    overlap_d = dplyr::if_else(!is.na(overlap_d) & overlap_d > 0L,
                               overlap_d, 0L)
  ) |>
  dplyr::group_by(drug_exposure_id) |>
  dplyr::slice_max(overlap_d, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(match_id = dplyr::if_else(overlap_d > 0L, match_id, NA_integer_))

# Gold rows (one per gold episode)
gold_rows <- gold_indexed |>
  dplyr::left_join(
    ev$comparison |>
      dplyr::select(patient_id, episode_start, episode_end,
                    computed_dose, absolute_error, bias_error,
                    agreement_category, error_direction),
    by = c("patient_id", "episode_start", "episode_end")
  ) |>
  dplyr::transmute(
    source               = "gold",
    match_id             = match_id,
    patient_id           = pt_id_int,
    drug_name_std        = dplyr::coalesce(drug_name_std, NA_character_),
    start_date           = episode_start,
    end_date             = episode_end,
    duration_days        = as.integer(episode_end - episode_start) + 1L,
    daily_dose_mg        = median_daily_dose,
    computed_dose_for_ep = computed_dose,
    agreement_category   = agreement_category,
    absolute_error_mg    = round(absolute_error, 2),
    bias_error_mg        = round(bias_error, 2),
    error_direction      = error_direction,
    method               = NA_character_,
    sig_type             = NA_character_,
    sig_text             = NA_character_,
    sig_status           = NA_character_,
    bl_dose              = NA_real_,
    sig_dose_nlp         = NA_real_
  )

# Computed record rows (one per drug-exposure record)
computed_rows <- record_match |>
  dplyr::transmute(
    source               = "computed_record",
    match_id             = match_id,
    patient_id           = pt_id_int,
    drug_name_std        = drug_name_std,
    start_date           = drug_exposure_start_date,
    end_date             = drug_exposure_end_date,
    duration_days        = as.integer(drug_exposure_end_date -
                                        drug_exposure_start_date) + 1L,
    daily_dose_mg        = daily_dose_mg,
    computed_dose_for_ep = NA_real_,
    agreement_category   = NA_character_,
    absolute_error_mg    = NA_real_,
    bias_error_mg        = NA_real_,
    error_direction      = NA_character_,
    method               = hierarchical_method,
    sig_type             = sig_type,
    sig_text             = sig,
    sig_status           = sig_status,
    bl_dose              = round(bl_dose, 2),
    sig_dose_nlp         = round(sig_dose, 2)
  )

# Bind: within each patient, gold episode first, then its records
manual_review <- dplyr::bind_rows(gold_rows, computed_rows) |>
  dplyr::arrange(patient_id, match_id, dplyr::desc(source == "gold"), start_date)

message(sprintf(
  "manual_review: %d rows  (%d gold episodes | %d computed records)",
  nrow(manual_review),
  sum(manual_review$source == "gold"),
  sum(manual_review$source == "computed_record")
))

# ===========================================================================
# 5. Save CSV
# ===========================================================================
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(OUTPUT_DIR, sprintf("manual_review_%s.csv",
                                          format(Sys.time(), "%Y-%m-%d_%H-%M-%S")))
readr::write_csv(manual_review, csv_path)
message(sprintf("CSV saved: %s", csv_path))

# ===========================================================================
# 6. Shiny dashboard
# ===========================================================================
if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("DT",    quietly = TRUE)) {
  message("Install shiny + DT to launch the dashboard:\n",
          "  install.packages(c('shiny', 'DT'))")
  message("\n=== Done ===")
  invisible(NULL)
} else {

  agree_pal <- c(
    "Exact (<=5%)"     = "#2166AC",
    "Good (<=20%)"     = "#92C5DE",
    "Moderate (<=50%)" = "#F4A582",
    "Poor (>50%)"      = "#D6604D",
    "Unmatched"        = "#BBBBBB"
  )

  sig_pal <- c(
    steady      = "#2271B3",
    taper       = "#E69F00",
    escalation  = "#CC79A7",
    prn         = "#009E73",
    ambiguous   = "#999999",
    conflicting = "#D55E00",
    no_sig      = "#CCCCCC"
  )

  patient_ids <- sort(unique(manual_review$patient_id))

  # ── UI ─────────────────────────────────────────────────────────────────────
  ui <- shiny::fluidPage(

    shiny::tags$head(shiny::tags$style(shiny::HTML("
      body { font-family: 'Helvetica Neue', sans-serif; font-size: 13px; }
      .summary-box { background:#f8f9fa; border-radius:6px;
                     padding:10px 14px; margin-bottom:8px; }
      .summary-box b { display:inline-block; width:155px; }
    "))),

    shiny::titlePanel(
      shiny::div("Manual Review — Steroid Dose", style = "font-size:20px;")
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectizeInput(
          "patient_id", "Patient ID",
          choices  = patient_ids,
          selected = patient_ids[[1L]],
          options  = list(placeholder = "Search…")
        ),
        shiny::checkboxInput("show_unmatched",
                             "Include unmatched computed records", value = TRUE),
        shiny::hr(),
        shiny::uiOutput("patient_summary_ui"),
        shiny::hr(),
        shiny::p(
          shiny::tags$small(
            shiny::tags$b("Rows: "), nrow(manual_review),
            " | Patients: ", length(patient_ids),
            shiny::tags$br(),
            "CSV: ", shiny::tags$code(basename(csv_path))
          )
        )
      ),

      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          # ── Tab 1: Timeline ────────────────────────────────────────────────
          shiny::tabPanel(
            shiny::icon("timeline"), " Timeline",
            shiny::br(),
            shiny::plotOutput("timeline_plot", height = "380px")
          ),
          # ── Tab 2: Table ───────────────────────────────────────────────────
          shiny::tabPanel(
            shiny::icon("table"), " Table",
            shiny::br(),
            DT::DTOutput("review_table")
          )
        )
      )
    )
  )

  # ── Server ─────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    pt_data <- shiny::reactive({
      df <- manual_review |>
        dplyr::filter(patient_id == as.integer(input$patient_id))
      if (!input$show_unmatched)
        df <- df |> dplyr::filter(!is.na(match_id) | source == "gold")
      df
    })

    # Patient summary cards
    output$patient_summary_ui <- shiny::renderUI({
      df   <- pt_data()
      gold <- df |> dplyr::filter(source == "gold")
      comp <- df |> dplyr::filter(source == "computed_record")

      n_matched <- sum(!is.na(gold$computed_dose_for_ep))
      n_poor    <- sum(gold$agreement_category == "Poor (>50%)", na.rm = TRUE)
      mae_val   <- if (n_matched > 0L)
        sprintf("%.1f mg", mean(gold$absolute_error_mg, na.rm = TRUE)) else "—"

      shiny::div(
        class = "summary-box",
        shiny::HTML(paste0(
          "<b>Gold episodes:</b> ", nrow(gold), "<br>",
          "<b>Computed records:</b> ", nrow(comp), "<br>",
          "<b>Matched:</b> ", n_matched, "<br>",
          "<b>Poor agreement:</b> ", n_poor, "<br>",
          "<b>MAE:</b> ", mae_val
        ))
      )
    })

    # Timeline
    output$timeline_plot <- shiny::renderPlot({
      df   <- pt_data()
      gold <- df |>
        dplyr::filter(source == "gold") |>
        dplyr::mutate(
          agr = factor(dplyr::coalesce(agreement_category, "Unmatched"),
                       levels = names(agree_pal)),
          y   = 0.72
        )
      comp <- df |>
        dplyr::filter(source == "computed_record") |>
        dplyr::mutate(
          stype = dplyr::coalesce(sig_type, "ambiguous"),
          y     = 0.28
        )

      if (nrow(gold) + nrow(comp) == 0L) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "No data for this patient",
                              size = 5, colour = "grey50") +
            ggplot2::theme_void()
        )
      }

      p <- ggplot2::ggplot() +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::scale_y_continuous(
          breaks = c(0.28, 0.72),
          labels = c("Computed records", "Gold episodes"),
          limits = c(0, 1)
        ) +
        ggplot2::labs(
          title = sprintf("Patient %s", input$patient_id),
          x = NULL, y = NULL
        ) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor   = ggplot2::element_blank()
        )

      if (nrow(gold) > 0L) {
        p <- p +
          ggplot2::geom_rect(
            data = gold,
            ggplot2::aes(xmin = start_date, xmax = end_date,
                         ymin = y - 0.15, ymax = y + 0.15,
                         fill = agr),
            colour = "white", linewidth = 0.5, alpha = 0.92
          ) +
          ggplot2::geom_text(
            data = gold,
            ggplot2::aes(
              x     = start_date + (end_date - start_date) / 2,
              y     = y,
              label = sprintf("Gold: %.0f mg\n%s",
                              daily_dose_mg,
                              dplyr::coalesce(agreement_category, "unmatched"))
            ),
            size = 3, colour = "white", fontface = "bold", lineheight = 1.1
          ) +
          ggplot2::scale_fill_manual(values = agree_pal,
                                     name = "Agreement", drop = FALSE)
      }

      if (nrow(comp) > 0L) {
        p <- p +
          ggplot2::geom_rect(
            data = comp,
            ggplot2::aes(xmin = start_date, xmax = end_date,
                         ymin = y - 0.10, ymax = y + 0.10,
                         fill = stype),
            colour = "white", linewidth = 0.3, alpha = 0.85,
            show.legend = FALSE
          ) +
          ggplot2::geom_text(
            data = comp,
            ggplot2::aes(
              x     = start_date + (end_date - start_date) / 2,
              y     = y,
              label = sprintf("%.0f mg\n[%s]",
                              dplyr::coalesce(daily_dose_mg, NA_real_),
                              dplyr::coalesce(method, "?"))
            ),
            size = 2.4, colour = "white", lineheight = 1.1
          ) +
          ggplot2::scale_fill_manual(values = sig_pal,
                                     name = "SIG type", drop = FALSE)
      }
      p
    })

    # DT table
    output$review_table <- DT::renderDT({
      df <- pt_data() |>
        dplyr::select(
          source, match_id, drug_name_std,
          start_date, end_date, duration_days,
          daily_dose_mg, computed_dose_for_ep,
          agreement_category, absolute_error_mg, bias_error_mg, error_direction,
          method, sig_type, sig_text, sig_status,
          bl_dose, sig_dose_nlp
        ) |>
        dplyr::mutate(
          dplyr::across(where(is.numeric), ~ round(., 2)),
          start_date = as.character(start_date),
          end_date   = as.character(end_date)
        )

      DT::datatable(
        df,
        options = list(
          pageLength = 50,
          scrollX    = TRUE,
          dom        = "tip",
          columnDefs = list(
            list(width = "120px", targets = which(names(df) == "sig_text") - 1L)
          )
        ),
        rownames = FALSE,
        filter   = "none"
      ) |>
        DT::formatStyle(
          "source",
          target          = "row",
          backgroundColor = DT::styleEqual(
            c("gold",    "computed_record"),
            c("#FFFDE7", "#E3F2FD")
          ),
          fontWeight = DT::styleEqual("gold", "bold")
        ) |>
        DT::formatStyle(
          "agreement_category",
          backgroundColor = DT::styleEqual(
            c("Exact (<=5%)", "Good (<=20%)", "Moderate (<=50%)", "Poor (>50%)"),
            c("#C8E6C9",      "#FFF9C4",      "#FFE0B2",          "#FFCDD2")
          )
        )
    })
  }

  message("Launching review dashboard ...")
  message("(Close the browser tab or press Escape to stop.)")
  shiny::runApp(shiny::shinyApp(ui, server))
}
