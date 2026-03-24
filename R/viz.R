# viz.R
# Patient-level episode visualization: static ggplot2 plot and
# interactive Shiny review dashboard.
#
# Requires ggplot2 (Suggests) for plot_patient_episodes().
# Requires shiny + DT (Suggests) for launch_dose_dashboard().

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Check that a suggested package is installed; abort with install hint.
#' @noRd
.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(sprintf(
      "Package '%s' is required.\nInstall it with: install.packages(\"%s\")",
      pkg, pkg
    ))
  }
}

# Default colour palette for methods (extendable by the user).
.METHOD_COLORS <- c(
  "Baseline"     = "#2271B3",  # blue
  "NLP"          = "#E69F00",  # amber
  "Advanced NLP" = "#009E73",  # green
  "Gold"         = "#333333"   # dark grey (dashed)
)

# Convert episode data frame to segment data for ggplot2.
# Returns one row per episode with x, xend, y.
#' @noRd
.episodes_to_segs <- function(episodes, dose_col, method_name) {
  if (!dose_col %in% names(episodes)) return(NULL)
  if (!"person_id" %in% names(episodes)) {
    # try common alternative
    if ("patient_id" %in% names(episodes))
      episodes <- dplyr::rename(episodes, person_id = "patient_id")
    else return(NULL)
  }

  episodes |>
    dplyr::filter(!is.na(.data[[dose_col]])) |>
    dplyr::transmute(
      person_id     = as.character(.data$person_id),
      drug_name_std = if ("drug_name_std" %in% names(episodes))
                        .data$drug_name_std else "unknown",
      method        = method_name,
      x             = .data$episode_start,
      xend          = .data$episode_end,
      y             = safe_as_numeric(.data[[dose_col]])
    )
}

# ---------------------------------------------------------------------------
# Exported: static ggplot2 timeline
# ---------------------------------------------------------------------------

#' Plot dose timelines for selected patients across methods
#'
#' Generates a `ggplot2` figure showing computed daily dose over time for
#' one or more patients, overlaying any combination of Baseline, NLP,
#' Advanced NLP, and gold-standard episodes as horizontal step segments.
#'
#' Each method's episodes appear as coloured horizontal lines spanning
#' `episode_start` to `episode_end`. Gaps between episodes are blank (no
#' dose prescribed). The gold standard is drawn in dark grey with a dashed
#' line style. Panels are faceted by patient x drug.
#'
#' @param episode_list A **named** list of episode data frames, one per
#'   method. Each data frame must be output from [build_episodes()] and
#'   contain columns `person_id`, `drug_name_std`, `episode_start`,
#'   `episode_end`, and the dose column specified in `dose_col`.
#'   Example:
#'   ```r
#'   list(Baseline = baseline_episodes,
#'        NLP      = nlp_episodes,
#'        "Advanced NLP" = adv_episodes)
#'   ```
#' @param patient_ids Character or integer vector of `person_id` values to
#'   include. At least one must be provided.
#' @param gold_std Optional gold-standard data frame with columns for patient
#'   ID (`gold_id_col`), `episode_start`, `episode_end`, and a dose column
#'   (`gold_dose_col`). Default: `NULL` (no gold standard plotted).
#' @param drug_filter Character vector of `drug_name_std` values to restrict
#'   the plot to. Default: `NULL` (all drugs).
#' @param dose_col `character(1)`. Dose column to use from each episode data
#'   frame. One of `"median_daily_dose"` (default), `"min_daily_dose"`,
#'   `"max_daily_dose"`, or `"mean_daily_dose"`.
#' @param gold_dose_col `character(1)`. Dose column in `gold_std`. Default:
#'   `"median_daily_dose"`.
#' @param gold_id_col `character(1)`. Patient-ID column in `gold_std`.
#'   Default: `"patient_id"`.
#' @param colors Named character vector mapping method names to hex colour
#'   codes. Defaults to the built-in palette.
#' @param linewidth `numeric(1)`. Thickness of episode segments. Default: 2.
#' @param title `character(1)`. Plot title. Default: `"Patient dose episodes"`.
#'
#' @return A `ggplot` object. Print or save with [ggplot2::ggsave()].
#'
#' @seealso [launch_dose_dashboard()], [build_episodes()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' p <- plot_patient_episodes(
#'   list(Baseline = baseline_episodes, NLP = nlp_episodes),
#'   patient_ids = c(1001L, 1002L),
#'   gold_std    = gold_standard
#' )
#' print(p)
#' ggplot2::ggsave("dose_review.pdf", p, width = 12, height = 8)
#' }
plot_patient_episodes <- function(episode_list,
                                  patient_ids,
                                  gold_std      = NULL,
                                  drug_filter   = NULL,
                                  dose_col      = "median_daily_dose",
                                  gold_dose_col = "median_daily_dose",
                                  gold_id_col   = "patient_id",
                                  colors        = NULL,
                                  linewidth     = 2,
                                  title         = "Patient dose episodes") {
  .require_pkg("ggplot2")

  if (!is.list(episode_list) || is.null(names(episode_list))) {
    rlang::abort("episode_list must be a named list (e.g. list(Baseline = df, NLP = df)).")
  }
  if (length(patient_ids) == 0L) rlang::abort("patient_ids must contain at least one ID.")

  patient_ids <- as.character(patient_ids)

  # --- build segment data for each method ------------------------------------
  segs_list <- lapply(names(episode_list), function(nm) {
    .episodes_to_segs(episode_list[[nm]], dose_col, nm)
  })
  segs <- dplyr::bind_rows(segs_list[!sapply(segs_list, is.null)])

  if (nrow(segs) == 0L) {
    rlang::warn("No episode data available for the requested patients / dose column.")
  }

  # Filter to requested patients and drugs
  segs <- segs[segs$person_id %in% patient_ids, ]
  if (!is.null(drug_filter)) segs <- segs[segs$drug_name_std %in% drug_filter, ]

  # --- gold standard ---------------------------------------------------------
  gold_segs <- NULL
  if (!is.null(gold_std)) {
    gs <- gold_std
    if (gold_id_col != "person_id") {
      if (gold_id_col %in% names(gs))
        gs <- dplyr::rename(gs, person_id = dplyr::all_of(gold_id_col))
      else
        rlang::warn(sprintf("gold_id_col '%s' not found in gold_std; skipping.", gold_id_col))
    }
    if ("person_id" %in% names(gs)) {
      gold_segs <- gs |>
        dplyr::filter(as.character(.data$person_id) %in% patient_ids) |>
        dplyr::transmute(
          person_id     = as.character(.data$person_id),
          drug_name_std = if ("drug_name_std" %in% names(gs))
                            .data$drug_name_std else "unknown",
          x    = .data$episode_start,
          xend = .data$episode_end,
          y    = safe_as_numeric(.data[[gold_dose_col]])
        ) |>
        dplyr::filter(!is.na(.data$y))
      if (!is.null(drug_filter))
        gold_segs <- gold_segs[gold_segs$drug_name_std %in% drug_filter, ]
    }
  }

  # --- colour palette --------------------------------------------------------
  pal <- .METHOD_COLORS
  if (!is.null(colors)) pal <- c(pal, colors)

  method_levels  <- names(episode_list)
  method_colours <- pal[method_levels]
  missing_col    <- method_levels[is.na(method_colours)]
  if (length(missing_col) > 0L) {
    extras <- grDevices::palette.colors(length(missing_col), palette = "Set2")
    method_colours[missing_col] <- extras
  }

  # --- facet label: person x drug --------------------------------------------
  segs$facet_label <- paste0("Patient ", segs$person_id,
                              "\n", segs$drug_name_std)
  if (!is.null(gold_segs) && nrow(gold_segs) > 0L)
    gold_segs$facet_label <- paste0("Patient ", gold_segs$person_id,
                                    "\n", gold_segs$drug_name_std)

  # --- build plot ------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = segs,
      ggplot2::aes(
        x = .data$x, xend = .data$xend,
        y = .data$y, yend = .data$y,
        colour = .data$method
      ),
      linewidth = linewidth
    ) +
    ggplot2::scale_colour_manual(
      name   = "Method",
      values = method_colours,
      breaks = method_levels
    ) +
    ggplot2::labs(
      title = title,
      x     = NULL,
      y     = paste0("Daily dose -- ", gsub("_", " ", dose_col), " (mg/day)")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add gold standard as dashed dark-grey layer
  if (!is.null(gold_segs) && nrow(gold_segs) > 0L) {
    p <- p +
      ggplot2::geom_segment(
        data = gold_segs,
        ggplot2::aes(x = .data$x, xend = .data$xend,
                     y = .data$y, yend = .data$y),
        colour    = pal[["Gold"]],
        linetype  = "dashed",
        linewidth = linewidth * 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
        label = "-- Gold standard", colour = pal[["Gold"]],
        size = 3.2, fontface = "italic"
      )
  }

  # Facet only when > 1 patient-drug combination exists
  n_panels <- length(unique(segs$facet_label))
  if (n_panels > 1L) {
    p <- p + ggplot2::facet_wrap(~ facet_label, scales = "free_x", ncol = 1L)
  } else if (n_panels == 1L) {
    patient_label <- unique(segs$facet_label)[[1L]]
    p <- p + ggplot2::labs(subtitle = patient_label)
  }

  p
}

# ---------------------------------------------------------------------------
# Exported: interactive Shiny dashboard
# ---------------------------------------------------------------------------

#' Launch an interactive dose review dashboard
#'
#' Opens a Shiny application in the browser for manual review of patient-level
#' dose episodes. The dashboard overlays computed episodes from one or more
#' methods and an optional gold standard, with controls for:
#' - Selecting patients by ID (multi-select or free-text entry)
#' - Filtering by drug name
#' - Choosing the dose aggregation metric (median / min / max / mean)
#' - Toggling individual methods on/off
#' - Viewing the underlying episode table
#'
#' The main panel is organised into three tabs:
#' \describe{
#'   \item{Timeline}{Dose trajectory plot overlaying all selected methods and
#'     the gold standard in distinct colours.}
#'   \item{Episodes}{Episode-level summary table (one row per computed
#'     episode), filterable and downloadable as CSV.}
#'   \item{Raw Records}{Record-level diagnostic table showing the original
#'     prescription rows with SIG strings, imputation method chosen, and
#'     calculated doses — colour-coded by method. Only available when
#'     \code{raw_list} is supplied.}
#' }
#'
#' @param episode_list A **named** list of episode data frames -- one per method
#'   (see [plot_patient_episodes()] for format). Must include at least one
#'   entry.
#' @param raw_list An optional **named** list of record-level data frames, one
#'   per method -- the direct output of [calc_daily_dose_baseline()],
#'   [calc_daily_dose_nlp()], or [calc_daily_dose_nlp_advanced()] (optionally
#'   passed through [convert_pred_equiv()] first). Names should match those in
#'   `episode_list`. When supplied, the **Raw Records** tab is populated with
#'   key diagnostic columns (`sig`, `imputation_method`,
#'   `daily_dose_mg_imputed`, `pred_equiv_mg`, etc.) and rows are
#'   colour-coded by method to match the Timeline plot palette. Default:
#'   `NULL` (tab shows a placeholder message).
#' @param gold_std Optional gold-standard data frame. Default: `NULL`.
#' @param dose_col `character(1)`. Default dose metric on startup. One of
#'   `"median_daily_dose"` (default), `"min_daily_dose"`, `"max_daily_dose"`,
#'   `"mean_daily_dose"`.
#' @param gold_dose_col `character(1)`. Dose column in `gold_std`.
#'   Default: `"median_daily_dose"`.
#' @param gold_id_col `character(1)`. Patient-ID column in `gold_std`.
#'   Default: `"patient_id"`.
#' @param plot_height `integer(1)`. Height of the episode plot in pixels.
#'   Default: `500L`.
#'
#' @return Launches a Shiny app (does not return a value). Press **Escape** or
#'   close the browser tab to stop.
#'
#' @seealso [plot_patient_episodes()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' launch_dose_dashboard(
#'   episode_list = list(Baseline = baseline_episodes,
#'                       NLP      = nlp_episodes,
#'                       "Advanced NLP" = adv_episodes),
#'   raw_list     = list(Baseline = baseline_df,
#'                       NLP      = nlp_df,
#'                       "Advanced NLP" = adv_df),
#'   gold_std     = gold_standard
#' )
#'
#' # Backward-compatible call (no raw records tab)
#' launch_dose_dashboard(
#'   list(Baseline = baseline_episodes, NLP = nlp_episodes),
#'   gold_std = gold_standard
#' )
#' }
launch_dose_dashboard <- function(episode_list,
                                  raw_list      = NULL,
                                  gold_std      = NULL,
                                  dose_col      = "median_daily_dose",
                                  gold_dose_col = "median_daily_dose",
                                  gold_id_col   = "patient_id",
                                  plot_height   = 500L) {
  .require_pkg("shiny")
  .require_pkg("ggplot2")

  if (!is.list(episode_list) || is.null(names(episode_list))) {
    rlang::abort("episode_list must be a named list.")
  }

  # Validate raw_list -- warn and discard if malformed, never abort
  if (!is.null(raw_list)) {
    if (!is.list(raw_list) || is.null(names(raw_list))) {
      rlang::warn("raw_list must be a named list (e.g. list(Baseline = df, NLP = df)); ignoring.")
      raw_list <- NULL
    }
  }
  has_raw <- !is.null(raw_list)

  # Collect all patient IDs and drug names across all methods
  all_ids <- sort(unique(as.character(unlist(lapply(
    episode_list, function(df) if ("person_id" %in% names(df)) df$person_id else character(0)
  )))))
  all_drugs <- sort(unique(unlist(lapply(
    episode_list, function(df) if ("drug_name_std" %in% names(df)) df$drug_name_std else character(0)
  ))))

  dose_choices <- c(
    "Median daily dose"             = "median_daily_dose",
    "Min daily dose"                = "min_daily_dose",
    "Max daily dose"                = "max_daily_dose",
    "Mean daily dose (duration-wt)" = "mean_daily_dose"
  )

  # Diagnostic columns shown in the Raw Records tab (subset to those present)
  RAW_DISPLAY_COLS <- c(
    "method", "person_id", "drug_concept_name", "drug_source_value", "sig",
    "drug_exposure_start_date", "drug_exposure_end_date",
    "amount_value", "amount_unit_concept_id", "quantity", "days_supply",
    "daily_dose", "imputation_method", "daily_dose_mg_imputed", "pred_equiv_mg"
  )

  # --- UI --------------------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      "body { font-family: -apple-system, sans-serif; }
       .sidebar { background: #f4f6f9; padding: 12px; border-radius: 6px; }
       h4 { color: #0f3460; margin-top: 14px; }"
    ))),

    shiny::titlePanel(
      shiny::div(
        shiny::strong("SteroidDoseR"), " -- Patient Dose Review Dashboard",
        style = "color:#0f3460;"
      )
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        class = "sidebar", width = 3,

        shiny::h4("Patients"),
        shiny::selectizeInput(
          "patient_ids", "Select patient IDs",
          choices  = all_ids,
          selected = utils::head(all_ids, 3L),
          multiple = TRUE,
          options  = list(placeholder = "Type or choose IDs...")
        ),
        shiny::actionButton("select_all",   "All",   class = "btn-sm"),
        shiny::actionButton("select_clear", "Clear", class = "btn-sm"),

        shiny::h4("Drug"),
        shiny::selectInput(
          "drug_filter", NULL,
          choices  = c("All drugs" = "__ALL__", all_drugs),
          selected = "__ALL__"
        ),

        shiny::h4("Dose metric"),
        shiny::selectInput(
          "dose_col", NULL,
          choices  = dose_choices,
          selected = dose_col
        ),

        shiny::h4("Methods"),
        shiny::checkboxGroupInput(
          "methods", NULL,
          choices  = names(episode_list),
          selected = names(episode_list)
        ),
        if (!is.null(gold_std))
          shiny::checkboxInput("show_gold", "Show gold standard", value = TRUE),

        shiny::hr(),
        shiny::h4("Plot"),
        shiny::sliderInput("lw", "Line width", min = 0.5, max = 4, value = 2, step = 0.5),
        shiny::downloadButton("dl_plot",  "Download plot (.pdf)"),
        shiny::br(), shiny::br(),
        shiny::downloadButton("dl_table", "Download episodes (.csv)"),
        if (has_raw) {
          shiny::tagList(shiny::br(), shiny::br(),
                         shiny::downloadButton("dl_raw", "Download raw records (.csv)"))
        }
      ),

      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          id = "main_tabs",

          shiny::tabPanel(
            "Timeline",
            shiny::br(),
            shiny::plotOutput("episode_plot",
                              height = paste0(plot_height, "px"))
          ),

          shiny::tabPanel(
            "Episodes",
            shiny::br(),
            shiny::h4("Episode table (selected patients)"),
            DT::dataTableOutput("episode_table")
          ),

          shiny::tabPanel(
            "Raw Records",
            shiny::br(),
            if (!has_raw) {
              shiny::div(
                style = "margin-top:1rem; padding:12px; background:#fff8e1; border-left:4px solid #E69F00; border-radius:4px;",
                shiny::strong("No raw record data provided."),
                shiny::p(
                  "Pass ", shiny::code("raw_list"), " to ",
                  shiny::code("launch_dose_dashboard()"),
                  " to see record-level details (SIG strings, imputation method, calculated doses)."
                )
              )
            } else {
              DT::dataTableOutput("raw_table")
            }
          )
        )
      )
    )
  )

  # --- Server ----------------------------------------------------------------
  server <- function(input, output, session) {

    # Select-all / clear buttons
    shiny::observeEvent(input$select_all,   {
      shiny::updateSelectizeInput(session, "patient_ids", selected = all_ids)
    })
    shiny::observeEvent(input$select_clear, {
      shiny::updateSelectizeInput(session, "patient_ids", selected = character(0))
    })

    # Reactive: filtered episode list
    filtered_episodes <- shiny::reactive({
      req_methods <- input$methods
      if (length(req_methods) == 0L) return(list())
      episode_list[req_methods]
    })

    # Reactive: drug filter
    drug_sel <- shiny::reactive({
      if (is.null(input$drug_filter) || input$drug_filter == "__ALL__") NULL
      else input$drug_filter
    })

    # Reactive: gold standard to pass
    gold_to_pass <- shiny::reactive({
      if (is.null(gold_std)) return(NULL)
      if (!is.null(input$show_gold) && !input$show_gold) return(NULL)
      gold_std
    })

    # Build the ggplot
    build_plot <- shiny::reactive({
      shiny::req(length(input$patient_ids) > 0L)
      ep <- filtered_episodes()
      if (length(ep) == 0L) return(ggplot2::ggplot() + ggplot2::theme_void())

      plot_patient_episodes(
        episode_list = ep,
        patient_ids  = input$patient_ids,
        gold_std     = gold_to_pass(),
        drug_filter  = drug_sel(),
        dose_col     = input$dose_col,
        gold_dose_col = gold_dose_col,
        gold_id_col   = gold_id_col,
        linewidth    = input$lw,
        title        = paste0(
          "Dose episodes -- patients: ",
          paste(utils::head(input$patient_ids, 5L), collapse = ", "),
          if (length(input$patient_ids) > 5L) "..." else ""
        )
      )
    })

    output$episode_plot <- shiny::renderPlot({ print(build_plot()) },
                                              res = 96)

    # Episode table (all selected methods, all selected patients, bound together)
    ep_table_data <- shiny::reactive({
      pid <- as.character(input$patient_ids)
      ep  <- filtered_episodes()
      dplyr::bind_rows(lapply(names(ep), function(nm) {
        df <- ep[[nm]]
        if (!"person_id" %in% names(df)) return(NULL)
        df <- df[as.character(df$person_id) %in% pid, ]
        if (!is.null(drug_sel()) && "drug_name_std" %in% names(df))
          df <- df[df$drug_name_std %in% drug_sel(), ]
        if (nrow(df) == 0L) return(NULL)
        dplyr::mutate(df, method = nm, .before = 1L)
      }))
    })

    output$episode_table <- DT::renderDataTable(
      ep_table_data(),
      options = list(pageLength = 15L, scrollX = TRUE),
      rownames = FALSE
    )

    # Raw records table (record-level, only when has_raw)
    raw_table_data <- shiny::reactive({
      if (!has_raw) return(NULL)
      pid <- as.character(input$patient_ids)
      if (length(pid) == 0L) return(NULL)
      valid_methods <- intersect(input$methods, names(raw_list))
      dplyr::bind_rows(lapply(valid_methods, function(nm) {
        df <- raw_list[[nm]]
        if (!"person_id" %in% names(df)) {
          if ("patient_id" %in% names(df))
            df <- dplyr::rename(df, person_id = "patient_id")
          else
            return(NULL)
        }
        df <- df[as.character(df$person_id) %in% pid, ]
        if (!is.null(drug_sel()) && "drug_name_std" %in% names(df))
          df <- df[df$drug_name_std %in% drug_sel(), ]
        if (nrow(df) == 0L) return(NULL)
        dplyr::mutate(df, method = nm, .before = 1L)
      }))
    })

    if (has_raw) {
      output$raw_table <- DT::renderDataTable({
        dat <- raw_table_data()
        if (is.null(dat) || nrow(dat) == 0L) {
          return(DT::datatable(
            data.frame(message = "No raw records match the current selection."),
            options  = list(dom = "t"),
            rownames = FALSE
          ))
        }
        # Keep only diagnostic columns that exist in this data frame
        keep_cols <- intersect(RAW_DISPLAY_COLS, names(dat))
        dat <- dat[, keep_cols, drop = FALSE]

        dt <- DT::datatable(
          dat,
          options  = list(pageLength = 20L, scrollX = TRUE),
          rownames = FALSE,
          filter   = "top"
        )
        # Colour rows by method using a light (~13% opacity) tint of each
        # method's plot colour ("22" hex suffix = 0x22/0xFF ≈ 13% opacity)
        DT::formatStyle(
          dt, "method",
          target          = "row",
          backgroundColor = DT::styleEqual(
            levels = c("Baseline", "NLP", "Advanced NLP"),
            values = c(
              paste0(.METHOD_COLORS[["Baseline"]],     "22"),
              paste0(.METHOD_COLORS[["NLP"]],          "22"),
              paste0(.METHOD_COLORS[["Advanced NLP"]], "22")
            )
          )
        )
      }, server = FALSE)
    }

    # Download handlers
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("dose_review_", Sys.Date(), ".pdf"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = build_plot(),
                        width = 12, height = 8, device = "pdf")
      }
    )
    output$dl_table <- shiny::downloadHandler(
      filename = function() paste0("dose_episodes_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(ep_table_data(), file, row.names = FALSE)
    )
    if (has_raw) {
      output$dl_raw <- shiny::downloadHandler(
        filename = function() paste0("raw_records_", Sys.Date(), ".csv"),
        content  = function(file) {
          dat <- raw_table_data()
          utils::write.csv(if (is.null(dat)) data.frame() else dat,
                           file, row.names = FALSE)
        }
      )
    }
  }

  shiny::shinyApp(ui, server)
}
