# CodeToRun.R
# SteroidDoseR — Main Study Analysis Script
#
# Primary execution script for the corticosteroid dose study.
# Computes prednisone-equivalent daily doses using three methods
# (Baseline, NLP, Advanced NLP) and evaluates against a gold standard.
#
# Analysis workflow
# -----------------
# STEP 1 — Extraction       : pull steroid drug_exposure from OMOP CDM.
#                              Optionally restrict to COHORT_PERSON_IDS.
# STEP 2 — Medication filter : restrict to steroid concept IDs (SQL) and oral
#                              route + known steroids (filter_oral = TRUE in R).
# STEP 3 — Dose calculation  : Baseline (M1-M4), NLP, Advanced NLP.
# STEP 4 — Evaluation        : compare to gold standard per overlapping window.
#
# Usage
# -----
#   Source this file interactively in RStudio, or run:
#     Rscript CodeToRun.R
#
# Supplementary analyses (run after this script, in the same R session):
#   source("extras/ErrorAnalysis.R")       # deep-dive into high-error episodes
#   source("extras/EligibilityAnalysis.R") # patient/episode funnel
#
# Connection modes  (set USE_SYNTHETIC below)
# --------------------------------------------
#   Mode A — Synthetic data (no database required):
#     USE_SYNTHETIC = TRUE
#
#   Mode B / C — Live OMOP CDM  (USE_SYNTHETIC = FALSE):
#     Fill in ONE connection block in Section 1a; Sections 1b–1c run unchanged.
#
#     Option B  DatabaseConnector (OHDSI standard) — SQL Server, PostgreSQL,
#               Redshift, Snowflake, BigQuery, Databricks/Spark, and more.
#               Required packages: DatabaseConnector, SqlRender
#
#     Option C  DBI / odbc — Databricks (Simba Spark ODBC driver) or any
#               other ODBC-compatible source.
#               Required packages: DBI, odbc, SqlRender
#               Driver: https://www.databricks.com/spark/odbc-drivers-download

devtools::install_local(getwd())
# This script is designed for interactive use in RStudio.
if (!interactive()) quit(status = 0L, save = "no")


library(SteroidDoseR)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
USE_SYNTHETIC  <- FALSE   # set TRUE to use bundled data; no DB required
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
GAP_DAYS       <- 30L


GOLD_STD_PATH  <- "/your/path/to/gold-standard"
GOLD_NEG_PATH  <- "/your/path/to/gold-negative"   # CSV with confirmed non-users; must have a patient ID column
OUTPUT_DIR     <- file.path(getwd(), "output")   # folder for saved CSVs and plots

# Optional patient filter — set to an integer vector of person_ids to restrict
# extraction to a specific cohort, or leave NULL to include all patients in DB.
COHORT_PERSON_IDS <- NULL

# Steroid drug_concept_id allow-list (matches Version2 Baseline extraction).
# Loaded from the bundled CSV; each row is one integer concept ID.
STEROID_CONCEPT_IDS <- as.integer(readr::read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

# ---------------------------------------------------------------------------
# 1a. Connection  —  fill in ONE option below (live DB only; skip for synthetic)
# ---------------------------------------------------------------------------
if (!USE_SYNTHETIC) {

  # ── Option B: DatabaseConnector (SQL Server, PostgreSQL, Redshift, Snowflake,
  #              BigQuery, Databricks/Spark, and more — OHDSI standard)
  # Required packages: DatabaseConnector, SqlRender
  # cdm_schema   <- "database.dbo"   # SQL Server format; "schema" for PostgreSQL
  # vocab_schema <- "database.dbo"
  # connectionDetails <- DatabaseConnector::createConnectionDetails(
  #   dbms             = "sql server",   # "postgresql", "redshift", "spark", ...
  #   connectionString = "",
  #   pathToDriver     = ""
  # )
  # conn <- DatabaseConnector::connect(connectionDetails)

  # ── Option C: DBI / odbc  (Databricks Simba Spark driver or any ODBC source)
  # Required packages: DBI, odbc, SqlRender
  # Driver download: https://www.databricks.com/spark/odbc-drivers-download
  # Databricks Unity Catalog requires three-part names (catalog.schema.table).
  # Set cdm_schema as "catalog.schema" so the rendered SQL becomes
  # catalog.schema.table — using only "schema" produces a two-part name that
  # Unity Catalog cannot resolve.
  # library(DBI); library(odbc)
  # DB_DIALECT   <- "spark"              # SqlRender targetDialect
  # cdm_schema   <- "catalog.schema"     # e.g. "hive_metastore.my_omop_cdm"
  # vocab_schema <- "catalog.schema"     # often the same as cdm_schema
  # host         <- ""                   # workspace URL hostname
  # http_path    <- ""                   # Settings > SQL Warehouse > Connection details
  # token        <- ""                   # Personal Access Token
  # conn <- DBI::dbConnect(
  #   odbc::odbc(),
  #   Driver          = "Simba Spark ODBC Driver",
  #   Host            = host,
  #   Port            = 443,
  #   HTTPPath        = http_path,
  #   AuthMech        = 3,
  #   UID             = "token",
  #   PWD             = token,
  #   SSL             = 1,
  #   ThriftTransport = 2
  # )

}

# Locate a bundled SQL file: checks the installed package first, then falls
# back to inst/sql/ in the working directory (useful during development before
# devtools::install_local() has been re-run).
read_pkg_sql <- function(filename) {
  path <- system.file("sql", filename, package = "SteroidDoseR")
  if (nchar(path) == 0L) path <- file.path("inst", "sql", filename)
  if (!file.exists(path)) stop("SQL file not found: ", filename)
  SqlRender::readSql(path)
}

# ---------------------------------------------------------------------------
# 1b. Query helper  —  works with both DatabaseConnector and DBI connections
# ---------------------------------------------------------------------------
# Renders SqlRender template parameters, translates to the target SQL dialect,
# and executes. Section 1c calls query_omop() unchanged for both options.
if (!USE_SYNTHETIC) {

  query_omop <- function(sql, ...) {
    if (inherits(conn, "DatabaseConnectorConnection")) {
      DatabaseConnector::renderTranslateQuerySql(
        conn, sql, ..., snakeCaseToCamelCase = FALSE
      )
    } else {
      DBI::dbGetQuery(
        conn,
        SqlRender::translate(SqlRender::render(sql, ...), targetDialect = DB_DIALECT)
      )
    }
  }

}

# ---------------------------------------------------------------------------
# 1c. Load data  —  Mode A: synthetic CSV  |  Mode B/C: live DB extraction
# ---------------------------------------------------------------------------
if (USE_SYNTHETIC) {
  message("=== Using bundled synthetic data ===")
  drug_df <- readr::read_csv(
    system.file("extdata", "synthetic_drug_exposure.csv", package = "SteroidDoseR"),
    show_col_types = FALSE
  )
} else {
  message("=== Extracting data from live OMOP CDM ===")

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

message(sprintf(
  "Fetched %d rows | %d unique persons | %s | concept filter: %d steroid concept IDs",
  nrow(drug_df),
  length(unique(drug_df$person_id)),
  if (is.null(COHORT_PERSON_IDS)) "cohort: all patients in DB"
  else sprintf("cohort: %d pre-specified person_ids", length(COHORT_PERSON_IDS)),
  length(STEROID_CONCEPT_IDS)
))

drug_df <- drug_df |>
  dplyr::mutate(drug_name_std = standardize_drug_name(drug_concept_name))

if (!"drug_exposure_id" %in% names(drug_df)) {
  drug_df <- drug_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())
}

# ===========================================================================
# Helper: print agreement summary as a compact single line
# ===========================================================================
print_agreement <- function(comparison_df, label) {
  lvls <- c("Exact (<=5%)", "Good (<=20%)", "Moderate (<=50%)", "Poor (>50%)")
  tbl  <- comparison_df |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::count(agreement_category) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 1))
  total <- sum(tbl$n)
  parts <- vapply(lvls, function(lv) {
    row <- tbl[tbl$agreement_category == lv, ]
    if (nrow(row) == 0L) return(sprintf("%s: 0%% (0)", lv))
    sprintf("%s: %.1f%% (%d)", lv, row$pct, row$n)
  }, character(1L))
  cat(sprintf(
    "\n%s agreement (n=%d):  %s\n",
    label, total, paste(parts, collapse = "  |  ")
  ))
}

# ===========================================================================
# Helper: print person-level episode trajectories
# ===========================================================================
show_person_trajectories <- function(episodes_df, method_name, n_patients = 3L) {
  cat(sprintf(
    "\n--- %s: %d episodes from %d persons (sample trajectories) ---\n",
    method_name,
    nrow(episodes_df),
    dplyr::n_distinct(episodes_df$person_id)
  ))
  cat("    doses shown in mg prednisone-equivalent\n")

  # Select patients with the most episodes (most informative trajectories)
  sample_pts <- episodes_df |>
    dplyr::count(person_id, sort = TRUE) |>
    dplyr::slice_head(n = n_patients) |>
    dplyr::pull(person_id)

  for (pt in sample_pts) {
    cat(sprintf("\n  Patient %s:\n", pt))
    traj <- episodes_df |>
      dplyr::filter(person_id == pt) |>
      dplyr::arrange(episode_start) |>
      dplyr::select(
        drug_name_std, episode_start, episode_end,
        n_days, n_records, median_daily_dose, mean_daily_dose
      )
    print(as.data.frame(traj), row.names = FALSE)
  }
}




# ===========================================================================
# 4. BASELINE METHOD
# ===========================================================================
message("\n=== [1/3] Baseline method ===")

baseline_df <- calc_daily_dose_baseline(
  drug_df,
  m2_sig_parse      = "auto",
  max_daily_dose_mg = 2000,
  filter_oral       = TRUE
)

cat("\nImputation method breakdown:\n")
print(table(baseline_df$imputation_method, useNA = "ifany"))

cat("\nIntermediate dose column non-NA counts:\n")
inter_cols <- c("dose_from_original", "dose_from_tablets_freq",
                "dose_from_supply",   "dose_from_actual_duration",
                "daily_dose_mg_imputed")
print(sapply(inter_cols, function(col) sum(!is.na(baseline_df[[col]]))))

cat("\nSample rows (intermediate + final columns):\n")
baseline_df |>
  dplyr::filter(!is.na(daily_dose_mg_imputed)) |>
  dplyr::select(person_id, drug_name_std, imputation_method,
                dose_from_original, dose_from_tablets_freq,
                dose_from_supply, dose_from_actual_duration,
                daily_dose_mg_imputed) |>
  head(10) |>
  print()

cat("\nDose summary (non-missing):\n")
print(summary(baseline_df$daily_dose_mg_imputed[!is.na(baseline_df$daily_dose_mg_imputed)]))

# Person-level: run pipeline to get episodes, then show trajectories
baseline_episodes <- run_pipeline(
  drug_df,
  method       = "baseline",
  m2_sig_parse = "auto",
  return_level = "episode",
  gap_days     = GAP_DAYS
)

show_person_trajectories(baseline_episodes, "Baseline")

# ===========================================================================
# 5. NLP METHOD
# ===========================================================================
message("\n=== [2/3] NLP method ===")

nlp_df <- calc_daily_dose_nlp(drug_df)

cat("\nparsed_status breakdown:\n")
print(table(nlp_df$parsed_status, useNA = "ifany"))

cat("\nDose summary (parsed_status == 'ok'):\n")
print(summary(nlp_df$daily_dose_mg[nlp_df$parsed_status == "ok"]))

cat("\nTop 15 unparsed SIG strings:\n")
nlp_df |>
  dplyr::filter(parsed_status == "no_parse") |>
  dplyr::count(sig, sort = TRUE) |>
  head(15) |>
  print()

# Person-level
nlp_episodes <- run_pipeline(
  drug_df,
  method       = "nlp",
  return_level = "episode",
  gap_days     = GAP_DAYS
)

show_person_trajectories(nlp_episodes, "NLP")

# ===========================================================================
# 6. ADVANCED NLP METHOD
# ===========================================================================
message("\n=== [3/3] Advanced NLP method ===")

adv_nlp_df <- calc_daily_dose_nlp_advanced(
  drug_df,
  max_daily_dose_mg = 2000,
  expand_tapers     = FALSE,
  filter_oral       = TRUE
)

cat("\nparsed_status breakdown (Advanced NLP):\n")
print(table(adv_nlp_df$parsed_status, useNA = "ifany"))

cat("\nDose summary (parsed_status == 'ok' or 'taper_ok'):\n")
ok_mask <- adv_nlp_df$parsed_status %in% c("ok", "taper_ok")
print(summary(adv_nlp_df$daily_dose_mg[ok_mask]))

cat(sprintf(
  "\nGain over standard NLP: %d → %d records parsed (+%d)\n",
  sum(nlp_df$parsed_status == "ok", na.rm = TRUE),
  sum(ok_mask, na.rm = TRUE),
  sum(ok_mask, na.rm = TRUE) - sum(nlp_df$parsed_status == "ok", na.rm = TRUE)
))

adv_nlp_episodes <- build_episodes(
  adv_nlp_df,
  end_col  = "drug_exposure_end_date",
  dose_col = "daily_dose_mg",
  gap_days = GAP_DAYS
)

show_person_trajectories(adv_nlp_episodes, "Advanced NLP")

# ===========================================================================
# 7. DOSE DISTRIBUTIONS
# ===========================================================================
message("\n=== Dose distributions ===")

# Build a combined data frame for plotting (gold panel added after Section 8)
make_dist_df <- function(episodes_df, method_label) {
  episodes_df |>
    dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
    dplyr::select(person_id, drug_name_std, median_daily_dose) |>
    dplyr::mutate(method = method_label)
}

dist_df <- dplyr::bind_rows(
  make_dist_df(baseline_episodes,  "Baseline"),
  make_dist_df(nlp_episodes,       "NLP"),
  make_dist_df(adv_nlp_episodes,   "Advanced NLP")
)
# NOTE: Distribution plot printed after Section 8 once gold standard is loaded.

# Descriptive summary per method
cat("\nDose distribution summary by method (mg prednisone-equivalent):\n")
dist_df |>
  dplyr::mutate(method = factor(method, levels = c("Baseline", "NLP", "Advanced NLP"))) |>
  dplyr::group_by(method) |>
  dplyr::summarise(
    n_episodes = dplyr::n(),
    n_patients = dplyr::n_distinct(person_id),
    min    = min(median_daily_dose),
    q25    = stats::quantile(median_daily_dose, 0.25),
    median = stats::median(median_daily_dose),
    mean   = mean(median_daily_dose),
    q75    = stats::quantile(median_daily_dose, 0.75),
    max    = max(median_daily_dose),
    .groups = "drop"
  ) |>
  print()

# ===========================================================================
# 8. Load gold standard
# ===========================================================================
message("\n=== Loading gold standard ===")

gold_std_raw <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)
gold_std     <- parse_dmard_gold(gold_std_raw)

cat(sprintf(
  "Gold standard: %d records from %d patients\n",
  nrow(gold_std), dplyr::n_distinct(gold_std$person_id)
))

cat("\nParse status breakdown:\n")
print(table(gold_std$parse_status, useNA = "ifany"))

cat("\nGold standard preview:\n")
print(head(gold_std[, c("person_id", "drug_name_std", "episode_start",
                        "episode_end", "dose_raw", "dose_daily_mg_equiv",
                        "parse_status")]))

cat("\nDaily mg equivalent distribution (parseable records):\n")
print(summary(gold_std$dose_daily_mg_equiv[gold_std$parse_status == "ok"]))

# --- Distribution plot including gold standard (4 panels) -------------------
gold_dist_df <- gold_std |>
  dplyr::filter(!is.na(dose_daily_mg_equiv), dose_daily_mg_equiv > 0) |>
  dplyr::transmute(
    person_id         = as.integer(person_id),
    drug_name_std     = dplyr::coalesce(drug_name_std, "unknown"),
    median_daily_dose = dose_daily_mg_equiv,
    method            = "Gold"
  )

dist_method_colors <- c(
  "Baseline"     = "#2271B3",
  "NLP"          = "#E69F00",
  "Advanced NLP" = "#009E73",
  "Gold"         = "#333333"
)

dist_df_all <- dplyr::bind_rows(dist_df, gold_dist_df) |>
  dplyr::mutate(method = factor(method,
                                levels = c("Baseline", "NLP", "Advanced NLP", "Gold")))

p_dist <- ggplot2::ggplot(
  dist_df_all,
  ggplot2::aes(x = median_daily_dose, fill = method, colour = method)
) +
  ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
  ggplot2::scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320, 640),
    labels = scales::label_number()
  ) +
  ggplot2::scale_fill_manual(values   = dist_method_colors) +
  ggplot2::scale_colour_manual(values = dist_method_colors) +
  ggplot2::facet_wrap(~ method, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title    = "Distribution of median daily prednisone-equivalent dose by method",
    subtitle = "One data point per patient-drug episode; x-axis on log10 scale",
    x        = "Median daily dose (mg prednisone-equivalent)",
    y        = "Density"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

print(p_dist)

# ===========================================================================
# 9. Episode-level comparison (each method vs gold standard)
# ===========================================================================
message("\n=== Episode-level comparisons vs gold standard ===")

# gold_std must have parse_status == "ok" to have a usable dose for comparison
gold_std_ok <- gold_std |> dplyr::filter(parse_status == "ok")
cat(sprintf(
  "Gold records with parseable dose: %d / %d\n",
  nrow(gold_std_ok), nrow(gold_std)
))

.run_comparison <- function(episodes_df, label) {
  ev <- compare_dmard_episodes(episodes_df, gold_std_ok)
  ev$n_common_patients <- length(intersect(
    unique(episodes_df$person_id), unique(gold_std_ok$person_id)
  ))
  cat(sprintf(
    "\n%s: %d common patients | %d/%d gold records matched (%.1f%% coverage)\n",
    label,
    ev$n_common_patients,
    ev$summary$n_matched_records,
    ev$summary$n_gold_records,
    ev$summary$coverage_pct
  ))
  cat(sprintf("  MAE=%.2f  MBE=%.2f  RMSE=%.2f  MAPE=%.1f%%  r=%.3f\n",
    ev$summary$MAE, ev$summary$MBE, ev$summary$RMSE,
    ev$summary$MAPE, ev$summary$pearson_corr))
  cat("By drug:\n")
  print(as.data.frame(ev$stratified$by_drug))
  ev
}

# --- 9a. Baseline ---
message("\n  Baseline vs gold standard ...")
ev_baseline <- .run_comparison(baseline_episodes, "Baseline")

# --- 9b. NLP ---
message("\n  NLP vs gold standard ...")
ev_nlp <- .run_comparison(nlp_episodes, "NLP")

# --- 9c. Advanced NLP ---
message("\n  Advanced NLP vs gold standard ...")
ev_adv <- .run_comparison(adv_nlp_episodes, "Advanced NLP")

# ===========================================================================
# 9b. Binary detection evaluation (kappa, sensitivity, specificity)
#     Requires GOLD_NEG_PATH — a CSV whose rows are confirmed non-steroid users.
#     Set GOLD_NEG_ID_COL to whichever column holds the patient ID.
# ===========================================================================
GOLD_NEG_ID_COL <- "person_id"   # adjust to match your CSV column name

if (!is.null(GOLD_NEG_PATH) && file.exists(GOLD_NEG_PATH)) {
  message("\n=== Binary detection evaluation (gold positive vs gold negative) ===")

  gold_neg <- readr::read_csv(GOLD_NEG_PATH, show_col_types = FALSE)
  cat(sprintf("Gold negative: %d confirmed non-users\n", nrow(gold_neg)))

  .run_detection <- function(episodes_df, label) {
    det <- evaluate_detection(
      computed_df      = episodes_df,
      gold_positive_df = gold_std_ok,
      gold_negative_df = gold_neg,
      detection_threshold = 0,
      obs_window_source   = "computed",
      computed_id_col     = "person_id",
      gold_pos_id_col     = "person_id",
      gold_neg_id_col     = GOLD_NEG_ID_COL
    )
    m <- det$metrics
    cat(sprintf(
      "\n%s:\n  TP=%d  FN=%d  FP=%d  TN=%d\n  Sensitivity=%.3f  Specificity=%.3f  PPV=%.3f  NPV=%.3f\n  Accuracy=%.3f  F1=%.3f  Kappa=%.3f\n",
      label, m$TP, m$FN, m$FP, m$TN,
      m$sensitivity, m$specificity, m$PPV, m$NPV,
      m$accuracy, m$F1, m$kappa
    ))
    det
  }

  det_baseline <- .run_detection(baseline_episodes, "Baseline")
  det_nlp      <- .run_detection(nlp_episodes,      "NLP")
  det_adv      <- .run_detection(adv_nlp_episodes,  "Advanced NLP")
} else {
  message("\nSkipping binary detection evaluation — GOLD_NEG_PATH not set or file not found.")
  det_baseline <- det_nlp <- det_adv <- NULL
}

# ===========================================================================
# 10. Comparison scatter plots (method dose vs gold dose)
# ===========================================================================
message("\n=== Comparison scatter plots ===")

make_scatter_df <- function(ev_result, method_label) {
  ev_result$comparison |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::transmute(
      person_id,
      drug_name_std,
      gold_dose,
      method_dose = computed_dose,
      method      = method_label
    )
}

scatter_df <- dplyr::bind_rows(
  make_scatter_df(ev_baseline, "Baseline"),
  make_scatter_df(ev_nlp,      "NLP"),
  make_scatter_df(ev_adv,      "Advanced NLP")
) |>
  dplyr::mutate(method = factor(method, levels = c("Baseline", "NLP", "Advanced NLP")))

p_scatter <- ggplot2::ggplot(
  scatter_df,
  ggplot2::aes(x = gold_dose, y = method_dose)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(alpha = 0.5, size = 1.8, colour = "#2166ac") +
  ggplot2::geom_smooth(method = "lm", se = TRUE,
                       colour = "#d6604d", linewidth = 0.8) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Method dose vs gold standard (overlapping time window)",
    subtitle = "Dashed line = perfect agreement; blue points = matched episodes",
    x        = "Gold standard median daily dose (mg pred-equiv)",
    y        = "Method median daily dose (mg pred-equiv)"
  ) +
  ggplot2::theme_bw()

print(p_scatter)

# ===========================================================================
# 10.5 Bland-Altman plots (method dose vs gold standard)
# ===========================================================================
message("\n=== Bland-Altman plots ===")

ba_df <- scatter_df |>
  dplyr::filter(!is.na(gold_dose), !is.na(method_dose)) |>
  dplyr::mutate(
    mean_dose = (method_dose + gold_dose) / 2,
    diff      = method_dose - gold_dose
  )

# Per-method bias and 95% limits of agreement
ba_limits <- ba_df |>
  dplyr::group_by(method) |>
  dplyr::summarise(
    n       = dplyr::n(),
    bias    = mean(diff),
    sd_diff = stats::sd(diff),
    loa_lo  = bias - 1.96 * sd_diff,
    loa_hi  = bias + 1.96 * sd_diff,
    .groups = "drop"
  )

cat("\nBland-Altman limits of agreement by method:\n")
print(as.data.frame(ba_limits |>
                      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))))

p_ba <- ggplot2::ggplot(ba_df, ggplot2::aes(x = mean_dose, y = diff)) +
  ggplot2::geom_hline(yintercept = 0,
                      linetype = "solid", colour = "grey70", linewidth = 0.5) +
  ggplot2::geom_hline(
    data     = ba_limits,
    ggplot2::aes(yintercept = bias),
    linetype = "dashed", colour = "#d6604d", linewidth = 0.8
  ) +
  ggplot2::geom_hline(
    data     = ba_limits,
    ggplot2::aes(yintercept = loa_lo),
    linetype = "dotted", colour = "#4393c3", linewidth = 0.7
  ) +
  ggplot2::geom_hline(
    data     = ba_limits,
    ggplot2::aes(yintercept = loa_hi),
    linetype = "dotted", colour = "#4393c3", linewidth = 0.7
  ) +
  ggplot2::geom_point(alpha = 0.45, size = 1.8, colour = "#2166ac") +
  ggplot2::geom_text(
    data = ba_limits,
    ggplot2::aes(
      x     = Inf,
      y     = bias,
      label = sprintf("Bias: %.1f mg", bias)
    ),
    hjust = 1.1, vjust = -0.5, colour = "#d6604d", size = 3.2
  ) +
  ggplot2::geom_text(
    data = ba_limits,
    ggplot2::aes(
      x     = Inf,
      y     = loa_hi,
      label = sprintf("+1.96 SD: %.1f mg", loa_hi)
    ),
    hjust = 1.1, vjust = -0.5, colour = "#4393c3", size = 3.2
  ) +
  ggplot2::geom_text(
    data = ba_limits,
    ggplot2::aes(
      x     = Inf,
      y     = loa_lo,
      label = sprintf("-1.96 SD: %.1f mg", loa_lo)
    ),
    hjust = 1.1, vjust = 1.5, colour = "#4393c3", size = 3.2
  ) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Bland-Altman: method dose minus gold standard",
    subtitle = paste0(
      "Red dashed = mean bias; blue dotted = 95% limits of agreement (\u00b11.96 SD);\n",
      "zero line = perfect agreement"
    ),
    x = "Mean of method and gold standard (mg pred-equiv)",
    y = "Method \u2212 Gold standard (mg pred-equiv)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))

print(p_ba)

# ===========================================================================
# 11. REPORT
# ===========================================================================
message("\n\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS REPORT — SteroidDoseR Method Comparison\n")
cat(strrep("=", 70), "\n\n")

cat("DATA OVERVIEW\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Drug-exposure records:  %d\n", nrow(drug_df)))
cat(sprintf("  Unique patients:        %d\n", dplyr::n_distinct(drug_df$person_id)))
cat(sprintf("  Study window:           %s to %s\n", START_DATE, END_DATE))
cat(sprintf("  Episode gap tolerance:  %d days\n\n", GAP_DAYS))

cat("PATIENT OVERLAP\n")
cat(strrep("-", 40), "\n")
db_patients_rpt      <- unique(as.integer(drug_df$person_id))
gold_patients_rpt    <- unique(as.integer(gold_std$person_id))
overlap_patients_rpt <- intersect(db_patients_rpt, gold_patients_rpt)
cat(sprintf("  Database (drug_exposure): %d unique patients\n",  length(db_patients_rpt)))
cat(sprintf("  Gold standard:            %d unique patients\n",  length(gold_patients_rpt)))
cat(sprintf("  Overlapping patients:     %d\n\n",               length(overlap_patients_rpt)))

cat("EPISODE COUNTS BY SOURCE\n")
cat(strrep("-", 40), "\n")
episode_counts <- tibble::tibble(
  Source            = c("Baseline", "NLP", "Advanced NLP", "Gold Standard"),
  Patients          = c(dplyr::n_distinct(baseline_episodes$person_id),
                        dplyr::n_distinct(nlp_episodes$person_id),
                        dplyr::n_distinct(adv_nlp_episodes$person_id),
                        dplyr::n_distinct(gold_std$person_id)),
  Records_pre_collapse = c(
    sum(baseline_episodes$n_records, na.rm = TRUE),
    sum(nlp_episodes$n_records,      na.rm = TRUE),
    sum(adv_nlp_episodes$n_records,  na.rm = TRUE),
    NA_integer_
  ),
  Episodes          = c(nrow(baseline_episodes),
                        nrow(nlp_episodes),
                        nrow(adv_nlp_episodes),
                        nrow(gold_std)),
  Gold_total        = c(rep(ev_baseline$summary$n_gold_records, 3L), NA_integer_),
  Matched_to_Gold   = c(ev_baseline$summary$n_matched_records,
                        ev_nlp$summary$n_matched_records,
                        ev_adv$summary$n_matched_records,
                        NA_integer_),
  Coverage_pct      = c(round(ev_baseline$summary$coverage_pct, 1),
                        round(ev_nlp$summary$coverage_pct,      1),
                        round(ev_adv$summary$coverage_pct,      1),
                        NA_real_),
  Median_mg         = c(stats::median(baseline_episodes$median_daily_dose, na.rm = TRUE),
                        stats::median(nlp_episodes$median_daily_dose,       na.rm = TRUE),
                        stats::median(adv_nlp_episodes$median_daily_dose,   na.rm = TRUE),
                        stats::median(gold_std$dose_daily_mg_equiv,         na.rm = TRUE))
)
print(as.data.frame(episode_counts), row.names = FALSE)
cat("\n")

cat("GOLD STANDARD COMPARISON (episode-level, median dose)\n")
cat(strrep("-", 40), "\n")
metrics_tbl <- tibble::tibble(
  Method           = c("Baseline", "NLP", "Advanced NLP"),
  Common_Patients  = c(ev_baseline$n_common_patients,
                       ev_nlp$n_common_patients,
                       ev_adv$n_common_patients),
  Coverage_pct     = round(c(ev_baseline$summary$coverage_pct,
                             ev_nlp$summary$coverage_pct,
                             ev_adv$summary$coverage_pct), 1),
  MAE_mg           = round(c(ev_baseline$summary$MAE,
                             ev_nlp$summary$MAE,
                             ev_adv$summary$MAE), 2),
  MBE_mg           = round(c(ev_baseline$summary$MBE,
                             ev_nlp$summary$MBE,
                             ev_adv$summary$MBE), 2),
  RMSE_mg          = round(c(ev_baseline$summary$RMSE,
                             ev_nlp$summary$RMSE,
                             ev_adv$summary$RMSE), 2),
  MAPE_pct         = round(c(ev_baseline$summary$MAPE,
                             ev_nlp$summary$MAPE,
                             ev_adv$summary$MAPE), 1),
  Pearson_r        = round(c(ev_baseline$summary$pearson_corr,
                             ev_nlp$summary$pearson_corr,
                             ev_adv$summary$pearson_corr), 3),
  Spearman_rho     = round(c(ev_baseline$summary$spearman_corr,
                             ev_nlp$summary$spearman_corr,
                             ev_adv$summary$spearman_corr), 3)
)
print(as.data.frame(metrics_tbl), row.names = FALSE)

cat("\nINTERPRETATION\n")
cat(strrep("-", 40), "\n")

# Coverage interpretation
best_cov_idx  <- which.max(metrics_tbl$Coverage_pct)
best_cov_name <- metrics_tbl$Method[best_cov_idx]
cat(sprintf(
  paste0(
    "Coverage: %s achieves the highest coverage (%.1f%%) of gold-standard\n",
    "  episodes. Coverage reflects how many gold-standard medication periods\n",
    "  have at least one overlapping method record with a usable dose.\n\n"
  ),
  best_cov_name, metrics_tbl$Coverage_pct[best_cov_idx]
))

# Accuracy interpretation
best_mae_idx  <- which.min(metrics_tbl$MAE_mg)
best_mae_name <- metrics_tbl$Method[best_mae_idx]
cat(sprintf(
  paste0(
    "Accuracy (MAE): %s has the lowest MAE (%.2f mg), indicating its\n",
    "  dose estimates are closest to manually reviewed values on average.\n",
    "  MAE is expressed in prednisone-equivalent mg/day.\n\n"
  ),
  best_mae_name, metrics_tbl$MAE_mg[best_mae_idx]
))

# Bias interpretation
for (i in seq_len(nrow(metrics_tbl))) {
  mbe <- metrics_tbl$MBE_mg[i]
  if (is.na(mbe)) {
    cat(sprintf("  %s: MBE not available (no matched episodes).\n",
                metrics_tbl$Method[i]))
  } else {
    direction <- if (mbe > 0) "over-estimates" else "under-estimates"
    cat(sprintf(
      "  %s %s by %.2f mg on average (MBE = %.2f mg).\n",
      metrics_tbl$Method[i], direction, abs(mbe), mbe
    ))
  }
}
cat("\n")

# NLP gain
nlp_gain <- ev_adv$summary$n_matched_records - ev_nlp$summary$n_matched_records
if (isTRUE(nlp_gain != 0)) {
  cat(sprintf(
    paste0(
      "Advanced NLP vs Standard NLP: Advanced NLP matched %d additional\n",
      "  gold-standard episodes (+%d records parsed via taper/advanced rules),\n",
      "  demonstrating the value of extended SIG parsing.\n\n"
    ),
    nlp_gain,
    sum(adv_nlp_df$parsed_status == "taper_ok", na.rm = TRUE)
  ))
}

# Correlation
best_cor_idx  <- which.max(metrics_tbl$Pearson_r)
best_cor_name <- metrics_tbl$Method[best_cor_idx]
cat(sprintf(
  paste0(
    "Correlation: %s shows the strongest linear association with the gold\n",
    "  standard (Pearson r = %.3f, Spearman ρ = %.3f). High Spearman\n",
    "  correlation with lower Pearson r suggests rank ordering is preserved\n",
    "  but the relationship is non-linear (common in dose distributions).\n\n"
  ),
  best_cor_name,
  metrics_tbl$Pearson_r[best_cor_idx],
  metrics_tbl$Spearman_rho[best_cor_idx]
))

cat(paste0(
  "RECOMMENDATION: Select the method based on the primary use case:\n",
  "  - Baseline is robust and achieves maximum coverage by leveraging\n",
  "    structured OMOP fields (quantity, days_supply, dose_unit), making\n",
  "    it suitable when SIG text quality is low.\n",
  "  - NLP is preferable when SIG text is consistently populated and\n",
  "    accurately recorded, yielding more precise dose estimates.\n",
  "  - Advanced NLP additionally handles taper schedules, recovering\n",
  "    records that standard NLP cannot parse, at the cost of added\n",
  "    complexity in SIG parsing.\n"
))

cat(strrep("=", 70), "\n")

# ===========================================================================
# 12. Save results to a timestamped run folder
# ===========================================================================
message("\n=== Saving results ===")

# Each run gets its own subfolder so previous results are never overwritten.
RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

# ── params.txt — human-readable record of every configuration value ──────────
writeLines(c(
  "SteroidDoseR — Run Parameters",
  strrep("=", 40),
  sprintf("Script:              CodeToRun.R"),
  sprintf("Run time:            %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Data",
  strrep("-", 40),
  sprintf("USE_SYNTHETIC:       %s", USE_SYNTHETIC),
  sprintf("START_DATE:          %s", START_DATE),
  sprintf("END_DATE:            %s", END_DATE),
  sprintf("STEROID_CONCEPT_IDS: %d concept IDs", length(STEROID_CONCEPT_IDS)),
  sprintf("COHORT_PERSON_IDS:   %s",
          if (is.null(COHORT_PERSON_IDS)) "NULL (all patients)"
          else sprintf("%d person IDs", length(COHORT_PERSON_IDS))),
  sprintf("Drug-exposure rows:  %d", nrow(drug_df)),
  sprintf("Unique patients:     %d", dplyr::n_distinct(drug_df$person_id)),
  "",
  "Episode building",
  strrep("-", 40),
  sprintf("GAP_DAYS:            %d", GAP_DAYS),
  "",
  "Evaluation",
  strrep("-", 40),
  sprintf("GOLD_STD_PATH:       %s", GOLD_STD_PATH)
), con = file.path(RUN_DIR, "params.txt"))

# ── Record-level dose data (one row per drug-exposure record) ────────────────
readr::write_csv(baseline_df,            file.path(RUN_DIR, "records_baseline.csv"))
readr::write_csv(nlp_df,                 file.path(RUN_DIR, "records_nlp.csv"))
readr::write_csv(adv_nlp_df,             file.path(RUN_DIR, "records_adv_nlp.csv"))

# ── Episode-level summaries (one row per patient-drug episode) ────────────────
readr::write_csv(baseline_episodes,      file.path(RUN_DIR, "episodes_baseline.csv"))
readr::write_csv(nlp_episodes,           file.path(RUN_DIR, "episodes_nlp.csv"))
readr::write_csv(adv_nlp_episodes,       file.path(RUN_DIR, "episodes_adv_nlp.csv"))

# ── Gold standard (with pred-equiv conversion applied) ───────────────────────
readr::write_csv(gold_std,               file.path(RUN_DIR, "gold_standard.csv"))

# ── Evaluation comparison tables (one row per matched gold episode) ──────────
readr::write_csv(ev_baseline$comparison, file.path(RUN_DIR, "comparison_baseline.csv"))
readr::write_csv(ev_nlp$comparison,      file.path(RUN_DIR, "comparison_nlp.csv"))
readr::write_csv(ev_adv$comparison,      file.path(RUN_DIR, "comparison_adv_nlp.csv"))

# ── Summary tables ────────────────────────────────────────────────────────────
readr::write_csv(episode_counts,         file.path(RUN_DIR, "episode_counts.csv"))
readr::write_csv(metrics_tbl,            file.path(RUN_DIR, "metrics_table.csv"))

# ── Plot data (for reproducing figures without re-running) ────────────────────
readr::write_csv(dist_df_all,            file.path(RUN_DIR, "plot_data_dose_distribution.csv"))
readr::write_csv(scatter_df,             file.path(RUN_DIR, "plot_data_scatter.csv"))
readr::write_csv(ba_df,                  file.path(RUN_DIR, "plot_data_bland_altman.csv"))
readr::write_csv(ba_limits,              file.path(RUN_DIR, "bland_altman_limits.csv"))

# ── Figures ───────────────────────────────────────────────────────────────────
ggplot2::ggsave(file.path(RUN_DIR, "plot_dose_distribution.png"), p_dist,    width = 8,  height = 10, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter.png"),           p_scatter, width = 10, height = 5,  dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_bland_altman.png"),      p_ba,      width = 10, height = 5,  dpi = 150)

message(sprintf("Results saved to: %s", RUN_DIR))

# ===========================================================================
# 13. Interactive dose review dashboard
# ===========================================================================
# launch_dose_dashboard() opens a Shiny app in the browser.
# Pass raw_list (record-level data frames) to populate the Raw Records tab
# with diagnostic columns (sig, imputation_method, daily_dose_mg_imputed,
# etc.) so individual prescription rows can be inspected alongside the
# dose trajectory plot.
#
message("\n=== Launching interactive dose review dashboard ===")
message("(Close the browser tab or press Escape in R to stop.)")

launch_dose_dashboard(
  episode_list = list(
    "Baseline"     = baseline_episodes,
    "NLP"          = nlp_episodes,
    "Advanced NLP" = adv_nlp_episodes
  ),
  raw_list = list(
    "Baseline"     = baseline_df,
    "NLP"          = nlp_df,
    "Advanced NLP" = adv_nlp_df
  ),
  gold_std = gold_std
)

# ===========================================================================
# 14. Disconnect (live DB only)
# ===========================================================================
if (!USE_SYNTHETIC) {
  if (inherits(conn, "DatabaseConnectorConnection")) {
    DatabaseConnector::disconnect(conn)
  } else {
    DBI::dbDisconnect(conn)
  }
}

message("\n=== Analysis complete ===")

