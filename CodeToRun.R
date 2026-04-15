# CodeToRun.R
# SteroidDoseR — Main Study Analysis Script
#
# Primary execution script for the corticosteroid dose study.
# Computes prednisone-equivalent daily doses using three methods
# (Baseline, NLP, Advanced NLP) and evaluates against a gold standard.
#
# Analysis workflow
# -----------------
# STEP 1 — Cohort selection : define COHORT_PERSON_IDS (ICD-10, phenotype, etc.)
#                              Default NULL = all patients in the database.
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
# Connection modes (set USE_SYNTHETIC below)
# ------------------------------------------
#   Mode A — Synthetic data (no database required):
#     USE_SYNTHETIC = TRUE
#
#   Mode B — Live OMOP CDM via DatabaseConnector (OHDSI standard):
#     USE_SYNTHETIC = FALSE
#     Create connectionDetails with DatabaseConnector::createConnectionDetails()
#     and wrap with create_omop_connector(). Supports SQL Server, PostgreSQL,
#     Databricks/Spark, Redshift, BigQuery, Snowflake, and more.
#     Required packages: DatabaseConnector, SqlRender

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

# Dose-agreement thresholds for the episode-level binary agreement table.
# A computed day is "acceptable" if the dose is within the threshold.
#   DOSE_THRESHOLD_MG  — absolute difference in mg pred-equiv  (e.g. 10)
#   DOSE_THRESHOLD_PCT — relative difference in %              (e.g. 20)
# Set either to NULL to disable that criterion.
# If both are non-NULL, a day passes if it meets EITHER criterion (OR logic).
DOSE_THRESHOLD_MG  <- 10     # mg pred-equiv; NULL to disable
DOSE_THRESHOLD_PCT <- NULL   # percent; NULL to disable

GOLD_STD_PATH  <- "H:/Myositis/DoseCalculation/Version2/GoldStandard/qc_gold_standard/corticosteroids_metrics_per_record.csv"

# ---------------------------------------------------------------------------
# STEP 1 of the analysis workflow: Patient cohort selection
# ---------------------------------------------------------------------------
# Default NULL = all patients in the database (no patient-level pre-filter).
# Replace NULL with a vector of integer person_ids to restrict the analysis
# to a specific cohort. Common approaches:
#
#   ICD-10 / computable phenotype (run after connect, Section 1):
#     cohort_sql        <- "SELECT DISTINCT person_id
#                             FROM @cdm_schema.condition_occurrence
#                            WHERE condition_concept_id IN (80809, ...)"
#     COHORT_PERSON_IDS <- as.integer(DatabaseConnector::querySql(
#                            conn, cohort_sql)$PERSON_ID)
#
#   Gold-standard patients only (validation mode):
#     gold_std          <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)
#     COHORT_PERSON_IDS <- unique(as.integer(gold_std$patient_id))
#
#   Medication data file (all reviewed myositis patients):
#     med_data          <- readr::read_csv(MED_DATA_PATH, show_col_types = FALSE)
#     COHORT_PERSON_IDS <- unique(as.integer(med_data$myositis_omop_person_id))
COHORT_PERSON_IDS <- NULL   # NULL = no filter (entire database)

# Steroid drug_concept_id allow-list (matches Version2 Baseline extraction).
# Loaded from the bundled CSV; each row is one integer concept ID.
STEROID_CONCEPT_IDS <- as.integer(readr::read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

# ---------------------------------------------------------------------------
# 1. Connect / load data
# ---------------------------------------------------------------------------
if (USE_SYNTHETIC) {
  # -----------------------------------------------------------------------
  # Mode A: bundled synthetic data — no database required
  # -----------------------------------------------------------------------
  message("=== Using bundled synthetic data ===")
  drug_df <- readr::read_csv(
    system.file("extdata", "synthetic_drug_exposure.csv", package = "SteroidDoseR"),
    show_col_types = FALSE
  )
} else {
  # -----------------------------------------------------------------------
  # Mode B: Live OMOP CDM via DatabaseConnector (OHDSI standard)
  # -----------------------------------------------------------------------
  message("=== Connecting to live OMOP CDM ===")

  server       <- "Esmpmdbpr4.esm.johnshopkins.edu"
  database     <- "Myositis_OMOP"
  port         <- 1433L
  cdm_schema   <- paste0(database, ".dbo")
  vocab_schema <- paste0(database, ".dbo")

  jdbc_url <- sprintf(
    paste0("jdbc:sqlserver://%s:%d;databaseName=%s;",
           "integratedSecurity=true;encrypt=true;trustServerCertificate=true;"),
    server, port, database
  )

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms             = "sql server",
    connectionString = jdbc_url,
    pathToDriver     = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
  )

  conn <- DatabaseConnector::connect(connectionDetails)

  sql <- SqlRender::readSql(
    system.file("sql", "extract_drug_exposure.sql", package = "SteroidDoseR")
  )

  drug_df <- DatabaseConnector::renderTranslateQuerySql(
    connection     = conn,
    sql            = sql,
    cdm_schema     = cdm_schema,
    vocab_schema   = vocab_schema,
    start_date     = START_DATE,
    end_date       = END_DATE,
    concept_filter = paste(STEROID_CONCEPT_IDS, collapse = ","),
    person_filter  = if (!is.null(COHORT_PERSON_IDS))
                       paste(COHORT_PERSON_IDS, collapse = ",") else "",
    snakeCaseToCamelCase = FALSE
  )

  DatabaseConnector::disconnect(conn)

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
# Helper: day-level dose agreement (dose-accuracy-aware binary classification)
# ===========================================================================
# Unit of analysis: patient-day (within each patient's gold observation window).
#
# A patient-day is classified as:
#   TP — in a gold episode AND in a computed episode AND dose within threshold
#   FP — NOT in a gold episode BUT in a computed episode  (spurious episode day)
#   FN — in a gold episode but dose NOT acceptable         (missed or wrong dose)
#   TN — NOT in a gold episode AND NOT in a computed episode
#
# Parameters
#   threshold_mg  — acceptable absolute dose difference (mg pred-equiv); NULL = off
#   threshold_pct — acceptable relative dose difference (%);             NULL = off
#   If both non-NULL, a day is acceptable when it meets EITHER criterion (OR).
#
compute_episode_dose_agreement <- function(
    computed_episodes,
    gold_df,
    threshold_mg   = 10,
    threshold_pct  = NULL,
    gold_id_col    = "patient_id",
    gold_start_col = "episode_start",
    gold_end_col   = "episode_end",
    gold_dose_col  = "median_daily_dose",
    comp_dose_col  = "median_daily_dose") {

  if (is.null(threshold_mg) && is.null(threshold_pct))
    stop("At least one of threshold_mg or threshold_pct must be non-NULL.")

  comp_pts    <- unique(as.integer(computed_episodes$person_id))
  gold_pts    <- unique(as.integer(gold_df[[gold_id_col]]))
  overlap_pts <- intersect(comp_pts, gold_pts)

  if (length(overlap_pts) == 0L) {
    warning("No overlapping patients — returning NA metrics.")
    return(tibble::tibble(
      Threshold = NA_character_,
      TP = NA_integer_, FP = NA_integer_, FN = NA_integer_, TN = NA_integer_,
      Sensitivity = NA_real_, Specificity = NA_real_,
      PPV = NA_real_, NPV = NA_real_, F1 = NA_real_, Kappa = NA_real_
    ))
  }

  # Expand episodes to (pt_id, day, dose) — one row per patient-day per episode
  expand_to_days_dose <- function(df, id_col, start_col, end_col, dose_col) {
    df |>
      dplyr::filter(as.integer(.data[[id_col]]) %in% overlap_pts) |>
      dplyr::select(
        pt_id   = dplyr::all_of(id_col),
        e_start = dplyr::all_of(start_col),
        e_end   = dplyr::all_of(end_col),
        dose    = dplyr::all_of(dose_col)
      ) |>
      dplyr::mutate(
        pt_id   = as.integer(.data$pt_id),
        e_start = as.Date(.data$e_start),
        e_end   = as.Date(.data$e_end),
        dose    = as.numeric(.data$dose)
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(day = list(seq.Date(.data$e_start, .data$e_end, by = "day"))) |>
      dplyr::ungroup() |>
      tidyr::unnest(cols = day) |>
      dplyr::select(pt_id, day, dose) |>
      dplyr::distinct(pt_id, day, .keep_all = TRUE)   # keep first if episodes overlap
  }

  gold_days <- expand_to_days_dose(
    gold_df, gold_id_col, gold_start_col, gold_end_col, gold_dose_col)
  comp_days <- expand_to_days_dose(
    computed_episodes, "person_id", "episode_start", "episode_end", comp_dose_col)

  # Per-patient observation window = date range of that patient's gold episodes
  gold_windows <- gold_df |>
    dplyr::filter(as.integer(.data[[gold_id_col]]) %in% overlap_pts) |>
    dplyr::group_by(pt_id = as.integer(.data[[gold_id_col]])) |>
    dplyr::summarise(
      win_start = min(as.Date(.data[[gold_start_col]]), na.rm = TRUE),
      win_end   = max(as.Date(.data[[gold_end_col]]),   na.rm = TRUE),
      .groups   = "drop"
    )

  # Enumerate all patient-days within gold observation windows
  all_days <- gold_windows |>
    dplyr::rowwise() |>
    dplyr::mutate(day = list(seq.Date(.data$win_start, .data$win_end, by = "day"))) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = day) |>
    dplyr::select(pt_id, day)

  # Join gold and computed doses onto the full patient-day grid
  day_tbl <- all_days |>
    dplyr::left_join(
      gold_days |> dplyr::rename(gold_dose = dose),
      by = c("pt_id", "day")
    ) |>
    dplyr::left_join(
      comp_days |> dplyr::rename(comp_dose = dose),
      by = c("pt_id", "day")
    ) |>
    dplyr::mutate(
      gold_on = !is.na(.data$gold_dose),
      comp_on = !is.na(.data$comp_dose)
    )

  # Dose-acceptability criterion (vectorised, outside mutate)
  dose_diff     <- abs(day_tbl$comp_dose - day_tbl$gold_dose)
  dose_diff_pct <- 100 * dose_diff / day_tbl$gold_dose

  within_abs <- if (!is.null(threshold_mg)) {
    !is.na(dose_diff) & dose_diff <= threshold_mg
  } else {
    rep(FALSE, nrow(day_tbl))
  }

  within_pct <- if (!is.null(threshold_pct)) {
    !is.na(dose_diff_pct) & dose_diff_pct <= threshold_pct
  } else {
    rep(FALSE, nrow(day_tbl))
  }

  dose_ok_criterion <- if (!is.null(threshold_mg) && !is.null(threshold_pct)) {
    within_abs | within_pct   # OR: either criterion is sufficient
  } else if (!is.null(threshold_mg)) {
    within_abs
  } else {
    within_pct
  }

  day_tbl$dose_ok <- day_tbl$gold_on & day_tbl$comp_on & dose_ok_criterion

  TP <- sum( day_tbl$dose_ok)
  FP <- sum(!day_tbl$gold_on &  day_tbl$comp_on)  # computed day with no gold episode
  FN <- sum( day_tbl$gold_on & !day_tbl$dose_ok)  # gold day missed or dose wrong
  TN <- sum(!day_tbl$gold_on & !day_tbl$comp_on)  # correctly no episode
  N  <- TP + FP + FN + TN

  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv         <- TP / (TP + FP)
  npv         <- TN / (TN + FN)
  f1          <- 2L * TP / (2L * TP + FP + FN)

  po    <- (TP + TN) / N
  pe    <- ((TP + FN) * (TP + FP) + (TN + FP) * (TN + FN)) / N^2
  kappa <- (po - pe) / (1 - pe)

  thr_label <- if (!is.null(threshold_mg) && !is.null(threshold_pct)) {
    sprintf("%g mg OR %g%%", threshold_mg, threshold_pct)
  } else if (!is.null(threshold_mg)) {
    sprintf("%g mg", threshold_mg)
  } else {
    sprintf("%g%%", threshold_pct)
  }

  tibble::tibble(
    Threshold   = thr_label,
    TP          = TP,
    FP          = FP,
    FN          = FN,
    TN          = TN,
    Sensitivity = round(sensitivity, 3),
    Specificity = round(specificity, 3),
    PPV         = round(ppv,         3),
    NPV         = round(npv,         3),
    F1          = round(f1,          3),
    Kappa       = round(kappa,       3)
  )
}


# ===========================================================================
# 4. BASELINE METHOD
# ===========================================================================
message("\n=== [1/3] Baseline method ===")

baseline_df <- calc_daily_dose_baseline(
  drug_df,
  m2_sig_parse      = "warn",
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
  m2_sig_parse = "warn",
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

# Advanced NLP episodes — convert to pred-equiv first (same as run_pipeline),
# then build episodes so the dose scale matches baseline and NLP episodes.
adv_nlp_df <- convert_pred_equiv(
  adv_nlp_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg"
)
adv_nlp_episodes <- build_episodes(
  adv_nlp_df,
  end_col  = "drug_exposure_end_date",
  dose_col = "pred_equiv_mg",
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

gold_std <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)

cat(sprintf(
  "Gold standard: %d episodes from %d patients\n",
  nrow(gold_std), dplyr::n_distinct(gold_std$patient_id)
))

# --- Convert gold standard doses to prednisone-equivalent -------------------
# The gold standard records doses in the native drug unit (e.g., methylpred
# 8 mg ≠ 10 mg pred-equiv). We identify each gold episode's drug by finding
# the most-frequent drug in drug_df that overlaps the gold episode window,
# then apply the same convert_pred_equiv() used on the computed side.
# Use baseline_df (oral-filtered) so injection records cannot become the
# dominant drug and corrupt the pred-equiv conversion.
gold_drug_map <- baseline_df |>
  dplyr::select(person_id, drug_name_std,
                drug_exposure_start_date, drug_exposure_end_date) |>
  dplyr::rename(patient_id = person_id) |>
  dplyr::inner_join(
    gold_std |> dplyr::select(patient_id, episode_start, episode_end),
    by = "patient_id", relationship = "many-to-many"
  ) |>
  dplyr::filter(
    as.Date(drug_exposure_start_date) <= as.Date(episode_end),
    as.Date(drug_exposure_end_date)   >= as.Date(episode_start),
    !is.na(drug_name_std)
  ) |>
  dplyr::group_by(patient_id, episode_start, episode_end, drug_name_std) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(patient_id, episode_start, episode_end) |>
  dplyr::slice_max(n, n = 1L, with_ties = FALSE) |>   # most common drug
  dplyr::ungroup() |>
  dplyr::select(patient_id, episode_start, episode_end, drug_name_std)

gold_std <- gold_std |>
  dplyr::left_join(gold_drug_map,
                   by = c("patient_id", "episode_start", "episode_end")) |>
  convert_pred_equiv(
    drug_col = "drug_name_std",
    dose_col = "median_daily_dose",
    out_col  = "gold_pred_equiv_mg"
  ) |>
  dplyr::mutate(
    median_daily_dose_raw = median_daily_dose,
    median_daily_dose     = dplyr::coalesce(gold_pred_equiv_mg, median_daily_dose)
  )

cat(sprintf(
  "Gold std drug mapping: %d episodes converted to pred-equiv | %d drug unknown (kept raw)\n",
  sum(gold_std$pred_equiv_status == "ok",        na.rm = TRUE),
  sum(gold_std$pred_equiv_status != "ok"| is.na(gold_std$pred_equiv_status), na.rm = TRUE)
))
cat("\nGold standard preview (with pred-equiv dose):\n")
print(head(gold_std[, c("patient_id", "episode_start", "episode_end",
                         "drug_name_std", "median_daily_dose_raw",
                         "median_daily_dose", "days_covered")]))

cat("\nGold standard dose distribution (pred-equiv):\n")
print(summary(gold_std$median_daily_dose))

# --- Distribution plot including gold standard (4 panels) -------------------
gold_dist_df <- gold_std |>
  dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
  dplyr::transmute(
    person_id        = as.integer(patient_id),
    drug_name_std    = dplyr::coalesce(drug_name_std, "unknown"),
    median_daily_dose,
    method           = "Gold"
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

# --- 9a. Baseline ---
message("\n  Baseline vs gold standard ...")
ev_baseline <- evaluate_against_gold(
  baseline_episodes,
  gold_std,
  gold_id_col = "patient_id"
)
ev_baseline$n_common_patients <- length(intersect(
  unique(baseline_episodes$person_id), unique(gold_std$patient_id)
))

cat(sprintf(
  "\nBaseline: %d common patients | %d/%d gold episodes matched (%.1f%% coverage)\n",
  ev_baseline$n_common_patients,
  ev_baseline$summary$n_matched_periods,
  ev_baseline$summary$n_gold_periods,
  ev_baseline$summary$coverage_pct
))
cat("\nBaseline summary metrics:\n")
print(as.data.frame(ev_baseline$summary))

print_agreement(ev_baseline$comparison, "Baseline")

cat("\nBaseline — top-10 largest errors:\n")
ev_baseline$comparison |>
  dplyr::filter(!is.na(computed_dose)) |>
  dplyr::arrange(dplyr::desc(absolute_error)) |>
  dplyr::select(patient_id, episode_start, episode_end, gold_dose,
                computed_dose, absolute_error, bias_error) |>
  head(10) |>
  print()

# --- 9b. NLP ---
message("\n  NLP vs gold standard ...")
ev_nlp <- evaluate_against_gold(
  nlp_episodes,
  gold_std,
  gold_id_col = "patient_id"
)
ev_nlp$n_common_patients <- length(intersect(
  unique(nlp_episodes$person_id), unique(gold_std$patient_id)
))

cat(sprintf(
  "\nNLP: %d common patients | %d/%d gold episodes matched (%.1f%% coverage)\n",
  ev_nlp$n_common_patients,
  ev_nlp$summary$n_matched_periods,
  ev_nlp$summary$n_gold_periods,
  ev_nlp$summary$coverage_pct
))
cat("\nNLP summary metrics:\n")
print(as.data.frame(ev_nlp$summary))

print_agreement(ev_nlp$comparison, "NLP")

# --- 9c. Advanced NLP ---
message("\n  Advanced NLP vs gold standard ...")
ev_adv <- evaluate_against_gold(
  adv_nlp_episodes,
  gold_std,
  gold_id_col = "patient_id"
)
ev_adv$n_common_patients <- length(intersect(
  unique(adv_nlp_episodes$person_id), unique(gold_std$patient_id)
))

cat(sprintf(
  "\nAdvanced NLP: %d common patients | %d/%d gold episodes matched (%.1f%% coverage)\n",
  ev_adv$n_common_patients,
  ev_adv$summary$n_matched_periods,
  ev_adv$summary$n_gold_periods,
  ev_adv$summary$coverage_pct
))
cat("\nAdvanced NLP summary metrics:\n")
print(as.data.frame(ev_adv$summary))

print_agreement(ev_adv$comparison, "Advanced NLP")

# ===========================================================================
# 10. Comparison scatter plots (method dose vs gold dose)
# ===========================================================================
message("\n=== Comparison scatter plots ===")

make_scatter_df <- function(ev_result, method_label) {
  ev_result$comparison |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::transmute(
      patient_id,
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
gold_patients_rpt    <- unique(as.integer(gold_std$patient_id))
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
                        dplyr::n_distinct(gold_std$patient_id)),
  Episodes          = c(nrow(baseline_episodes),
                        nrow(nlp_episodes),
                        nrow(adv_nlp_episodes),
                        nrow(gold_std)),
  Overlap_with_Gold = c(ev_baseline$summary$n_matched_periods,
                        ev_nlp$summary$n_matched_periods,
                        ev_adv$summary$n_matched_periods,
                        NA_integer_),
  Median_mg         = c(stats::median(baseline_episodes$median_daily_dose, na.rm = TRUE),
                        stats::median(nlp_episodes$median_daily_dose,       na.rm = TRUE),
                        stats::median(adv_nlp_episodes$median_daily_dose,   na.rm = TRUE),
                        stats::median(gold_std$median_daily_dose,           na.rm = TRUE))
)
print(as.data.frame(episode_counts), row.names = FALSE)
cat("\n")

cat("EPISODE-LEVEL DOSE AGREEMENT (day-level, overlapping patients)\n")
cat(strrep("-", 40), "\n")
cat("  Observation window per patient = range of that patient's gold episodes.\n")
cat("  TP = patient-day in gold episode AND computed dose within threshold.\n")
cat("  FP = patient-day in computed episode but no gold episode (spurious).\n")
cat("  FN = patient-day in gold episode but dose missing or outside threshold.\n")
cat("  TN = patient-day in neither gold nor computed episode.\n")
thr_desc <- if (!is.null(DOSE_THRESHOLD_MG) && !is.null(DOSE_THRESHOLD_PCT)) {
  sprintf("Threshold: %g mg pred-equiv OR %g%%\n\n", DOSE_THRESHOLD_MG, DOSE_THRESHOLD_PCT)
} else if (!is.null(DOSE_THRESHOLD_MG)) {
  sprintf("Threshold: %g mg pred-equiv\n\n", DOSE_THRESHOLD_MG)
} else {
  sprintf("Threshold: %g%%\n\n", DOSE_THRESHOLD_PCT)
}
cat(thr_desc)

message("  Computing dose agreement — Baseline ...")
agr_baseline <- compute_episode_dose_agreement(
  baseline_episodes,  gold_std,
  threshold_mg  = DOSE_THRESHOLD_MG,
  threshold_pct = DOSE_THRESHOLD_PCT,
  gold_id_col   = "patient_id"
)
message("  Computing dose agreement — NLP ...")
agr_nlp      <- compute_episode_dose_agreement(
  nlp_episodes,       gold_std,
  threshold_mg  = DOSE_THRESHOLD_MG,
  threshold_pct = DOSE_THRESHOLD_PCT,
  gold_id_col   = "patient_id"
)
message("  Computing dose agreement — Advanced NLP ...")
agr_adv      <- compute_episode_dose_agreement(
  adv_nlp_episodes,   gold_std,
  threshold_mg  = DOSE_THRESHOLD_MG,
  threshold_pct = DOSE_THRESHOLD_PCT,
  gold_id_col   = "patient_id"
)

agreement_tbl <- dplyr::bind_rows(
  dplyr::mutate(agr_baseline, Method = "Baseline",     .before = 1),
  dplyr::mutate(agr_nlp,      Method = "NLP",          .before = 1),
  dplyr::mutate(agr_adv,      Method = "Advanced NLP", .before = 1)
)
print(as.data.frame(agreement_tbl), row.names = FALSE)
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
nlp_gain <- ev_adv$summary$n_matched_periods - ev_nlp$summary$n_matched_periods
if (!is.na(nlp_gain) && nlp_gain != 0) {
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
# 12. Interactive dose review dashboard
# ===========================================================================
# launch_dose_dashboard() opens a Shiny app in the browser.
# Pass raw_list (record-level data frames) to populate the Raw Records tab
# with diagnostic columns (sig, imputation_method, daily_dose_mg_imputed,
# pred_equiv_mg, etc.) so individual prescription rows can be inspected
# alongside the dose trajectory plot.
#
# adv_nlp_df already has pred_equiv_mg (converted in Section 6).
# Convert baseline_df and nlp_df to pred-equiv here for consistency.
message("\n=== Launching interactive dose review dashboard ===")
message("(Close the browser tab or press Escape in R to stop.)")

baseline_eq <- convert_pred_equiv(
  baseline_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg_imputed"
)

nlp_eq <- convert_pred_equiv(
  nlp_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg"
)

launch_dose_dashboard(
  episode_list = list(
    "Baseline"     = baseline_episodes,
    "NLP"          = nlp_episodes,
    "Advanced NLP" = adv_nlp_episodes
  ),
  raw_list = list(
    "Baseline"     = baseline_eq,
    "NLP"          = nlp_eq,
    "Advanced NLP" = adv_nlp_df   # pred_equiv_mg already present from Section 6
  ),
  gold_std = gold_std
)

# ===========================================================================
# 13. Disconnect (live DB only)
# ===========================================================================
if (!USE_SYNTHETIC) {
  disconnect_connector(con)
}

message("\n=== Analysis complete ===")
