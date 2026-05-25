# CodeToRun_hierarchical.R
# SteroidDoseR — Hierarchical Method Analysis Script
#
# Runs the hierarchical Baseline × NLP Advanced method and evaluates it
# against the gold standard, alongside Baseline and Advanced NLP for comparison.
#
# Hierarchical decision tree (per record):
# ─────────────────────────────────────────────────────────────────────────────
#  Baseline (M1/M3/M4)   SIG complete      Outcome
#  ─────────────────────────────────────────────────────────────────────────
#  complete               complete          cross_checked  → |diff| ≤ MATCH_TOL
#                                           blended        → |diff| ≤ DIFF_THRESHOLD
#                                           nlp_override   → |diff| > DIFF_THRESHOLD
#  complete               absent/unparseable baseline_only
#  incomplete             complete, steady  nlp_fills_baseline
#  incomplete             complete, taper   nlp_taper
#  incomplete             absent/unparseable missing (NA)
# ─────────────────────────────────────────────────────────────────────────────
#
# Analysis workflow
# -----------------
# STEP 1 — Extraction       : pull steroid drug_exposure from OMOP CDM.
# STEP 2 — Medication filter : restrict to oral steroids.
# STEP 3 — Hierarchical dose : calc_daily_dose_hierarchical().
# STEP 4 — Comparators       : Baseline, Advanced NLP (for benchmarking).
# STEP 5 — Method breakdown  : how many records took each branch.
# STEP 6 — Evaluation        : compare all three methods to gold standard.
#
# Usage
# -----
#   Source this file interactively in RStudio, or run:
#     Rscript CodeToRun_hierarchical.R
#
# Connection modes  (set USE_SYNTHETIC below)
# --------------------------------------------
#   Mode A — Synthetic data (no database required):   USE_SYNTHETIC = TRUE
#   Mode B / C — Live OMOP CDM:                       USE_SYNTHETIC = FALSE
#                Fill in ONE connection block in Section 1a.

devtools::install_local(getwd())
if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)
library(ggplot2)

# ===========================================================================
# 0. Configuration
# ===========================================================================
USE_SYNTHETIC  <- FALSE
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
GAP_DAYS       <- 30L
CONCURRENT_AGG <- "per_drug"   # "per_drug" or "sum_all"

# -- Hierarchical method parameters ------------------------------------------
# DIFF_THRESHOLD: when |baseline_dose - nlp_dose| > this, NLP overrides baseline.
# MATCH_TOL:      when |diff| <= this, the two estimates are considered to match.
# Both in mg/day (prednisone-equivalent before conversion).
DIFF_THRESHOLD <- 0     # mg/day
MATCH_TOL      <- 0.01  # mg/day

# -- Dose-agreement thresholds (gold-standard evaluation) --------------------
DOSE_THRESHOLD_MG  <- 10    # absolute mg pred-equiv; NULL to disable
DOSE_THRESHOLD_PCT <- NULL  # relative %; NULL to disable

GOLD_STD_PATH  <- "/your/path/to/gold-standard"
OUTPUT_DIR     <- file.path(getwd(), "output_hierarchical")

COHORT_PERSON_IDS <- NULL

STEROID_CONCEPT_IDS <- as.integer(readr::read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

# ===========================================================================
# 1a. Connection  —  fill in ONE option (live DB only; skip for synthetic)
# ===========================================================================
if (!USE_SYNTHETIC) {

  # ── Option B: DatabaseConnector
  # cdm_schema   <- "database.dbo"
  # vocab_schema <- "database.dbo"
  # connectionDetails <- DatabaseConnector::createConnectionDetails(
  #   dbms             = "sql server",
  #   connectionString = "",
  #   pathToDriver     = ""
  # )
  # conn <- DatabaseConnector::connect(connectionDetails)

  # ── Option C: DBI / odbc  (Databricks or any ODBC source)
  # library(DBI); library(odbc)
  # DB_DIALECT   <- "spark"
  # cdm_schema   <- "catalog.schema"
  # vocab_schema <- "catalog.schema"
  # host         <- ""
  # http_path    <- ""
  # token        <- ""
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

read_pkg_sql <- function(filename) {
  path <- system.file("sql", filename, package = "SteroidDoseR")
  if (nchar(path) == 0L) path <- file.path("inst", "sql", filename)
  if (!file.exists(path)) stop("SQL file not found: ", filename)
  SqlRender::readSql(path)
}

# ===========================================================================
# 1b. Query helper
# ===========================================================================
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

# ===========================================================================
# 1c. Load data
# ===========================================================================
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
  "Fetched %d rows | %d unique persons | concept filter: %d steroid concept IDs",
  nrow(drug_df), length(unique(drug_df$person_id)), length(STEROID_CONCEPT_IDS)
))

drug_df <- drug_df |>
  dplyr::mutate(drug_name_std = standardize_drug_name(drug_concept_name))

if (!"drug_exposure_id" %in% names(drug_df)) {
  drug_df <- drug_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())
}

# ===========================================================================
# Helper functions (identical to CodeToRun.R)
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
  cat(sprintf("\n%s agreement (n=%d):  %s\n",
              label, total, paste(parts, collapse = "  |  ")))
}

show_person_trajectories <- function(episodes_df, method_name, n_patients = 3L) {
  cat(sprintf(
    "\n--- %s: %d episodes from %d persons (sample trajectories) ---\n",
    method_name, nrow(episodes_df), dplyr::n_distinct(episodes_df$person_id)
  ))
  sample_pts <- episodes_df |>
    dplyr::count(person_id, sort = TRUE) |>
    dplyr::slice_head(n = n_patients) |>
    dplyr::pull(person_id)
  for (pt in sample_pts) {
    cat(sprintf("\n  Patient %s:\n", pt))
    traj <- episodes_df |>
      dplyr::filter(person_id == pt) |>
      dplyr::arrange(episode_start) |>
      dplyr::select(drug_name_std, episode_start, episode_end,
                    n_days, n_records, median_daily_dose, mean_daily_dose)
    print(as.data.frame(traj), row.names = FALSE)
  }
}

compute_episode_dose_agreement <- function(
    computed_episodes, gold_df,
    threshold_mg   = 10, threshold_pct  = NULL,
    gold_id_col    = "patient_id", gold_start_col = "episode_start",
    gold_end_col   = "episode_end", gold_dose_col  = "median_daily_dose",
    comp_dose_col  = "median_daily_dose") {

  if (is.null(threshold_mg) && is.null(threshold_pct))
    stop("At least one threshold must be non-NULL.")

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

  expand_to_days_dose <- function(df, id_col, start_col, end_col, dose_col) {
    df |>
      dplyr::filter(as.integer(.data[[id_col]]) %in% overlap_pts) |>
      dplyr::select(pt_id = dplyr::all_of(id_col),
                    e_start = dplyr::all_of(start_col),
                    e_end   = dplyr::all_of(end_col),
                    dose    = dplyr::all_of(dose_col)) |>
      dplyr::mutate(pt_id   = as.integer(.data$pt_id),
                    e_start = as.Date(.data$e_start),
                    e_end   = as.Date(.data$e_end),
                    dose    = as.numeric(.data$dose)) |>
      dplyr::rowwise() |>
      dplyr::mutate(day = list(seq.Date(.data$e_start, .data$e_end, by = "day"))) |>
      dplyr::ungroup() |>
      tidyr::unnest(cols = day) |>
      dplyr::select(pt_id, day, dose) |>
      dplyr::distinct(pt_id, day, .keep_all = TRUE)
  }

  gold_days <- expand_to_days_dose(
    gold_df, gold_id_col, gold_start_col, gold_end_col, gold_dose_col)
  comp_days <- expand_to_days_dose(
    computed_episodes, "person_id", "episode_start", "episode_end", comp_dose_col)

  gold_windows <- gold_df |>
    dplyr::filter(as.integer(.data[[gold_id_col]]) %in% overlap_pts) |>
    dplyr::group_by(pt_id = as.integer(.data[[gold_id_col]])) |>
    dplyr::summarise(win_start = min(as.Date(.data[[gold_start_col]]), na.rm = TRUE),
                     win_end   = max(as.Date(.data[[gold_end_col]]),   na.rm = TRUE),
                     .groups   = "drop")

  all_days <- gold_windows |>
    dplyr::rowwise() |>
    dplyr::mutate(day = list(seq.Date(.data$win_start, .data$win_end, by = "day"))) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = day) |>
    dplyr::select(pt_id, day)

  day_tbl <- all_days |>
    dplyr::left_join(gold_days |> dplyr::rename(gold_dose = dose), by = c("pt_id", "day")) |>
    dplyr::left_join(comp_days |> dplyr::rename(comp_dose = dose), by = c("pt_id", "day")) |>
    dplyr::mutate(gold_on = !is.na(.data$gold_dose), comp_on = !is.na(.data$comp_dose))

  dose_diff     <- abs(day_tbl$comp_dose - day_tbl$gold_dose)
  dose_diff_pct <- 100 * dose_diff / day_tbl$gold_dose

  within_abs <- if (!is.null(threshold_mg))
    !is.na(dose_diff) & dose_diff <= threshold_mg else rep(FALSE, nrow(day_tbl))
  within_pct <- if (!is.null(threshold_pct))
    !is.na(dose_diff_pct) & dose_diff_pct <= threshold_pct else rep(FALSE, nrow(day_tbl))

  dose_ok_criterion <- if (!is.null(threshold_mg) && !is.null(threshold_pct))
    within_abs | within_pct else if (!is.null(threshold_mg)) within_abs else within_pct

  day_tbl$dose_ok <- day_tbl$gold_on & day_tbl$comp_on & dose_ok_criterion

  TP <- sum( day_tbl$dose_ok)
  FP <- sum(!day_tbl$gold_on &  day_tbl$comp_on)
  FN <- sum( day_tbl$gold_on & !day_tbl$dose_ok)
  TN <- sum(!day_tbl$gold_on & !day_tbl$comp_on)
  N  <- TP + FP + FN + TN

  po    <- (TP + TN) / N
  pe    <- ((TP + FN) * (TP + FP) + (TN + FP) * (TN + FN)) / N^2
  kappa <- (po - pe) / (1 - pe)

  thr_label <- if (!is.null(threshold_mg) && !is.null(threshold_pct))
    sprintf("%g mg OR %g%%", threshold_mg, threshold_pct)
  else if (!is.null(threshold_mg)) sprintf("%g mg", threshold_mg)
  else sprintf("%g%%", threshold_pct)

  tibble::tibble(
    Threshold   = thr_label,
    TP = TP, FP = FP, FN = FN, TN = TN,
    Sensitivity = round(TP / (TP + FN),                3),
    Specificity = round(TN / (TN + FP),                3),
    PPV         = round(TP / (TP + FP),                3),
    NPV         = round(TN / (TN + FN),                3),
    F1          = round(2L * TP / (2L * TP + FP + FN), 3),
    Kappa       = round(kappa,                          3)
  )
}

# ===========================================================================
# 4. HIERARCHICAL METHOD  (primary)
# ===========================================================================
message("\n=== [1/3] Hierarchical method ===")
message(sprintf(
  "  Parameters: diff_threshold = %g mg/day | match_tol = %g mg/day",
  DIFF_THRESHOLD, MATCH_TOL
))

hier_df <- calc_daily_dose_hierarchical(
  drug_df,
  diff_threshold    = DIFF_THRESHOLD,
  match_tol         = MATCH_TOL,
  max_daily_dose_mg = 100,
  filter_oral       = TRUE
)

# ── Method-choice breakdown ─────────────────────────────────────────────────
cat("\nhierarchical_method breakdown:\n")
method_tbl <- hier_df |>
  dplyr::count(hierarchical_method, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(method_tbl), row.names = FALSE)

cat(sprintf(
  "\nRecords with a computable dose: %d / %d (%.1f%%)\n",
  sum(!is.na(hier_df$daily_dose_mg)),
  nrow(hier_df),
  100 * mean(!is.na(hier_df$daily_dose_mg))
))

# ── Cross-check discrepancy summary ─────────────────────────────────────────
# Rows where both baseline and SIG produced a dose — examine the diffs
xcheck_rows <- hier_df |>
  dplyr::filter(hierarchical_method %in% c("cross_checked", "blended", "nlp_override")) |>
  dplyr::mutate(dose_diff = abs(bl_dose - sig_dose))

if (nrow(xcheck_rows) > 0L) {
  cat(sprintf(
    "\nCross-check rows (%d records where both baseline and SIG produced a dose):\n",
    nrow(xcheck_rows)
  ))
  cat("  Dose difference (|baseline - SIG|) distribution:\n")
  print(summary(xcheck_rows$dose_diff))

  cross_summary <- xcheck_rows |>
    dplyr::count(hierarchical_method) |>
    dplyr::mutate(pct = round(100 * n / nrow(xcheck_rows), 1))
  cat("\n  Resolution of cross-checked rows:\n")
  print(as.data.frame(cross_summary), row.names = FALSE)

  # Inspect largest overrides (where NLP overrode baseline)
  overrides <- hier_df |>
    dplyr::filter(hierarchical_method == "nlp_override") |>
    dplyr::mutate(dose_diff = abs(bl_dose - sig_dose)) |>
    dplyr::arrange(dplyr::desc(dose_diff))

  if (nrow(overrides) > 0L) {
    cat("\n  Top 10 nlp_override records (largest discrepancy):\n")
    overrides |>
      dplyr::select(person_id, drug_name_std, sig,
                    bl_dose, bl_method, sig_dose, sig_status,
                    dose_diff, daily_dose_mg) |>
      head(10) |>
      print()
  }
}

# ── NLP-fills-baseline breakdown ────────────────────────────────────────────
fills_rows <- hier_df |> dplyr::filter(hierarchical_method == "nlp_fills_baseline")
if (nrow(fills_rows) > 0L) {
  cat(sprintf(
    "\nnlp_fills_baseline: %d records where baseline lacked a dose but SIG succeeded.\n",
    nrow(fills_rows)
  ))
  cat("  sig_status breakdown within nlp_fills_baseline:\n")
  print(table(fills_rows$sig_status, useNA = "ifany"))
}

# ── Remaining no-dose records ────────────────────────────────────────────────
missing_rows <- hier_df |> dplyr::filter(hierarchical_method == "missing")
if (nrow(missing_rows) > 0L) {
  cat(sprintf(
    "\nmissing: %d records where neither baseline nor SIG could compute a dose.\n",
    nrow(missing_rows)
  ))
  cat("  drug_name_std breakdown:\n")
  print(table(missing_rows$drug_name_std, useNA = "ifany"))
}

cat("\nHierarchical dose summary (non-missing):\n")
print(summary(hier_df$daily_dose_mg[!is.na(hier_df$daily_dose_mg)]))

# ── Sample rows ─────────────────────────────────────────────────────────────
cat("\nSample rows across method branches:\n")
hier_df |>
  dplyr::filter(!is.na(daily_dose_mg)) |>
  dplyr::group_by(hierarchical_method) |>
  dplyr::slice_head(n = 2L) |>
  dplyr::ungroup() |>
  dplyr::select(person_id, drug_name_std, sig,
                bl_dose, bl_method,
                sig_dose, sig_status,
                daily_dose_mg, hierarchical_method) |>
  print()

# ── Build episodes ───────────────────────────────────────────────────────────
hier_df_eq <- convert_pred_equiv(
  hier_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg"
)

hier_episodes <- build_episodes(
  hier_df_eq,
  end_col        = "drug_exposure_end_date",
  dose_col       = "pred_equiv_mg",
  gap_days       = GAP_DAYS,
  concurrent_agg = CONCURRENT_AGG
)

show_person_trajectories(hier_episodes, "Hierarchical")

# ===========================================================================
# 5. BASELINE METHOD  (comparator)
# ===========================================================================
message("\n=== [2/3] Baseline method (comparator) ===")

baseline_df <- calc_daily_dose_baseline(
  drug_df,
  m2_sig_parse      = "warn",
  max_daily_dose_mg = 100,
  filter_oral       = TRUE
)

cat("\nImputation method breakdown:\n")
print(table(baseline_df$imputation_method, useNA = "ifany"))

cat("\nDose summary (non-missing):\n")
print(summary(baseline_df$daily_dose_mg_imputed[!is.na(baseline_df$daily_dose_mg_imputed)]))

baseline_episodes <- run_pipeline(
  drug_df,
  method         = "baseline",
  m2_sig_parse   = "warn",
  return_level   = "episode",
  gap_days       = GAP_DAYS,
  concurrent_agg = CONCURRENT_AGG
)

show_person_trajectories(baseline_episodes, "Baseline")

# ===========================================================================
# 6. ADVANCED NLP METHOD  (comparator)
# ===========================================================================
message("\n=== [3/3] Advanced NLP method (comparator) ===")

adv_nlp_df <- calc_daily_dose_nlp_advanced(
  drug_df,
  max_daily_dose_mg = 100,
  expand_tapers     = FALSE,
  filter_oral       = TRUE
)

cat("\nparsed_status breakdown (Advanced NLP):\n")
print(table(adv_nlp_df$parsed_status, useNA = "ifany"))

cat("\nDose summary (ok / taper_ok):\n")
ok_mask <- adv_nlp_df$parsed_status %in% c("ok", "taper_ok")
print(summary(adv_nlp_df$daily_dose_mg[ok_mask]))

adv_nlp_df_eq <- convert_pred_equiv(adv_nlp_df, drug_col = "drug_name_std",
                                    dose_col = "daily_dose_mg")
adv_nlp_episodes <- build_episodes(
  adv_nlp_df_eq,
  end_col        = "drug_exposure_end_date",
  dose_col       = "pred_equiv_mg",
  gap_days       = GAP_DAYS,
  concurrent_agg = CONCURRENT_AGG
)

show_person_trajectories(adv_nlp_episodes, "Advanced NLP")

# ===========================================================================
# 7. BRANCH COMPARISON: Hierarchical vs individual methods
# ===========================================================================
message("\n=== Method comparison: hierarchical branch analysis ===")

# For each hierarchical_method, compare final dose to the individual methods
branch_summary <- hier_df |>
  dplyr::filter(!is.na(daily_dose_mg)) |>
  dplyr::group_by(hierarchical_method) |>
  dplyr::summarise(
    n           = dplyr::n(),
    pct         = round(100 * dplyr::n() / nrow(hier_df), 1),
    dose_median = round(stats::median(daily_dose_mg, na.rm = TRUE), 1),
    dose_mean   = round(mean(daily_dose_mg, na.rm = TRUE), 1),
    dose_sd     = round(stats::sd(daily_dose_mg, na.rm = TRUE), 1),
    .groups     = "drop"
  )

cat("\nDose statistics by hierarchical branch:\n")
print(as.data.frame(branch_summary), row.names = FALSE)

# Sensitivity analysis: what if we set DIFF_THRESHOLD to 10 or 20?
cat("\nSensitivity: how method choices shift with different DIFF_THRESHOLD values:\n")
threshold_sweep <- purrr::map_dfr(c(1, 5, 10, 20, 50), function(thr) {
  diff_vals <- abs(hier_df$bl_dose - hier_df$sig_dose)
  n_total   <- sum(!is.na(diff_vals))
  tibble::tibble(
    diff_threshold  = thr,
    cross_checked   = sum(!is.na(diff_vals) & diff_vals <= MATCH_TOL),
    blended         = sum(!is.na(diff_vals) & diff_vals > MATCH_TOL & diff_vals <= thr),
    nlp_override    = sum(!is.na(diff_vals) & diff_vals > thr),
    n_cross_check   = n_total
  )
})
print(as.data.frame(threshold_sweep), row.names = FALSE)

# ===========================================================================
# 8. Dose distributions (all methods + gold placeholder)
# ===========================================================================
message("\n=== Dose distributions ===")

make_dist_df <- function(episodes_df, method_label) {
  episodes_df |>
    dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
    dplyr::select(person_id, drug_name_std, median_daily_dose) |>
    dplyr::mutate(method = method_label)
}

dist_df <- dplyr::bind_rows(
  make_dist_df(hier_episodes,     "Hierarchical"),
  make_dist_df(baseline_episodes, "Baseline"),
  make_dist_df(adv_nlp_episodes,  "Advanced NLP")
)

cat("\nDose distribution summary by method:\n")
dist_df |>
  dplyr::mutate(method = factor(method,
                  levels = c("Hierarchical", "Baseline", "Advanced NLP"))) |>
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
# 9. Load gold standard
# ===========================================================================
message("\n=== Loading gold standard ===")

gold_std <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)

cat(sprintf(
  "Gold standard: %d episodes from %d patients\n",
  nrow(gold_std), dplyr::n_distinct(gold_std$patient_id)
))

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
  dplyr::slice_max(n, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(patient_id, episode_start, episode_end, drug_name_std)

gold_std <- gold_std |>
  dplyr::left_join(gold_drug_map,
                   by = c("patient_id", "episode_start", "episode_end")) |>
  convert_pred_equiv(drug_col = "drug_name_std", dose_col = "median_daily_dose",
                     out_col  = "gold_pred_equiv_mg") |>
  dplyr::mutate(
    median_daily_dose_raw = median_daily_dose,
    median_daily_dose     = dplyr::coalesce(gold_pred_equiv_mg, median_daily_dose)
  )

cat(sprintf(
  "Gold pred-equiv conversion: %d ok | %d unknown (kept raw)\n",
  sum(gold_std$pred_equiv_status == "ok", na.rm = TRUE),
  sum(gold_std$pred_equiv_status != "ok" | is.na(gold_std$pred_equiv_status), na.rm = TRUE)
))

# Add gold to distribution plot
gold_dist_df <- gold_std |>
  dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
  dplyr::transmute(person_id = as.integer(patient_id),
                   drug_name_std = dplyr::coalesce(drug_name_std, "unknown"),
                   median_daily_dose, method = "Gold")

METHOD_COLORS <- c(
  "Hierarchical" = "#CC79A7",
  "Baseline"     = "#2271B3",
  "Advanced NLP" = "#009E73",
  "Gold"         = "#333333"
)

dist_df_all <- dplyr::bind_rows(dist_df, gold_dist_df) |>
  dplyr::mutate(method = factor(method,
                  levels = c("Hierarchical", "Baseline", "Advanced NLP", "Gold")))

p_dist <- ggplot2::ggplot(
  dist_df_all,
  ggplot2::aes(x = median_daily_dose, fill = method, colour = method)
) +
  ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
  ggplot2::scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320, 640),
    labels = scales::label_number()
  ) +
  ggplot2::scale_fill_manual(values   = METHOD_COLORS) +
  ggplot2::scale_colour_manual(values = METHOD_COLORS) +
  ggplot2::facet_wrap(~ method, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title    = "Distribution of median daily prednisone-equivalent dose",
    subtitle = "One data point per patient-drug episode; x-axis log10 scale",
    x        = "Median daily dose (mg pred-equiv)",
    y        = "Density"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

print(p_dist)

# ===========================================================================
# 10. Episode-level comparisons vs gold standard
# ===========================================================================
message("\n=== Episode-level comparisons vs gold standard ===")

run_eval <- function(episodes, gold, label) {
  message(sprintf("  %s vs gold standard ...", label))
  ev <- evaluate_against_gold(episodes, gold, gold_id_col = "patient_id")
  ev$n_common_patients <- length(intersect(
    unique(episodes$person_id), unique(gold$patient_id)
  ))
  cat(sprintf(
    "\n%s: %d common patients | %d/%d gold episodes matched (%.1f%% coverage)\n",
    label,
    ev$n_common_patients,
    ev$summary$n_matched_periods,
    ev$summary$n_gold_periods,
    ev$summary$coverage_pct
  ))
  cat(sprintf("%s summary metrics:\n", label))
  print(as.data.frame(ev$summary))
  print_agreement(ev$comparison, label)
  ev
}

ev_hier     <- run_eval(hier_episodes,     gold_std, "Hierarchical")
ev_baseline <- run_eval(baseline_episodes, gold_std, "Baseline")
ev_adv      <- run_eval(adv_nlp_episodes,  gold_std, "Advanced NLP")

# ===========================================================================
# 11. Scatter + Bland-Altman plots
# ===========================================================================
message("\n=== Comparison scatter plots ===")

make_scatter_df <- function(ev, label) {
  ev$comparison |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::transmute(patient_id, gold_dose,
                     method_dose = computed_dose, method = label)
}

scatter_df <- dplyr::bind_rows(
  make_scatter_df(ev_hier,     "Hierarchical"),
  make_scatter_df(ev_baseline, "Baseline"),
  make_scatter_df(ev_adv,      "Advanced NLP")
) |>
  dplyr::mutate(method = factor(method,
                  levels = c("Hierarchical", "Baseline", "Advanced NLP")))

p_scatter <- ggplot2::ggplot(
  scatter_df, ggplot2::aes(x = gold_dose, y = method_dose)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(alpha = 0.5, size = 1.8, colour = "#2166ac") +
  ggplot2::geom_smooth(method = "lm", se = TRUE,
                       colour = "#d6604d", linewidth = 0.8) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Method dose vs gold standard",
    subtitle = "Dashed = perfect agreement",
    x        = "Gold standard median daily dose (mg pred-equiv)",
    y        = "Method median daily dose (mg pred-equiv)"
  ) +
  ggplot2::theme_bw()

print(p_scatter)

message("\n=== Bland-Altman plots ===")

ba_df <- scatter_df |>
  dplyr::filter(!is.na(gold_dose), !is.na(method_dose)) |>
  dplyr::mutate(mean_dose = (method_dose + gold_dose) / 2,
                diff      = method_dose - gold_dose)

ba_limits <- ba_df |>
  dplyr::group_by(method) |>
  dplyr::summarise(n = dplyr::n(), bias = mean(diff),
                   sd_diff = stats::sd(diff),
                   loa_lo  = bias - 1.96 * sd_diff,
                   loa_hi  = bias + 1.96 * sd_diff,
                   .groups = "drop")

cat("\nBland-Altman limits of agreement:\n")
print(as.data.frame(ba_limits |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))))

p_ba <- ggplot2::ggplot(ba_df, ggplot2::aes(x = mean_dose, y = diff)) +
  ggplot2::geom_hline(yintercept = 0,
                      linetype = "solid", colour = "grey70", linewidth = 0.5) +
  ggplot2::geom_hline(data = ba_limits,
                      ggplot2::aes(yintercept = bias),
                      linetype = "dashed", colour = "#d6604d", linewidth = 0.8) +
  ggplot2::geom_hline(data = ba_limits,
                      ggplot2::aes(yintercept = loa_lo),
                      linetype = "dotted", colour = "#4393c3", linewidth = 0.7) +
  ggplot2::geom_hline(data = ba_limits,
                      ggplot2::aes(yintercept = loa_hi),
                      linetype = "dotted", colour = "#4393c3", linewidth = 0.7) +
  ggplot2::geom_point(alpha = 0.45, size = 1.8, colour = "#2166ac") +
  ggplot2::geom_text(data = ba_limits,
                     ggplot2::aes(x = Inf, y = bias,
                                  label = sprintf("Bias: %.1f mg", bias)),
                     hjust = 1.1, vjust = -0.5, colour = "#d6604d", size = 3.2) +
  ggplot2::geom_text(data = ba_limits,
                     ggplot2::aes(x = Inf, y = loa_hi,
                                  label = sprintf("+1.96 SD: %.1f mg", loa_hi)),
                     hjust = 1.1, vjust = -0.5, colour = "#4393c3", size = 3.2) +
  ggplot2::geom_text(data = ba_limits,
                     ggplot2::aes(x = Inf, y = loa_lo,
                                  label = sprintf("-1.96 SD: %.1f mg", loa_lo)),
                     hjust = 1.1, vjust = 1.5, colour = "#4393c3", size = 3.2) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Bland-Altman: method dose minus gold standard",
    subtitle = "Red dashed = mean bias; blue dotted = 95% limits of agreement (±1.96 SD)",
    x        = "Mean of method and gold standard (mg pred-equiv)",
    y        = "Method − Gold standard (mg pred-equiv)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))

print(p_ba)

# ===========================================================================
# 12. REPORT
# ===========================================================================
message("\n\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS REPORT — SteroidDoseR Hierarchical Method\n")
cat(strrep("=", 70), "\n\n")

cat("CONFIGURATION\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  diff_threshold:   %g mg/day (|baseline - NLP| above this → NLP overrides)\n", DIFF_THRESHOLD))
cat(sprintf("  match_tol:        %g mg/day (|diff| at or below this → 'matched')\n",         MATCH_TOL))
cat(sprintf("  Study window:     %s to %s\n", START_DATE, END_DATE))
cat(sprintf("  Episode gap:      %d days\n",  GAP_DAYS))
cat(sprintf("  Concurrent agg:   %s\n\n",     CONCURRENT_AGG))

cat("DATA OVERVIEW\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Drug-exposure records:  %d\n",   nrow(drug_df)))
cat(sprintf("  Unique patients:        %d\n\n", dplyr::n_distinct(drug_df$person_id)))

cat("PATIENT OVERLAP\n")
cat(strrep("-", 40), "\n")
db_pts      <- unique(as.integer(drug_df$person_id))
gold_pts    <- unique(as.integer(gold_std$patient_id))
overlap_pts <- intersect(db_pts, gold_pts)
cat(sprintf("  Database (drug_exposure): %d unique patients\n", length(db_pts)))
cat(sprintf("  Gold standard:            %d unique patients\n", length(gold_pts)))
cat(sprintf("  Overlapping patients:     %d\n\n",              length(overlap_pts)))

cat("HIERARCHICAL METHOD — BRANCH BREAKDOWN\n")
cat(strrep("-", 40), "\n")
branch_desc <- c(
  cross_checked      = "Both estimates matched (|diff| <= match_tol) → NLP value used",
  blended            = "Small discrepancy → average of baseline and NLP",
  nlp_override       = "Large discrepancy (|diff| > diff_threshold) → NLP value",
  baseline_only      = "SIG absent or unparseable → baseline value",
  nlp_fills_baseline = "Baseline incomplete, SIG complete (steady dosing) → NLP",
  nlp_taper          = "Baseline incomplete, SIG is taper → NLP taper",
  missing            = "Neither baseline nor SIG could compute a dose → NA",
  implausible        = "Dose > max_daily_dose_mg cap → NA"
)
for (m in method_tbl$hierarchical_method) {
  row <- method_tbl[method_tbl$hierarchical_method == m, ]
  desc <- if (m %in% names(branch_desc)) branch_desc[[m]] else ""
  cat(sprintf("  %-22s %5d (%5.1f%%)  %s\n", m, row$n, row$pct, desc))
}
cat("\n")

cat("EPISODE COUNTS\n")
cat(strrep("-", 40), "\n")
episode_counts <- tibble::tibble(
  Source               = c("Hierarchical", "Baseline", "Advanced NLP", "Gold Standard"),
  Patients             = c(dplyr::n_distinct(hier_episodes$person_id),
                            dplyr::n_distinct(baseline_episodes$person_id),
                            dplyr::n_distinct(adv_nlp_episodes$person_id),
                            dplyr::n_distinct(gold_std$patient_id)),
  Episodes             = c(nrow(hier_episodes), nrow(baseline_episodes),
                            nrow(adv_nlp_episodes), nrow(gold_std)),
  Matched_to_Gold      = c(ev_hier$summary$n_matched_periods,
                            ev_baseline$summary$n_matched_periods,
                            ev_adv$summary$n_matched_periods,
                            NA_integer_),
  Coverage_pct         = c(round(ev_hier$summary$coverage_pct,     1),
                            round(ev_baseline$summary$coverage_pct, 1),
                            round(ev_adv$summary$coverage_pct,      1),
                            NA_real_),
  Median_dose_mg       = c(stats::median(hier_episodes$median_daily_dose,     na.rm = TRUE),
                            stats::median(baseline_episodes$median_daily_dose, na.rm = TRUE),
                            stats::median(adv_nlp_episodes$median_daily_dose,  na.rm = TRUE),
                            stats::median(gold_std$median_daily_dose,          na.rm = TRUE))
)
print(as.data.frame(episode_counts), row.names = FALSE)
cat("\n")

cat("EPISODE-LEVEL DOSE AGREEMENT (day-level)\n")
cat(strrep("-", 40), "\n")
message("  Computing dose agreement — Hierarchical ...")
agr_hier <- compute_episode_dose_agreement(
  hier_episodes, gold_std,
  threshold_mg = DOSE_THRESHOLD_MG, threshold_pct = DOSE_THRESHOLD_PCT,
  gold_id_col  = "patient_id"
)
message("  Computing dose agreement — Baseline ...")
agr_baseline <- compute_episode_dose_agreement(
  baseline_episodes, gold_std,
  threshold_mg = DOSE_THRESHOLD_MG, threshold_pct = DOSE_THRESHOLD_PCT,
  gold_id_col  = "patient_id"
)
message("  Computing dose agreement — Advanced NLP ...")
agr_adv <- compute_episode_dose_agreement(
  adv_nlp_episodes, gold_std,
  threshold_mg = DOSE_THRESHOLD_MG, threshold_pct = DOSE_THRESHOLD_PCT,
  gold_id_col  = "patient_id"
)

agreement_tbl <- dplyr::bind_rows(
  dplyr::mutate(agr_hier,     Method = "Hierarchical", .before = 1),
  dplyr::mutate(agr_baseline, Method = "Baseline",     .before = 1),
  dplyr::mutate(agr_adv,      Method = "Advanced NLP", .before = 1)
)
print(as.data.frame(agreement_tbl), row.names = FALSE)
cat("\n")

cat("ACCURACY vs GOLD STANDARD (episode-level, median dose)\n")
cat(strrep("-", 40), "\n")
metrics_tbl <- tibble::tibble(
  Method          = c("Hierarchical", "Baseline", "Advanced NLP"),
  Common_Patients = c(ev_hier$n_common_patients,
                       ev_baseline$n_common_patients,
                       ev_adv$n_common_patients),
  Coverage_pct    = round(c(ev_hier$summary$coverage_pct,
                             ev_baseline$summary$coverage_pct,
                             ev_adv$summary$coverage_pct), 1),
  MAE_mg          = round(c(ev_hier$summary$MAE,
                             ev_baseline$summary$MAE,
                             ev_adv$summary$MAE), 2),
  MBE_mg          = round(c(ev_hier$summary$MBE,
                             ev_baseline$summary$MBE,
                             ev_adv$summary$MBE), 2),
  RMSE_mg         = round(c(ev_hier$summary$RMSE,
                             ev_baseline$summary$RMSE,
                             ev_adv$summary$RMSE), 2),
  MAPE_pct        = round(c(ev_hier$summary$MAPE,
                             ev_baseline$summary$MAPE,
                             ev_adv$summary$MAPE), 1),
  Pearson_r       = round(c(ev_hier$summary$pearson_corr,
                             ev_baseline$summary$pearson_corr,
                             ev_adv$summary$pearson_corr), 3),
  Spearman_rho    = round(c(ev_hier$summary$spearman_corr,
                             ev_baseline$summary$spearman_corr,
                             ev_adv$summary$spearman_corr), 3)
)
print(as.data.frame(metrics_tbl), row.names = FALSE)

cat("\nINTERPRETATION\n")
cat(strrep("-", 40), "\n")

best_mae_idx  <- which.min(metrics_tbl$MAE_mg)
best_cov_idx  <- which.max(metrics_tbl$Coverage_pct)
hier_idx      <- which(metrics_tbl$Method == "Hierarchical")

cat(sprintf(
  paste0(
    "Coverage: %s achieves highest coverage (%.1f%%).\n",
    "  Hierarchical coverage = %.1f%%.\n\n"
  ),
  metrics_tbl$Method[best_cov_idx], metrics_tbl$Coverage_pct[best_cov_idx],
  metrics_tbl$Coverage_pct[hier_idx]
))

cat(sprintf(
  paste0(
    "Accuracy (MAE): %s has the lowest MAE (%.2f mg).\n",
    "  Hierarchical MAE = %.2f mg",
    " (%s vs best-performing single method).\n\n"
  ),
  metrics_tbl$Method[best_mae_idx],
  metrics_tbl$MAE_mg[best_mae_idx],
  metrics_tbl$MAE_mg[hier_idx],
  if (hier_idx == best_mae_idx) "IS the best" else "comparison available"
))

# Branch contributions
n_cross  <- sum(method_tbl$n[method_tbl$hierarchical_method %in%
                               c("cross_checked", "blended", "nlp_override")], na.rm = TRUE)
n_bl_onl <- sum(method_tbl$n[method_tbl$hierarchical_method == "baseline_only"],    na.rm = TRUE)
n_nlp_f  <- sum(method_tbl$n[method_tbl$hierarchical_method %in%
                               c("nlp_fills_baseline", "nlp_taper")],               na.rm = TRUE)
n_miss   <- sum(method_tbl$n[method_tbl$hierarchical_method == "missing"],          na.rm = TRUE)
n_all    <- nrow(hier_df)

cat(sprintf(paste0(
  "Hierarchical branch contribution:\n",
  "  Cross-checked (both sources):   %d records (%4.1f%%)\n",
  "  Baseline-only (no SIG):         %d records (%4.1f%%)\n",
  "  NLP fills baseline:             %d records (%4.1f%%)\n",
  "  Missing (both failed):          %d records (%4.1f%%)\n\n"
),
n_cross,  100 * n_cross  / n_all,
n_bl_onl, 100 * n_bl_onl / n_all,
n_nlp_f,  100 * n_nlp_f  / n_all,
n_miss,   100 * n_miss   / n_all
))

cat(sprintf(
  paste0(
    "Recommendation: The hierarchical method is most appropriate when both\n",
    "  structured OMOP fields (quantity/days_supply) and SIG text are available\n",
    "  for some records. It achieves better accuracy than pure baseline on records\n",
    "  where the SIG carries dose information not captured in structured fields,\n",
    "  while retaining baseline's robustness for records with missing SIG text.\n",
    "  The diff_threshold parameter (currently %g mg/day) can be tuned based on\n",
    "  the expected clinical precision requirements of your study.\n"
  ),
  DIFF_THRESHOLD
))

cat(strrep("=", 70), "\n")

# ===========================================================================
# 13. Save results to a timestamped run folder
# ===========================================================================
message("\n=== Saving results ===")

# Each run gets its own subfolder so previous results are never overwritten.
RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

# ── params.txt — human-readable record of every configuration value ──────────
writeLines(c(
  "SteroidDoseR — Run Parameters",
  strrep("=", 40),
  sprintf("Script:              CodeToRun_hierarchical.R"),
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
  "Hierarchical method",
  strrep("-", 40),
  sprintf("DIFF_THRESHOLD:      %g mg/day", DIFF_THRESHOLD),
  sprintf("MATCH_TOL:           %g mg/day", MATCH_TOL),
  "",
  "Episode building",
  strrep("-", 40),
  sprintf("GAP_DAYS:            %d", GAP_DAYS),
  sprintf("CONCURRENT_AGG:      %s", CONCURRENT_AGG),
  "",
  "Evaluation",
  strrep("-", 40),
  sprintf("DOSE_THRESHOLD_MG:   %s",
          if (is.null(DOSE_THRESHOLD_MG))  "NULL (disabled)"
          else as.character(DOSE_THRESHOLD_MG)),
  sprintf("DOSE_THRESHOLD_PCT:  %s",
          if (is.null(DOSE_THRESHOLD_PCT)) "NULL (disabled)"
          else as.character(DOSE_THRESHOLD_PCT)),
  sprintf("GOLD_STD_PATH:       %s", GOLD_STD_PATH)
), con = file.path(RUN_DIR, "params.txt"))

# ── Record-level dose data ────────────────────────────────────────────────────
readr::write_csv(hier_df,                file.path(RUN_DIR, "records_hierarchical.csv"))
readr::write_csv(baseline_df,            file.path(RUN_DIR, "records_baseline.csv"))
readr::write_csv(adv_nlp_df_eq,          file.path(RUN_DIR, "records_adv_nlp.csv"))

# ── Episode-level summaries ───────────────────────────────────────────────────
readr::write_csv(hier_episodes,          file.path(RUN_DIR, "episodes_hierarchical.csv"))
readr::write_csv(baseline_episodes,      file.path(RUN_DIR, "episodes_baseline.csv"))
readr::write_csv(adv_nlp_episodes,       file.path(RUN_DIR, "episodes_adv_nlp.csv"))

# ── Gold standard ─────────────────────────────────────────────────────────────
readr::write_csv(gold_std,               file.path(RUN_DIR, "gold_standard.csv"))

# ── Evaluation comparison tables ─────────────────────────────────────────────
readr::write_csv(ev_hier$comparison,     file.path(RUN_DIR, "comparison_hierarchical.csv"))
readr::write_csv(ev_baseline$comparison, file.path(RUN_DIR, "comparison_baseline.csv"))
readr::write_csv(ev_adv$comparison,      file.path(RUN_DIR, "comparison_adv_nlp.csv"))

# ── Summary tables ────────────────────────────────────────────────────────────
readr::write_csv(episode_counts,         file.path(RUN_DIR, "episode_counts.csv"))
readr::write_csv(metrics_tbl,            file.path(RUN_DIR, "metrics_table.csv"))
readr::write_csv(agreement_tbl,          file.path(RUN_DIR, "agreement_table.csv"))
readr::write_csv(branch_summary,         file.path(RUN_DIR, "branch_summary.csv"))
readr::write_csv(threshold_sweep,        file.path(RUN_DIR, "threshold_sensitivity.csv"))

# ── Plot data (for reproducing figures without re-running) ────────────────────
readr::write_csv(dist_df_all,            file.path(RUN_DIR, "plot_data_distribution.csv"))
readr::write_csv(scatter_df,             file.path(RUN_DIR, "plot_data_scatter.csv"))
readr::write_csv(ba_df,                  file.path(RUN_DIR, "plot_data_bland_altman.csv"))
readr::write_csv(ba_limits,              file.path(RUN_DIR, "bland_altman_limits.csv"))

# ── Figures ───────────────────────────────────────────────────────────────────
ggplot2::ggsave(file.path(RUN_DIR, "plot_distribution.png"),  p_dist,    width = 8,  height = 10, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter.png"),       p_scatter, width = 10, height = 5,  dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_bland_altman.png"),  p_ba,      width = 10, height = 5,  dpi = 150)

message(sprintf("Results saved to: %s", RUN_DIR))

# ===========================================================================
# 14. Interactive dose review dashboard
# ===========================================================================
message("\n=== Launching interactive dose review dashboard ===")
message("(Close the browser tab or press Escape in R to stop.)")

hier_df_dash <- hier_df_eq   # pred_equiv_mg already present

baseline_eq <- convert_pred_equiv(
  baseline_df, drug_col = "drug_name_std", dose_col = "daily_dose_mg_imputed"
)

launch_dose_dashboard(
  episode_list = list(
    "Hierarchical" = hier_episodes,
    "Baseline"     = baseline_episodes,
    "Advanced NLP" = adv_nlp_episodes
  ),
  raw_list = list(
    "Hierarchical" = hier_df_dash,
    "Baseline"     = baseline_eq,
    "Advanced NLP" = adv_nlp_df_eq
  ),
  gold_std = gold_std
)

# ===========================================================================
# 15. Disconnect (live DB only)
# ===========================================================================
if (!USE_SYNTHETIC) {
  if (inherits(conn, "DatabaseConnectorConnection")) {
    DatabaseConnector::disconnect(conn)
  } else {
    DBI::dbDisconnect(conn)
  }
}

message("\n=== Analysis complete ===")
