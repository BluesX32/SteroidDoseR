# error_decomposition.R
# Three-tier error decomposition for the SteroidDoseR hierarchical method.
#
# Tier 1 — SIG type stratification
#   Classify each record's SIG text as: steady / taper / escalation / PRN /
#   ambiguous / conflicting / no_sig — then compute MAE, RMSE, MAPE, and
#   agreement rates for each type. If taper/PRN/ambiguous dominate the error,
#   that branch needs separate treatment rather than a single global algorithm.
#
# Tier 2 — Data-source / hierarchical-branch stratification
#   Show which branch of the decision tree (cross_checked, blended,
#   adaptive_override, baseline_only, nlp_fills_baseline, nlp_taper, missing)
#   contributes most to overall MAE. Helps focus improvement effort on the
#   weakest branch rather than global parameter tuning.
#
# Tier 3 — Episode-level error type labeling
#   For each matched gold episode, flag the specific failure mode:
#   start_date_wrong | end_date_wrong | duration_wrong | daily_dose_wrong |
#   total_dose_wrong | overlapping_records | short_gold_episode |
#   large_mape | dose_overestimated | dose_underestimated
#
# Run modes
# ---------
#   RESULTS_DIR = "path/to/output_hierarchical/YYYY-MM-DD_HH-MM-SS"
#     → load saved CSVs from a previous CodeToRun_hierarchical.R run (fast)
#   RESULTS_DIR = NULL
#     → recompute from scratch (same data-loading boilerplate as
#       CodeToRun_hierarchical.R)
#
# Usage
# -----
#   Source interactively in RStudio, or:
#     Rscript error_decomposition.R

devtools::install_local(getwd())
if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)
library(ggplot2)
library(tidyr)

# ===========================================================================
# 0. Configuration
# ===========================================================================

# Set to the output folder of a previous CodeToRun_hierarchical.R run to skip
# recomputation. Set to NULL to load raw data and recompute from scratch.
RESULTS_DIR <- NULL   # e.g., "output_hierarchical/2025-05-24_14-32-05"

# ── Only used when RESULTS_DIR = NULL ────────────────────────────────────────
USE_SYNTHETIC  <- FALSE
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
DIFF_THRESHOLD <- 5      # mg/day — used if recomputing hierarchical
MATCH_TOL      <- 0.01   # mg/day
GAP_DAYS       <- 30L
CONCURRENT_AGG <- "per_drug"

GOLD_STD_PATH  <- "/your/path/to/gold-standard"
COHORT_PERSON_IDS <- NULL

STEROID_CONCEPT_IDS <- as.integer(readr::read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

# ── Error-type thresholds ─────────────────────────────────────────────────────
DATE_TOL_DAYS       <- 7L    # start/end date error larger than this is flagged
DURATION_TOL_DAYS   <- 14L   # duration error larger than this is flagged
DOSE_TOL_MG         <- 5     # daily dose error (mg) larger than this is flagged
TOTAL_DOSE_TOL_MG   <- 100   # total-dose error (mg) larger than this is flagged
SHORT_EPISODE_DAYS  <- 7L    # gold episodes at or shorter than this inflate MAPE
CONFLICT_THR_MG     <- 20    # |bl_dose - sig_dose| above this → "conflicting" SIG type

OUTPUT_DIR <- file.path(getwd(), "output_error_decomposition")

# ===========================================================================
# 1. Load data
# ===========================================================================

if (!is.null(RESULTS_DIR)) {
  # ── Mode A: load saved results ──────────────────────────────────────────────
  message(sprintf("=== Loading saved results from: %s ===", RESULTS_DIR))
  hier_df       <- readr::read_csv(file.path(RESULTS_DIR, "records_hierarchical.csv"),
                                   show_col_types = FALSE)
  hier_episodes <- readr::read_csv(file.path(RESULTS_DIR, "episodes_hierarchical.csv"),
                                   show_col_types = FALSE)
  gold_std      <- readr::read_csv(file.path(RESULTS_DIR, "gold_standard.csv"),
                                   show_col_types = FALSE)

  # Restore date types
  hier_df$drug_exposure_start_date  <- as.Date(hier_df$drug_exposure_start_date)
  hier_df$drug_exposure_end_date    <- as.Date(hier_df$drug_exposure_end_date)
  hier_episodes$episode_start        <- as.Date(hier_episodes$episode_start)
  hier_episodes$episode_end          <- as.Date(hier_episodes$episode_end)
  gold_std$episode_start             <- as.Date(gold_std$episode_start)
  gold_std$episode_end               <- as.Date(gold_std$episode_end)

  message(sprintf(
    "  Loaded: %d records | %d episodes | %d gold episodes",
    nrow(hier_df), nrow(hier_episodes), nrow(gold_std)
  ))

} else {
  # ── Mode B: recompute from scratch ──────────────────────────────────────────
  # Connection boilerplate
  if (!USE_SYNTHETIC) {

    # ── Option B: DatabaseConnector
    # cdm_schema   <- "database.dbo"
    # vocab_schema <- "database.dbo"
    # connectionDetails <- DatabaseConnector::createConnectionDetails(...)
    # conn <- DatabaseConnector::connect(connectionDetails)

    # ── Option C: DBI / odbc
    # library(DBI); library(odbc)
    # DB_DIALECT <- "spark"
    # ...
    # conn <- DBI::dbConnect(...)
  }

  read_pkg_sql <- function(filename) {
    path <- system.file("sql", filename, package = "SteroidDoseR")
    if (nchar(path) == 0L) path <- file.path("inst", "sql", filename)
    if (!file.exists(path)) stop("SQL file not found: ", filename)
    SqlRender::readSql(path)
  }

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

  drug_df <- drug_df |>
    dplyr::mutate(drug_name_std = standardize_drug_name(drug_concept_name))
  if (!"drug_exposure_id" %in% names(drug_df)) {
    drug_df <- drug_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())
  }

  message("=== Running hierarchical method ===")
  hier_df <- calc_daily_dose_hierarchical(
    drug_df,
    diff_threshold    = DIFF_THRESHOLD,
    match_tol         = MATCH_TOL,
    max_daily_dose_mg = 2000,
    filter_oral       = TRUE
  )

  hier_eq <- convert_pred_equiv(hier_df,
                                drug_col = "drug_name_std",
                                dose_col = "daily_dose_mg")

  hier_episodes <- build_episodes(
    hier_eq,
    end_col        = "drug_exposure_end_date",
    dose_col       = "pred_equiv_mg",
    gap_days       = GAP_DAYS,
    concurrent_agg = CONCURRENT_AGG
  )

  message("=== Loading gold standard ===")
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
      by = "patient_id", relationship = "many-to-many"
    ) |>
    dplyr::filter(
      drug_exposure_start_date <= episode_end,
      drug_exposure_end_date   >= episode_start,
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
      median_daily_dose = dplyr::coalesce(gold_pred_equiv_mg, median_daily_dose)
    )

  if (!USE_SYNTHETIC && exists("conn")) {
    if (inherits(conn, "DatabaseConnectorConnection")) DatabaseConnector::disconnect(conn)
    else DBI::dbDisconnect(conn)
  }
}

# ===========================================================================
# Helper functions
# ===========================================================================

mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[[1L]]
}

strat_metrics <- function(df, group_col) {
  df |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(
      n_episodes   = dplyr::n(),
      coverage_pct = 100 * dplyr::n() / sum(!is.na(computed_dose) | is.na(computed_dose)),
      MAE          = round(mean(absolute_error,              na.rm = TRUE), 2),
      RMSE         = round(sqrt(mean(bias_error^2,           na.rm = TRUE)), 2),
      MBE          = round(mean(bias_error,                  na.rm = TRUE), 2),
      MAPE         = round(mean(absolute_relative_error_pct, na.rm = TRUE), 1),
      pct_exact    = round(100 * mean(agreement_category == "Exact (<=5%)",    na.rm = TRUE), 1),
      pct_good     = round(100 * mean(agreement_category == "Good (<=20%)",    na.rm = TRUE), 1),
      pct_poor     = round(100 * mean(agreement_category == "Poor (>50%)",     na.rm = TRUE), 1),
      .groups = "drop"
    )
}

classify_sig_type <- function(sig_text, sig_status, sig_taper_flag,
                               bl_dose, sig_dose, conflict_thr) {
  taper_flag <- !is.na(sig_taper_flag) & sig_taper_flag
  both_avail <- !is.na(bl_dose) & !is.na(sig_dose)
  txt_lc     <- tolower(ifelse(is.na(sig_text), "", sig_text))

  dplyr::case_when(
    # Taper: explicit flag or taper status from parser
    taper_flag | (!is.na(sig_status) & sig_status %in% c("taper", "taper_ok"))
      ~ "taper",
    # Conflicting: both sources present but large discrepancy
    both_avail & abs(bl_dose - sig_dose) > conflict_thr
      ~ "conflicting",
    # PRN: status or text
    (!is.na(sig_status) & sig_status == "prn") |
      grepl("\\bas needed\\b|\\bprn\\b", txt_lc)
      ~ "prn",
    # Escalation: increase/escalate keywords without taper/decrease keywords
    grepl("\\bincrease|\\bescalate|\\btitrate|\\bump\\s+to|\\bump\\s+dose", txt_lc) &
      !grepl("\\btaper|\\bdecrease|\\breduce|\\bwean", txt_lc)
      ~ "escalation",
    # No SIG available
    is.na(sig_text) | sig_text == ""
      ~ "no_sig",
    # Ambiguous / unparseable SIG
    !is.na(sig_status) & sig_status %in% c("no_parse", "free_text", "empty")
      ~ "ambiguous",
    # Steady / simple
    !is.na(sig_status) & sig_status == "ok"
      ~ "steady",
    TRUE ~ "ambiguous"
  )
}

detect_overlapping_records <- function(starts, ends) {
  n <- length(starts)
  if (n < 2L) return(FALSE)
  ord    <- order(starts)
  starts <- starts[ord]
  ends   <- ends[ord]
  any(ends[-n] >= starts[-1L], na.rm = TRUE)
}

# ===========================================================================
# 2. SIG type classification at record level
# ===========================================================================
message("\n=== Tier 1: SIG type classification ===")

hier_df <- hier_df |>
  dplyr::mutate(
    sig_type = classify_sig_type(
      sig_text       = sig,
      sig_status     = sig_status,
      sig_taper_flag = sig_taper_flag,
      bl_dose        = bl_dose,
      sig_dose       = sig_dose,
      conflict_thr   = CONFLICT_THR_MG
    )
  )

sig_type_record_tbl <- hier_df |>
  dplyr::count(sig_type, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))

cat("\nSIG type breakdown (record level):\n")
print(as.data.frame(sig_type_record_tbl), row.names = FALSE)

# ===========================================================================
# 3. Propagate record-level metadata to episode level
# ===========================================================================
message("\n=== Propagating record metadata to episodes ===")

# Link each drug-exposure record to the episode it belongs to
record_to_ep <- hier_df |>
  dplyr::mutate(pt_id_int = as.integer(person_id)) |>
  dplyr::inner_join(
    hier_episodes |>
      dplyr::mutate(pt_id_int = as.integer(person_id)) |>
      dplyr::select(pt_id_int, drug_name_std, episode_start, episode_end),
    by       = c("pt_id_int", "drug_name_std"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    drug_exposure_start_date <= episode_end,
    drug_exposure_end_date   >= episode_start
  )

# Aggregate to one row per episode
episode_meta <- record_to_ep |>
  dplyr::group_by(pt_id_int, drug_name_std, episode_start, episode_end) |>
  dplyr::summarise(
    n_records              = dplyr::n(),
    sig_type               = mode_chr(sig_type),
    hier_branch            = mode_chr(hierarchical_method),
    has_taper_record       = any(sig_taper_flag == TRUE, na.rm = TRUE),
    has_prn_record         = any(sig_status == "prn", na.rm = TRUE),
    has_conflicting_record = any(sig_type == "conflicting", na.rm = TRUE),
    any_missing_dose       = any(hierarchical_method == "missing", na.rm = TRUE),
    bl_dose_mean           = mean(bl_dose,  na.rm = TRUE),
    sig_dose_mean          = mean(sig_dose, na.rm = TRUE),
    has_overlapping_records = detect_overlapping_records(
      drug_exposure_start_date, drug_exposure_end_date
    ),
    .groups = "drop"
  ) |>
  dplyr::rename(person_id = pt_id_int)

cat(sprintf(
  "  %d records linked to %d episodes (%.1f records/episode avg)\n",
  nrow(record_to_ep),
  nrow(episode_meta),
  nrow(record_to_ep) / nrow(episode_meta)
))

# ===========================================================================
# 4. Standard evaluation via evaluate_against_gold
# ===========================================================================
message("\n=== Running evaluate_against_gold ===")

ev <- evaluate_against_gold(hier_episodes, gold_std, gold_id_col = "patient_id")

cat(sprintf(
  "Overall: %d gold episodes | %d matched (%.1f%% coverage) | MAE = %.2f mg\n",
  ev$summary$n_gold_periods,
  ev$summary$n_matched_periods,
  ev$summary$coverage_pct,
  ev$summary$MAE
))

# Attach episode metadata to the comparison table
# ev$comparison has: patient_id, episode_start, episode_end (gold dates), ...
comparison <- ev$comparison |>
  dplyr::left_join(
    episode_meta |>
      dplyr::mutate(patient_id = as.integer(person_id)) |>
      dplyr::select(patient_id, episode_start, episode_end,
                    n_records, sig_type, hier_branch,
                    has_taper_record, has_prn_record,
                    has_conflicting_record, any_missing_dose,
                    has_overlapping_records),
    by = c("patient_id", "episode_start", "episode_end")
  )

# ===========================================================================
# 5. Episode-level date and dose error decomposition
# ===========================================================================
# evaluate_against_gold does not return computed episode dates, so we do our
# own overlap join here to get comp_start / comp_end for date error analysis.
message("\n=== Tier 3: Episode-level error decomposition ===")

episode_join <- hier_episodes |>
  dplyr::mutate(pt_id_int = as.integer(person_id)) |>
  dplyr::inner_join(
    gold_std |>
      dplyr::transmute(
        pt_id_int = as.integer(patient_id),
        g_start   = episode_start,
        g_end     = episode_end,
        gold_dose = median_daily_dose
      ),
    by = c("pt_id_int"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(episode_start <= g_end, episode_end >= g_start) |>
  dplyr::mutate(
    overlap_days = as.integer(pmin(episode_end, g_end) - pmax(episode_start, g_start)) + 1L
  ) |>
  dplyr::group_by(pt_id_int, g_start, g_end) |>
  dplyr::slice_max(overlap_days, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    gold_duration_days = as.integer(g_end - g_start) + 1L,
    comp_duration_days = as.integer(episode_end - episode_start) + 1L,
    start_date_err_days = as.integer(abs(episode_start - g_start)),
    end_date_err_days   = as.integer(abs(episode_end   - g_end)),
    duration_err_days   = abs(comp_duration_days - gold_duration_days),
    dose_err_mg         = abs(median_daily_dose - gold_dose),
    total_dose_comp     = median_daily_dose * comp_duration_days,
    total_dose_gold     = gold_dose * gold_duration_days,
    total_dose_err_mg   = abs(total_dose_comp - total_dose_gold),

    # Error type flags
    err_start_date         = !is.na(start_date_err_days) & start_date_err_days > DATE_TOL_DAYS,
    err_end_date           = !is.na(end_date_err_days)   & end_date_err_days   > DATE_TOL_DAYS,
    err_duration           = !is.na(duration_err_days)   & duration_err_days   > DURATION_TOL_DAYS,
    err_daily_dose         = !is.na(dose_err_mg)         & dose_err_mg         > DOSE_TOL_MG,
    err_total_dose         = !is.na(total_dose_err_mg)   & total_dose_err_mg   > TOTAL_DOSE_TOL_MG,
    err_short_gold         = gold_duration_days <= SHORT_EPISODE_DAYS,
    err_large_mape         = !is.na(dose_err_mg) & !is.na(gold_dose) & gold_dose > 0 &
                             (dose_err_mg / gold_dose) > 0.5,
    err_dose_overestimated = !is.na(median_daily_dose) & !is.na(gold_dose) &
                             median_daily_dose > gold_dose + DOSE_TOL_MG,
    err_dose_underestimated = !is.na(median_daily_dose) & !is.na(gold_dose) &
                              median_daily_dose < gold_dose - DOSE_TOL_MG,
    err_overlapping_records = FALSE  # filled in below after join
  ) |>
  dplyr::rename(
    patient_id  = pt_id_int,
    comp_start  = episode_start,
    comp_end    = episode_end,
    comp_dose   = median_daily_dose
  )

# Fill in the overlapping-records flag from episode_meta
episode_join <- episode_join |>
  dplyr::left_join(
    episode_meta |>
      dplyr::mutate(patient_id = as.integer(person_id)) |>
      dplyr::select(patient_id, episode_start = episode_start, episode_end = episode_end,
                    has_overlapping_records, has_taper_record, n_records,
                    sig_type, hier_branch),
    by = c("patient_id", "comp_start" = "episode_start", "comp_end" = "episode_end")
  ) |>
  dplyr::mutate(
    err_overlapping_records = !is.na(has_overlapping_records) & has_overlapping_records,
    err_taper_collapsed     = !is.na(has_taper_record) & has_taper_record & err_duration
  )

# Gold episodes with NO computed match
gold_unmatched <- gold_std |>
  dplyr::anti_join(
    episode_join |>
      dplyr::transmute(patient_id = as.integer(patient_id), g_start, g_end),
    by = c("patient_id" = "patient_id",
           "episode_start" = "g_start",
           "episode_end"   = "g_end")
  ) |>
  dplyr::mutate(
    err_missed = TRUE,
    gold_duration_days = as.integer(episode_end - episode_start) + 1L
  )

cat(sprintf(
  "  Matched gold episodes: %d | Unmatched (missed): %d\n",
  nrow(episode_join), nrow(gold_unmatched)
))

# ===========================================================================
# 6. Error type summary table
# ===========================================================================

error_type_cols <- c(
  "err_start_date", "err_end_date", "err_duration",
  "err_daily_dose", "err_total_dose", "err_short_gold", "err_large_mape",
  "err_dose_overestimated", "err_dose_underestimated",
  "err_overlapping_records", "err_taper_collapsed"
)

error_type_summary <- purrr::map_dfr(error_type_cols, function(col) {
  vals <- episode_join[[col]]
  tibble::tibble(
    error_type = col,
    n_flagged  = sum(vals == TRUE, na.rm = TRUE),
    pct_of_matched = round(100 * mean(vals == TRUE, na.rm = TRUE), 1)
  )
}) |>
  dplyr::bind_rows(
    tibble::tibble(
      error_type    = "err_missed",
      n_flagged     = nrow(gold_unmatched),
      pct_of_matched = round(100 * nrow(gold_unmatched) /
                               (nrow(episode_join) + nrow(gold_unmatched)), 1)
    )
  ) |>
  dplyr::arrange(dplyr::desc(n_flagged))

cat("\nError type frequency (matched episodes):\n")
print(as.data.frame(error_type_summary), row.names = FALSE)

# ===========================================================================
# TIER 1: Stratified metrics by SIG type
# ===========================================================================
message("\n=== Tier 1 results: metrics by SIG type ===")

strat_sig <- strat_metrics(comparison, "sig_type") |>
  dplyr::arrange(dplyr::desc(MAE))

cat("\nMAE by SIG type:\n")
print(as.data.frame(strat_sig), row.names = FALSE)

# Also: SIG type among UNMATCHED (missed) gold episodes — what drove the miss?
missed_sig <- episode_join |>
  dplyr::filter(is.na(comp_dose)) |>
  dplyr::count(sig_type) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1), group = "missed")

if (nrow(missed_sig) > 0L) {
  cat("\nSIG type breakdown among missed (unmatched) episodes:\n")
  print(as.data.frame(missed_sig), row.names = FALSE)
}

# ===========================================================================
# TIER 2: Stratified metrics by hierarchical branch
# ===========================================================================
message("\n=== Tier 2 results: metrics by hierarchical branch ===")

strat_branch <- strat_metrics(comparison, "hier_branch") |>
  dplyr::arrange(dplyr::desc(MAE))

cat("\nMAE by hierarchical branch:\n")
print(as.data.frame(strat_branch), row.names = FALSE)

# Which branch has the worst % poor agreement?
worst_branch <- strat_branch |>
  dplyr::filter(!is.na(hier_branch)) |>
  dplyr::slice_max(pct_poor, n = 1L)

if (nrow(worst_branch) > 0L) {
  cat(sprintf(
    "\n>>> Worst branch: '%s' — %.1f%% poor agreement, MAE = %.2f mg (%d episodes)\n",
    worst_branch$hier_branch, worst_branch$pct_poor,
    worst_branch$MAE, worst_branch$n_episodes
  ))
}

# ===========================================================================
# TIER 3: Episode-level error type × SIG type cross-tabulation
# ===========================================================================
message("\n=== Tier 3: Error type × SIG type cross-tab ===")

cross_tab <- episode_join |>
  dplyr::filter(!is.na(sig_type)) |>
  tidyr::pivot_longer(
    cols      = dplyr::all_of(error_type_cols),
    names_to  = "error_type",
    values_to = "flagged"
  ) |>
  dplyr::filter(flagged == TRUE) |>
  dplyr::count(sig_type, error_type) |>
  tidyr::pivot_wider(names_from = error_type, values_from = n, values_fill = 0L)

cat("\nError count by SIG type (rows) × error type (columns):\n")
print(as.data.frame(cross_tab), row.names = FALSE)

# Error type × hierarchical branch
cross_tab_branch <- episode_join |>
  dplyr::filter(!is.na(hier_branch)) |>
  tidyr::pivot_longer(
    cols      = dplyr::all_of(error_type_cols),
    names_to  = "error_type",
    values_to = "flagged"
  ) |>
  dplyr::filter(flagged == TRUE) |>
  dplyr::count(hier_branch, error_type) |>
  tidyr::pivot_wider(names_from = error_type, values_from = n, values_fill = 0L)

cat("\nError count by hierarchical branch (rows) × error type (columns):\n")
print(as.data.frame(cross_tab_branch), row.names = FALSE)

# ===========================================================================
# 7. Visualizations
# ===========================================================================
message("\n=== Generating plots ===")

SIG_COLORS <- c(
  steady      = "#2271B3",
  taper       = "#E69F00",
  escalation  = "#CC79A7",
  prn         = "#009E73",
  ambiguous   = "#999999",
  conflicting = "#D55E00",
  no_sig      = "#BBBBBB"
)

# ── Plot 1: MAE by SIG type ──────────────────────────────────────────────────
p_mae_sig <- strat_sig |>
  dplyr::filter(!is.na(sig_type)) |>
  dplyr::mutate(sig_type = forcats::fct_reorder(sig_type, MAE, .desc = TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x = sig_type, y = MAE, fill = sig_type)) +
  ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("n=%d\n%.1f mg", n_episodes, MAE)),
                     vjust = -0.3, size = 3) +
  ggplot2::scale_fill_manual(values = SIG_COLORS, na.value = "#BBBBBB") +
  ggplot2::labs(
    title    = "MAE by SIG type",
    subtitle = "Identifies which SIG categories drive the most dose error",
    x        = "SIG type",
    y        = "MAE (mg pred-equiv)"
  ) +
  ggplot2::theme_bw()

print(p_mae_sig)

# ── Plot 2: MAE by hierarchical branch ───────────────────────────────────────
p_mae_branch <- strat_branch |>
  dplyr::filter(!is.na(hier_branch)) |>
  dplyr::mutate(hier_branch = forcats::fct_reorder(hier_branch, MAE, .desc = TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x = hier_branch, y = MAE, fill = pct_poor)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("n=%d\n%.1f mg", n_episodes, MAE)),
                     vjust = -0.3, size = 3) +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = -1,
                                 name = "% Poor\nagreement") +
  ggplot2::labs(
    title    = "MAE by hierarchical branch",
    subtitle = "Identifies which decision tree branch to improve",
    x        = "Hierarchical branch",
    y        = "MAE (mg pred-equiv)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

print(p_mae_branch)

# ── Plot 3: Agreement category stacked bar, by SIG type ──────────────────────
agree_levels <- c("Exact (<=5%)", "Good (<=20%)", "Moderate (<=50%)", "Poor (>50%)")
agree_colors <- c("#2166AC", "#92C5DE", "#F4A582", "#D6604D")

p_agree_sig <- comparison |>
  dplyr::filter(!is.na(computed_dose), !is.na(sig_type)) |>
  dplyr::count(sig_type, agreement_category) |>
  dplyr::group_by(sig_type) |>
  dplyr::mutate(pct = 100 * n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    agreement_category = factor(agreement_category, levels = rev(agree_levels)),
    sig_type = forcats::fct_reorder(sig_type, pct[agreement_category == "Poor (>50%)"],
                                    .na_rm = TRUE)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = sig_type, y = pct, fill = agreement_category)) +
  ggplot2::geom_col(position = "stack", width = 0.8) +
  ggplot2::scale_fill_manual(values = setNames(rev(agree_colors), rev(agree_levels)),
                              name = "Agreement") +
  ggplot2::labs(
    title    = "Dose agreement by SIG type",
    subtitle = "Ordered by % poor agreement",
    x        = "SIG type",
    y        = "% of matched episodes"
  ) +
  ggplot2::theme_bw()

print(p_agree_sig)

# ── Plot 4: Error type frequency bar chart ───────────────────────────────────
p_error_types <- error_type_summary |>
  dplyr::mutate(
    error_type = factor(
      error_type,
      levels = error_type_summary$error_type   # already sorted by n_flagged
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = error_type, y = pct_of_matched)) +
  ggplot2::geom_col(fill = "#CC79A7", width = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%\n(n=%d)", pct_of_matched, n_flagged)),
                     vjust = -0.3, size = 3) +
  ggplot2::labs(
    title    = "Episode-level error type frequency",
    subtitle = sprintf(
      "DATE_TOL=%dd | DURATION_TOL=%dd | DOSE_TOL=%gmg | SHORT_GOLD=%dd",
      DATE_TOL_DAYS, DURATION_TOL_DAYS, DOSE_TOL_MG, SHORT_EPISODE_DAYS
    ),
    x = "Error type",
    y = "% of matched gold episodes"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))

print(p_error_types)

# ── Plot 5: Scatter coloured by SIG type ─────────────────────────────────────
p_scatter_sig <- comparison |>
  dplyr::filter(!is.na(computed_dose), !is.na(sig_type)) |>
  ggplot2::ggplot(ggplot2::aes(x = gold_dose, y = computed_dose, colour = sig_type)) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(alpha = 0.55, size = 1.8) +
  ggplot2::scale_colour_manual(values = SIG_COLORS, na.value = "#BBBBBB",
                                name = "SIG type") +
  ggplot2::facet_wrap(~ sig_type) +
  ggplot2::labs(
    title    = "Computed dose vs gold, by SIG type",
    subtitle = "Dashed = perfect agreement",
    x        = "Gold standard median daily dose (mg pred-equiv)",
    y        = "Computed median daily dose (mg pred-equiv)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

print(p_scatter_sig)

# ── Plot 6: Absolute error vs gold episode duration (short-episode MAPE issue)
p_dur_err <- episode_join |>
  dplyr::filter(!is.na(dose_err_mg)) |>
  ggplot2::ggplot(ggplot2::aes(x = gold_duration_days, y = dose_err_mg,
                               colour = err_short_gold)) +
  ggplot2::geom_point(alpha = 0.4, size = 1.5) +
  ggplot2::scale_colour_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#2271B3"),
                                labels = c("TRUE" = sprintf("<= %dd", SHORT_EPISODE_DAYS),
                                           "FALSE" = sprintf("> %dd", SHORT_EPISODE_DAYS)),
                                name = "Short gold\nepisode") +
  ggplot2::geom_vline(xintercept = SHORT_EPISODE_DAYS,
                      linetype = "dashed", colour = "#D55E00") +
  ggplot2::labs(
    title    = "Daily dose error vs gold episode duration",
    subtitle = "Short gold episodes (red) can inflate MAPE even for small absolute errors",
    x        = "Gold episode duration (days)",
    y        = "Absolute daily dose error (mg)"
  ) +
  ggplot2::theme_bw()

print(p_dur_err)

# ===========================================================================
# 8. Report
# ===========================================================================
message("\n\n")
cat(strrep("=", 70), "\n")
cat("ERROR DECOMPOSITION REPORT — SteroidDoseR Hierarchical Method\n")
cat(strrep("=", 70), "\n\n")

cat("OVERALL\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Gold episodes:    %d\n",     ev$summary$n_gold_periods))
cat(sprintf("  Matched:          %d (%.1f%%)\n",
            ev$summary$n_matched_periods, ev$summary$coverage_pct))
cat(sprintf("  MAE:              %.2f mg\n", ev$summary$MAE))
cat(sprintf("  RMSE:             %.2f mg\n", ev$summary$RMSE))
cat(sprintf("  MAPE:             %.1f%%\n\n", ev$summary$MAPE))

cat("TIER 1 — MAE BY SIG TYPE\n")
cat(strrep("-", 40), "\n")
strat_sig |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2))) |>
  print()

cat("\nInterpretation hint:\n")
top_err_sig <- strat_sig |> dplyr::filter(!is.na(sig_type)) |> dplyr::slice(1L)
if (nrow(top_err_sig) > 0L) {
  cat(sprintf(
    paste0(
      "  '%s' has the highest MAE (%.2f mg, n=%d). ",
      "If this type dominates overall error,\n",
      "  consider a separate algorithm or stricter filtering for this SIG category.\n"
    ),
    top_err_sig$sig_type, top_err_sig$MAE, top_err_sig$n_episodes
  ))
}

cat("\nTIER 2 — MAE BY HIERARCHICAL BRANCH\n")
cat(strrep("-", 40), "\n")
strat_branch |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2))) |>
  print()

cat("\nInterpretation hint:\n")
if (nrow(worst_branch) > 0L) {
  cat(sprintf(
    paste0(
      "  Branch '%s' has the worst agreement (%.1f%% poor, MAE %.2f mg).\n",
      "  Improve this branch specifically rather than tuning global parameters.\n"
    ),
    worst_branch$hier_branch, worst_branch$pct_poor, worst_branch$MAE
  ))
}

cat("\nTIER 3 — EPISODE-LEVEL ERROR TYPES\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Thresholds: date=±%dd | duration=±%dd | dose=±%gmg | total=±%gmg\n",
            DATE_TOL_DAYS, DURATION_TOL_DAYS, DOSE_TOL_MG, TOTAL_DOSE_TOL_MG))
cat(sprintf("  Short gold episode cutoff: %d days\n\n", SHORT_EPISODE_DAYS))
print(as.data.frame(error_type_summary), row.names = FALSE)

cat(strrep("=", 70), "\n")

# ===========================================================================
# 9. Manual review CSV  (gold episodes + matched computed records, long format)
# ===========================================================================
# One row per gold episode (source = "gold") or per drug-exposure record
# (source = "computed_record").  Rows for the same patient-episode pair share
# the same match_id so they sort together.  Gold rows carry agreement stats;
# computed rows carry method, sig_type, SIG text, bl_dose, sig_dose.
# ===========================================================================
message("\n=== Building manual review CSV ===")

# Ensure every computed record has a stable row key
if (!"drug_exposure_id" %in% names(hier_df)) {
  hier_df <- hier_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())
}

# Assign a sequential match_id to every gold episode
gold_indexed <- gold_std |>
  dplyr::mutate(
    pt_id_int = as.integer(patient_id),
    match_id  = dplyr::row_number()
  )

# For each computed record, find the gold episode with the most overlap
record_match <- hier_df |>
  dplyr::mutate(pt_id_int = as.integer(person_id)) |>
  dplyr::left_join(
    gold_indexed |>
      dplyr::transmute(pt_id_int, match_id,
                       g_start = episode_start, g_end = episode_end),
    by = "pt_id_int",
    relationship = "many-to-many"
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
  dplyr::mutate(
    match_id = dplyr::if_else(overlap_d > 0L, match_id, NA_integer_)
  )

# Gold rows — enriched with agreement stats from ev$comparison
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
    absolute_error_mg    = absolute_error,
    bias_error_mg        = bias_error,
    error_direction      = error_direction,
    method               = NA_character_,
    sig_type             = NA_character_,
    sig_text             = NA_character_,
    sig_status           = NA_character_,
    bl_dose              = NA_real_,
    sig_dose_nlp         = NA_real_
  )

# Computed record rows
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
    bl_dose              = bl_dose,
    sig_dose_nlp         = sig_dose
  )

# Bind: within each patient, gold row first, then its computed records
manual_review <- dplyr::bind_rows(gold_rows, computed_rows) |>
  dplyr::arrange(
    patient_id,
    match_id,                          # NAs (unmatched records) fall to bottom
    dplyr::desc(source == "gold"),     # gold before computed within same match
    start_date
  )

cat(sprintf(
  "  Manual review: %d rows (%d gold episodes, %d computed records)\n",
  nrow(manual_review),
  sum(manual_review$source == "gold"),
  sum(manual_review$source == "computed_record")
))

# ===========================================================================
# 10. Save results
# ===========================================================================
message("\n=== Saving results ===")

RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

writeLines(c(
  "SteroidDoseR — Run Parameters",
  strrep("=", 40),
  sprintf("Script:              error_decomposition.R"),
  sprintf("Run time:            %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Data source",
  strrep("-", 40),
  sprintf("RESULTS_DIR:         %s",
          if (is.null(RESULTS_DIR)) "NULL (recomputed from scratch)" else RESULTS_DIR),
  sprintf("Records:             %d", nrow(hier_df)),
  sprintf("Episodes:            %d", nrow(hier_episodes)),
  sprintf("Gold episodes:       %d", nrow(gold_std)),
  "",
  "Error type thresholds",
  strrep("-", 40),
  sprintf("DATE_TOL_DAYS:       %d",  DATE_TOL_DAYS),
  sprintf("DURATION_TOL_DAYS:   %d",  DURATION_TOL_DAYS),
  sprintf("DOSE_TOL_MG:         %g",  DOSE_TOL_MG),
  sprintf("TOTAL_DOSE_TOL_MG:   %g",  TOTAL_DOSE_TOL_MG),
  sprintf("SHORT_EPISODE_DAYS:  %d",  SHORT_EPISODE_DAYS),
  sprintf("CONFLICT_THR_MG:     %g",  CONFLICT_THR_MG),
  "",
  "Overall metrics",
  strrep("-", 40),
  sprintf("Coverage:            %.1f%%", ev$summary$coverage_pct),
  sprintf("MAE:                 %.2f mg", ev$summary$MAE),
  sprintf("RMSE:                %.2f mg", ev$summary$RMSE),
  sprintf("MAPE:                %.1f%%", ev$summary$MAPE)
), con = file.path(RUN_DIR, "params.txt"))

readr::write_csv(strat_sig,          file.path(RUN_DIR, "tier1_by_sig_type.csv"))
readr::write_csv(strat_branch,       file.path(RUN_DIR, "tier2_by_hier_branch.csv"))
readr::write_csv(episode_join,       file.path(RUN_DIR, "tier3_episode_errors.csv"))
readr::write_csv(error_type_summary, file.path(RUN_DIR, "tier3_error_type_summary.csv"))
readr::write_csv(cross_tab,          file.path(RUN_DIR, "cross_tab_sig_x_error.csv"))
readr::write_csv(cross_tab_branch,   file.path(RUN_DIR, "cross_tab_branch_x_error.csv"))
readr::write_csv(sig_type_record_tbl, file.path(RUN_DIR, "sig_type_record_counts.csv"))
readr::write_csv(comparison,         file.path(RUN_DIR, "comparison_annotated.csv"))
readr::write_csv(manual_review,      file.path(RUN_DIR, "manual_review.csv"))

ggplot2::ggsave(file.path(RUN_DIR, "plot_mae_by_sig_type.png"),
                p_mae_sig,    width = 8, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_mae_by_branch.png"),
                p_mae_branch, width = 8, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_agreement_by_sig.png"),
                p_agree_sig,  width = 9, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_error_types.png"),
                p_error_types, width = 9, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter_by_sig.png"),
                p_scatter_sig, width = 10, height = 8, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_error_vs_duration.png"),
                p_dur_err,    width = 8, height = 5, dpi = 150)

message(sprintf("Results saved to: %s", RUN_DIR))
message("\n=== Error decomposition complete ===")

# ===========================================================================
# 11. Manual review dashboard  (patient-by-patient Shiny viewer)
# ===========================================================================
# Requires:  install.packages(c("shiny", "DT"))
# Displays the manual_review table filtered by patient ID, plus a timeline
# plot showing gold episodes (coloured by agreement) and computed records
# (coloured by method/sig_type) on the same date axis.
# ===========================================================================

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("DT",    quietly = TRUE)) {
  message(
    "Install shiny + DT to enable the review dashboard:\n",
    "  install.packages(c('shiny', 'DT'))"
  )
} else {

  agree_pal <- c(
    "Exact (<=5%)"     = "#2166AC",
    "Good (<=20%)"     = "#92C5DE",
    "Moderate (<=50%)" = "#F4A582",
    "Poor (>50%)"      = "#D6604D",
    "Unmatched"        = "#AAAAAA"
  )

  patient_ids <- sort(unique(manual_review$patient_id))

  ui <- shiny::fluidPage(
    shiny::titlePanel("Manual Review Dashboard — Error Decomposition"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput(
          "patient_id", "Patient ID",
          choices  = patient_ids,
          selected = patient_ids[[1L]]
        ),
        shiny::hr(),
        shiny::h5("Patient summary"),
        shiny::verbatimTextOutput("patient_summary"),
        shiny::hr(),
        shiny::checkboxInput("show_unmatched",
                             "Show unmatched computed records", value = TRUE)
      ),
      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Timeline",
            shiny::br(),
            shiny::plotOutput("timeline_plot", height = "420px")
          ),
          shiny::tabPanel(
            "Table",
            shiny::br(),
            DT::DTOutput("review_table")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # Reactive: rows for the selected patient
    patient_data <- shiny::reactive({
      df <- manual_review |>
        dplyr::filter(patient_id == as.integer(input$patient_id))
      if (!input$show_unmatched) {
        df <- df |> dplyr::filter(!is.na(match_id) | source == "gold")
      }
      df
    })

    # Patient-level summary text
    output$patient_summary <- shiny::renderText({
      df   <- patient_data()
      gold <- df |> dplyr::filter(source == "gold")
      comp <- df |> dplyr::filter(source == "computed_record")
      paste0(
        "Gold episodes:    ", nrow(gold), "\n",
        "Computed records: ", nrow(comp), "\n",
        "Matched:          ", sum(!is.na(gold$computed_dose_for_ep)), "\n",
        "Poor agreement:   ",
        sum(gold$agreement_category == "Poor (>50%)", na.rm = TRUE), "\n",
        "MAE:              ",
        if (any(!is.na(gold$absolute_error_mg)))
          sprintf("%.1f mg", mean(gold$absolute_error_mg, na.rm = TRUE))
        else "N/A"
      )
    })

    # Timeline plot
    output$timeline_plot <- shiny::renderPlot({
      df   <- patient_data()
      gold <- df |>
        dplyr::filter(source == "gold") |>
        dplyr::mutate(
          agr_label = factor(
            dplyr::coalesce(agreement_category, "Unmatched"),
            levels = names(agree_pal)
          ),
          y_mid = 0.70
        )
      comp <- df |>
        dplyr::filter(source == "computed_record") |>
        dplyr::mutate(
          method_label = dplyr::coalesce(method, "unknown"),
          y_mid = 0.28
        )

      if (nrow(gold) + nrow(comp) == 0L) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "No data for this patient") +
            ggplot2::theme_void()
        )
      }

      p <- ggplot2::ggplot() +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::scale_y_continuous(
          breaks = c(0.28, 0.70),
          labels = c("Computed records", "Gold episodes"),
          limits = c(0, 1)
        ) +
        ggplot2::labs(
          title = sprintf("Patient %s — episode timeline", input$patient_id),
          x = "Date", y = NULL
        )

      if (nrow(gold) > 0L) {
        p <- p +
          ggplot2::geom_rect(
            data = gold,
            ggplot2::aes(xmin = start_date, xmax = end_date,
                         ymin = y_mid - 0.14, ymax = y_mid + 0.14,
                         fill = agr_label),
            colour = "white", linewidth = 0.4, alpha = 0.90
          ) +
          ggplot2::geom_text(
            data = gold,
            ggplot2::aes(
              x     = start_date + (end_date - start_date) / 2,
              y     = y_mid,
              label = sprintf("Gold: %.0f mg\n%s",
                              daily_dose_mg,
                              dplyr::coalesce(agreement_category, "unmatched"))
            ),
            size = 3, colour = "white", fontface = "bold"
          ) +
          ggplot2::scale_fill_manual(
            values = agree_pal, name = "Agreement", drop = FALSE
          )
      }

      if (nrow(comp) > 0L) {
        p <- p +
          ggplot2::geom_rect(
            data = comp,
            ggplot2::aes(xmin = start_date, xmax = end_date,
                         ymin = y_mid - 0.10, ymax = y_mid + 0.10),
            fill = "#4D9DE0", colour = "white",
            linewidth = 0.3, alpha = 0.80
          ) +
          ggplot2::geom_text(
            data = comp,
            ggplot2::aes(
              x     = start_date + (end_date - start_date) / 2,
              y     = y_mid,
              label = sprintf("%.0f mg\n[%s] %s",
                              dplyr::coalesce(daily_dose_mg, NA_real_),
                              method_label,
                              dplyr::coalesce(sig_type, ""))
            ),
            size = 2.5, colour = "white"
          )
      }
      p
    })

    # DT table — gold rows in yellow, computed in light blue;
    # agreement column colour-coded by category
    output$review_table <- DT::renderDT({
      df <- patient_data() |>
        dplyr::select(
          source, match_id, drug_name_std,
          start_date, end_date, duration_days,
          daily_dose_mg, computed_dose_for_ep,
          agreement_category, absolute_error_mg, bias_error_mg,
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
        options  = list(pageLength = 50, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        filter   = "top"
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

  message("\n=== Launching manual review dashboard ===")
  message("(Close the browser tab or press Escape in R to stop.)")
  shiny::runApp(shiny::shinyApp(ui, server))
}
