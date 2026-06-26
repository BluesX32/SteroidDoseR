# CodeToRun.R
# SteroidDoseR — Baseline + NLP Method Script
#
# Runs the Baseline (M1-M4) and Advanced NLP dose-extraction methods against
# the OMOP CDM drug_exposure table and saves record- and episode-level CSVs.
#
# Run standalone OR source from RunAll.R (which sets shared params and handles
# the database connection and disconnect).
#
# Workflow
# --------
# STEP 1 — Connection  : OMOP CDM database (skip when USE_SYNTHETIC = TRUE)
# STEP 2 — Data load   : drug_exposure extraction + quality checks
# STEP 3 — Baseline    : M1-M4 cascading imputation
# STEP 4 — NLP         : Advanced SIG parser + frequency audit
# STEP 5 — Save        : records_*.csv + episodes_*.csv written to RUN_DIR
#
# Comparison against the gold standard is handled in CompareToRun.R.

# When sourced from RunAll.R these are already set; skip re-installation.
if (!exists(".RUNALL_ACTIVE")) {
  devtools::install_local(getwd())
  if (!interactive()) quit(status = 0L, save = "no")
  library(SteroidDoseR)
  library(dplyr)
  library(ggplot2)
}

# ---------------------------------------------------------------------------
# 0. Configuration  —  values set by RunAll.R take precedence
# ---------------------------------------------------------------------------
if (!exists("USE_SYNTHETIC"))     USE_SYNTHETIC     <- FALSE
if (!exists("START_DATE"))        START_DATE        <- "2015-01-01"
if (!exists("END_DATE"))          END_DATE          <- "2025-12-31"
if (!exists("GAP_DAYS"))          GAP_DAYS          <- 30L
if (!exists("COHORT_PERSON_IDS")) COHORT_PERSON_IDS <- NULL
if (!exists("OUTPUT_DIR"))        OUTPUT_DIR        <- file.path(getwd(), "output")
if (!exists("RUN_DIR")) {
  RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
  dir.create(RUN_DIR, recursive = TRUE)
}

# Steroid drug_concept_id allow-list (matches Version2 Baseline extraction).
# Loaded from the bundled CSV; each row is one integer concept ID.
STEROID_CONCEPT_IDS <- as.integer(readr::read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

# ---------------------------------------------------------------------------
# 1a. Connection  —  set in RunAll.R; guard here for standalone use
# ---------------------------------------------------------------------------
if (!USE_SYNTHETIC) {
  if (!exists("conn")) {
    stop(
      "No database connection found (`conn` is not set).\n",
      "  - To run the full analysis: source('RunAll.R')\n",
      "  - To run standalone:        create `conn` in RunAll.R, then source this file."
    )
  }
  is_valid <- tryCatch(
    if (inherits(conn, "DatabaseConnectorConnection")) DatabaseConnector::dbIsValid(conn)
    else DBI::dbIsValid(conn),
    error = function(e) FALSE
  )
  if (!is_valid) {
    stop(
      "Database connection is stale.\n",
      "Re-run the connection block in RunAll.R to create a fresh `conn`."
    )
  }
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

# Exclude liquid/solution formulations — dose is expressed as concentration
# (mg/mL) so quantity*strength/days_supply gives mg/day only for tablets.
# "Oral Solution", "Oral Suspension", "Oral Liquid", "Syrup", "Concentrate",
# "Drops" all carry strength in mg/mL which the imputation cascade cannot
# handle correctly without the dispensed volume.
if ("drug_concept_name" %in% names(drug_df)) {
  .n_before   <- nrow(drug_df)
  drug_df <- drug_df |>
    dplyr::filter(!grepl(
      "solution|suspension|liquid|syrup|concentrate|drops|/ml",
      .data$drug_concept_name,
      ignore.case = TRUE,
      perl        = TRUE
    ))
  cat(sprintf(
    "\nFormulation filter: removed %d solution/liquid records (%d remain)\n",
    .n_before - nrow(drug_df), nrow(drug_df)
  ))
}

if (!"drug_exposure_id" %in% names(drug_df)) {
  drug_df <- drug_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())
}

# ===========================================================================
# 3.5. DATA QUALITY: cohort funnel and field-level missingness
# ===========================================================================
message("\n=== Data quality: cohort funnel + missingness ===")

# Key imputation inputs — each feeds a specific method/cascade step
.funnel_fields <- c(
  "drug_exposure_start_date",  # required by all methods
  "drug_exposure_end_date",    # required for days-supply fall-back (M4)
  "quantity",                  # M4: qty × strength / days_supply
  "days_supply",               # M3 + M4
  "amount_value",              # M3: direct mg dose from formulary
  "sig",                       # NLP methods
  "route_concept_name"         # oral filter
)
.present_fields <- intersect(.funnel_fields, names(drug_df))

.missingness_tbl <- tibble::tibble(
  Field     = .present_fields,
  N_nonNA   = vapply(.present_fields,
                     function(col) sum(!is.na(drug_df[[col]])), integer(1L)),
  Pct_nonNA = vapply(.present_fields,
                     function(col)
                       round(100 * mean(!is.na(drug_df[[col]])), 1),
                     numeric(1L))
) |>
  dplyr::arrange(.data$Pct_nonNA)

cat("\nField-level completeness (key imputation inputs):\n")
print(as.data.frame(.missingness_tbl), row.names = FALSE)

# SIG availability funnel
.n_total    <- nrow(drug_df)
.n_with_sig <- if ("sig" %in% names(drug_df)) {
  sum(!is.na(drug_df$sig) &
        nchar(trimws(as.character(drug_df$sig))) > 0,
      na.rm = TRUE)
} else 0L

cat(sprintf(
  "\nRecords: %d total  |  %d with non-empty SIG (%.1f%%)\n",
  .n_total, .n_with_sig, 100 * .n_with_sig / max(.n_total, 1L)
))

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
message("\n=== [1/2] Baseline method ===")

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

baseline_episodes <- build_episodes(
  baseline_df,
  end_col  = "drug_exposure_end_date",
  dose_col = "daily_dose_mg_imputed",
  gap_days = GAP_DAYS
)

show_person_trajectories(baseline_episodes, "Baseline")

# ===========================================================================
# 5. NLP METHOD  (Advanced NLP parser — taper-aware, extended SIG vocabulary)
# ===========================================================================
message("\n=== [2/2] NLP method ===")

nlp_df <- calc_daily_dose_nlp_advanced(
  drug_df,
  max_daily_dose_mg = 2000,
  expand_tapers     = FALSE,
  filter_oral       = TRUE,
  prn_action        = "na"   # exclude PRN from dose calculations
)

cat("\nParsed-status breakdown (record level):\n")
.parse_tbl <- as.data.frame(table(nlp_df$parsed_status, useNA = "ifany"))
names(.parse_tbl) <- c("parsed_status", "n_records")
.parse_tbl$pct <- round(100 * .parse_tbl$n_records / nrow(nlp_df), 1)
print(.parse_tbl[order(-.parse_tbl$n_records), ], row.names = FALSE)

cat("\nDose summary (parsed_status == 'ok' or 'taper_ok'):\n")
ok_mask <- nlp_df$parsed_status %in% c("ok", "taper_ok")
print(summary(nlp_df$daily_dose_mg[ok_mask]))

# Pass parsed_status as an extra column so it appears at episode level and
# enables stratified evaluation via evaluate_against_gold()$stratified$by_sig_status
nlp_episodes <- build_episodes(
  nlp_df,
  end_col    = "drug_exposure_end_date",
  dose_col   = "daily_dose_mg",
  gap_days   = GAP_DAYS,
  extra_cols = "parsed_status"
)

show_person_trajectories(nlp_episodes, "NLP")

# ===========================================================================
# 5.5  FREQUENCY NORMALIZATION AUDIT
#      freq_per_day and mg_per_admin are emitted by parse_sig_one_advanced().
#      daily_dose_mg = mg_per_admin * freq_per_day when both are available.
#      This section checks (a) whether freq is detected for multi-dose SIGs,
#      (b) whether daily_dose_mg correctly reflects the frequency multiplier,
#      and (c) what share of the 5-15 mg episode peak is driven by missed
#      frequency (e.g. "10 mg BID" parsed as 10 rather than 20 mg/day).
# ===========================================================================
message("\n=== [5.5] Frequency normalization audit ===")

if (all(c("freq_per_day", "mg_per_admin", "daily_dose_mg") %in% names(nlp_df))) {

  # --- 5.5-i. Overall frequency distribution --------------------------------
  cat("\nDistribution of freq_per_day (parsed doses-per-day):\n")
  .freq_tbl <- as.data.frame(table(
    freq_per_day = round(nlp_df$freq_per_day, 3L),
    useNA        = "ifany"
  ))
  .freq_tbl$pct <- round(100 * .freq_tbl$Freq / nrow(nlp_df), 1)
  print(.freq_tbl[order(-.freq_tbl$Freq), ], row.names = FALSE)

  # --- 5.5-ii. Expected vs actual daily dose for multi-dose records ----------
  # For records where freq > 1 and mg_per_admin is available:
  #   expected_daily = mg_per_admin * freq_per_day
  #   If daily_dose_mg ≈ mg_per_admin  → frequency was NOT applied  (bug)
  #   If daily_dose_mg ≈ expected_daily → frequency WAS applied      (correct)
  cat("\n--- Multi-dose records (freq_per_day > 1) ---\n")
  .multi <- nlp_df |>
    dplyr::filter(!is.na(.data$freq_per_day), .data$freq_per_day > 1,
                  !is.na(.data$mg_per_admin),  !is.na(.data$daily_dose_mg)) |>
    dplyr::mutate(
      expected_daily   = .data$mg_per_admin * .data$freq_per_day,
      freq_applied     = abs(.data$daily_dose_mg - .data$expected_daily) < 0.5,
      freq_not_applied = abs(.data$daily_dose_mg - .data$mg_per_admin)   < 0.5
    )

  cat(sprintf("Records with freq_per_day > 1 and mg_per_admin available: %d\n",
              nrow(.multi)))
  if (nrow(.multi) > 0L) {
    cat(sprintf("  Frequency correctly applied (daily ≈ per_admin × freq):  %d (%.1f%%)\n",
                sum(.multi$freq_applied),
                100 * mean(.multi$freq_applied)))
    cat(sprintf("  Frequency NOT applied (daily ≈ per_admin only):           %d (%.1f%%)\n",
                sum(.multi$freq_not_applied),
                100 * mean(.multi$freq_not_applied)))
    cat(sprintf("  Neither pattern (other calculation path):                  %d (%.1f%%)\n",
                sum(!.multi$freq_applied & !.multi$freq_not_applied),
                100 * mean(!.multi$freq_applied & !.multi$freq_not_applied)))
  }

  # --- 5.5-iii. Missed frequency detection: SIG text has BID/TID but
  #              freq_per_day is NA or 1 (parser returned single-dose) --------
  cat("\n--- Frequency keyword vs parsed freq_per_day ---\n")
  if ("sig" %in% names(nlp_df)) {
    .sig_lc <- tolower(trimws(as.character(
      dplyr::coalesce(nlp_df$sig, "")
    )))
    .kw_multi <- grepl(
      paste0(
        "\\bbid\\b|\\bb\\.i\\.d|twice\\s+(a\\s+)?day|twice\\s+daily|",
        "2\\s+times\\s+(a\\s+)?day|every\\s+12\\s*h|",
        "\\btid\\b|\\bt\\.i\\.d|three\\s+times\\s+(a\\s+)?day|",
        "every\\s+8\\s*h|\\bqid\\b|four\\s+times\\s+(a\\s+)?day|every\\s+6\\s*h"
      ),
      .sig_lc, perl = TRUE
    )
    .parsed_single <- is.na(nlp_df$freq_per_day) | nlp_df$freq_per_day <= 1

    n_kw   <- sum(.kw_multi, na.rm = TRUE)
    n_miss <- sum(.kw_multi & .parsed_single, na.rm = TRUE)
    cat(sprintf("Records with BID/TID/QID keyword in SIG:          %d (%.1f%%)\n",
                n_kw, 100 * n_kw / nrow(nlp_df)))
    cat(sprintf("  …of which freq_per_day is NA or 1 (missed):     %d (%.1f%%)\n",
                n_miss, 100 * n_miss / max(n_kw, 1L)))

    # Show sample of missed cases
    if (n_miss > 0L) {
      cat("\nSample missed-frequency records (sig has BID/TID but freq_per_day ≤ 1):\n")
      .missed_sample <- nlp_df[.kw_multi & .parsed_single, ] |>
        dplyr::select(dplyr::any_of(c(
          "person_id", "sig", "mg_per_admin", "freq_per_day",
          "daily_dose_mg", "parsed_status"
        ))) |>
        head(15L)
      print(as.data.frame(.missed_sample), row.names = FALSE)
    }
  }

  # --- 5.5-iv. 5-15 mg episode peak: what share is multi-dose? -------------
  cat("\n--- 5-15 mg daily_dose_mg records: frequency breakdown ---\n")
  .low_mid <- nlp_df |>
    dplyr::filter(!is.na(.data$daily_dose_mg),
                  .data$daily_dose_mg >= 5, .data$daily_dose_mg <= 15)
  cat(sprintf("Records in 5-15 mg range: %d\n", nrow(.low_mid)))
  if (nrow(.low_mid) > 0L) {
    .freq_breakdown <- as.data.frame(table(
      freq_per_day   = round(.low_mid$freq_per_day, 2),
      parsed_status  = .low_mid$parsed_status,
      useNA          = "ifany"
    ))
    .freq_breakdown <- .freq_breakdown[.freq_breakdown$Freq > 0L, ]
    print(.freq_breakdown[order(-.freq_breakdown$Freq), ], row.names = FALSE)
  }

} else {
  message("  freq_per_day / mg_per_admin not in nlp_df — skipping frequency audit.")
}

# Plausibility flag summary (applies to both methods)
.flag_summary <- function(ep, label) {
  n_impl  <- sum(ep$dose_implausible, na.rm = TRUE)
  n_pulse <- sum(ep$pulse_episode,    na.rm = TRUE)
  cat(sprintf(
    "\n%s flags: %d dose_implausible (<1 mg/day, %.1f%%), %d pulse_episode (>100 mg/day, %.1f%%) of %d episodes\n",
    label,
    n_impl,  100 * n_impl  / nrow(ep),
    n_pulse, 100 * n_pulse / nrow(ep),
    nrow(ep)
  ))
}
.flag_summary(baseline_episodes, "Baseline")
.flag_summary(nlp_episodes,      "NLP")

# ===========================================================================
# 6. SAVE — records and episodes
#    Gold-standard comparison and plots are handled in CompareToRun.R
# ===========================================================================
message("\n=== Saving CodeToRun.R results ===")

writeLines(c(
  "SteroidDoseR — CodeToRun.R Parameters",
  strrep("=", 40),
  sprintf("Run time:            %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("USE_SYNTHETIC:       %s", USE_SYNTHETIC),
  sprintf("START_DATE:          %s", START_DATE),
  sprintf("END_DATE:            %s", END_DATE),
  sprintf("GAP_DAYS:            %d", GAP_DAYS),
  sprintf("STEROID_CONCEPT_IDS: %d concept IDs", length(STEROID_CONCEPT_IDS)),
  sprintf("COHORT_PERSON_IDS:   %s",
          if (is.null(COHORT_PERSON_IDS)) "NULL (all patients)"
          else sprintf("%d person IDs", length(COHORT_PERSON_IDS))),
  sprintf("Drug-exposure rows:  %d", nrow(drug_df)),
  sprintf("Unique patients:     %d", dplyr::n_distinct(drug_df$person_id))
), con = file.path(RUN_DIR, "params_code.txt"))

readr::write_csv(baseline_df,       file.path(RUN_DIR, "records_baseline.csv"))
readr::write_csv(nlp_df,            file.path(RUN_DIR, "records_nlp.csv"))
readr::write_csv(baseline_episodes, file.path(RUN_DIR, "episodes_baseline.csv"))
readr::write_csv(nlp_episodes,      file.path(RUN_DIR, "episodes_nlp.csv"))

message(sprintf("Records and episodes saved → %s", RUN_DIR))
message("Run CompareToRun.R (or source RunAll.R) to compare against the gold standard.")

# Disconnect only when run standalone — RunAll.R owns the connection lifecycle.
if (!exists(".RUNALL_ACTIVE") && !USE_SYNTHETIC && exists("conn")) {
  tryCatch(
    if (inherits(conn, "DatabaseConnectorConnection")) DatabaseConnector::disconnect(conn)
    else DBI::dbDisconnect(conn),
    error = function(e) NULL
  )
}

message("=== CodeToRun.R complete ===")
