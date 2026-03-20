# run_analysis.R
# Full SteroidDoseR analysis: Baseline, NLP, and method comparison.
#
# Usage
# -----
#   Source this file interactively in RStudio, or run:
#     Rscript tests/run_analysis.R
#
# Configuration
# -------------
#   Set environment variables (or populate a .env file) before running:
#     JDBC_DRIVER_PATH   path to folder containing JDBC driver JARs
#     OMOP_SERVER        database server address (e.g. myserver.esm.johnshopkins.edu)
#     OMOP_CDM_SCHEMA    CDM schema  (default: "Myositis_OMOP.dbo")
#     OMOP_RESULTS_SCHEMA results schema (default: "Myositis_OMOP.Results")
#
#   The script also works without a live database: set USE_SYNTHETIC = TRUE
#   below to run against the bundled 29-record synthetic dataset.

library(SteroidDoseR)
library(dplyr)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
USE_SYNTHETIC  <- FALSE   # set TRUE to use bundled data; no DB required
ENV_FILE       <- ".env"  # path to .env file (relative to working directory)
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
GAP_DAYS       <- 30L

# ---------------------------------------------------------------------------
# 1. Connect / load data
# ---------------------------------------------------------------------------
if (USE_SYNTHETIC) {
  message("=== Using bundled synthetic data ===")
  extdata  <- system.file("extdata", package = "SteroidDoseR")
  drug_exp <- readr::read_csv(
    file.path(extdata, "synthetic_drug_exposure.csv"),
    show_col_types = FALSE
  )
  con <- create_df_connector(drug_exp)
} else {
  message("=== Connecting to live OMOP CDM ===")
  con <- create_connection_from_env(ENV_FILE)
  # con is an eager omop_connector — connection is already open.
  # Close it at the end with: disconnect_connector(con)
}

# ---------------------------------------------------------------------------
# 2. (Optional) Detect available fields at this site
# ---------------------------------------------------------------------------
con <- detect_capabilities(con)
message("\nCapabilities:")
print(con$capabilities)

# ---------------------------------------------------------------------------
# 3. Fetch raw drug-exposure rows (shared by both methods)
# ---------------------------------------------------------------------------
message("\n=== Fetching drug_exposure ===")
drug_df <- with_connector(con, function(active) {
  fetch_drug_exposure(active, start_date = START_DATE, end_date = END_DATE)
})
message(sprintf(
  "Fetched %d rows | %d unique persons",
  nrow(drug_df), length(unique(drug_df$person_id))
))

# Standardise drug names once so the column flows through to both methods.
# calc_daily_dose_nlp() would add it anyway, but baseline does not.
drug_df <- drug_df |>
  dplyr::mutate(drug_name_std = standardize_drug_name(drug_concept_name))

# Ensure drug_exposure_id exists (present in live DB fetch; absent in synthetic CSV).
if (!"drug_exposure_id" %in% names(drug_df)) {
  drug_df <- drug_df |> dplyr::mutate(drug_exposure_id = dplyr::row_number())
}

# ---------------------------------------------------------------------------
# 4. Baseline method
# ---------------------------------------------------------------------------
message("\n=== Baseline method ===")

baseline_df <- calc_daily_dose_baseline(
  drug_df,
  m2_sig_parse = "auto"   # parse sig automatically to enable M2
)

cat("\nImputation method breakdown:\n")
print(table(baseline_df$imputation_method, useNA = "ifany"))

cat("\nDose summary (non-missing):\n")
print(summary(baseline_df$daily_dose_mg_imputed[
  !is.na(baseline_df$daily_dose_mg_imputed)
]))

baseline_episodes <- run_pipeline(
  drug_df,
  method       = "baseline",
  m2_sig_parse = "auto",
  return_level = "episode",
  gap_days     = GAP_DAYS
)

cat(sprintf(
  "\nBaseline: %d episodes from %d persons\n",
  nrow(baseline_episodes), length(unique(baseline_episodes$person_id))
))
print(head(baseline_episodes[, c(
  "person_id", "drug_name_std", "episode_start", "episode_end",
  "n_days", "n_records", "median_daily_dose"
)]))

# ---------------------------------------------------------------------------
# 5. NLP method
# ---------------------------------------------------------------------------
message("\n=== NLP method ===")

nlp_df <- calc_daily_dose_nlp(drug_df)

cat("\nparsed_status breakdown:\n")
print(table(nlp_df$parsed_status, useNA = "ifany"))

cat("\nDose summary (parsed_status == 'ok'):\n")
print(summary(nlp_df$daily_dose_mg[nlp_df$parsed_status == "ok"]))

cat("\nTop 20 unparsed SIG strings:\n")
nlp_df |>
  filter(parsed_status == "no_parse") |>
  count(sig, sort = TRUE) |>
  head(20) |>
  print()

cat("\nSample taper SIGs:\n")
nlp_df |>
  filter(taper_flag) |>
  select(drug_name_std, sig) |>
  distinct() |>
  head(10) |>
  print()

nlp_episodes <- run_pipeline(
  drug_df,
  method       = "nlp",
  return_level = "episode",
  gap_days     = GAP_DAYS
)

cat(sprintf(
  "\nNLP: %d episodes from %d persons\n",
  nrow(nlp_episodes), length(unique(nlp_episodes$person_id))
))
print(head(nlp_episodes[, c(
  "person_id", "drug_name_std", "episode_start", "episode_end",
  "n_days", "n_records", "median_daily_dose"
)]))

# ---------------------------------------------------------------------------
# 6. Method comparison (exposure level)
# ---------------------------------------------------------------------------
message("\n=== Baseline vs NLP comparison (exposure level) ===")

# Join on drug_exposure_id so we compare the exact same records
cmp <- inner_join(
  baseline_df |>
    select(drug_exposure_id, person_id, drug_name_std,
           sig, imputation_method,
           baseline_dose = daily_dose_mg_imputed),
  nlp_df |>
    select(drug_exposure_id, nlp_dose = daily_dose_mg,
           parsed_status, taper_flag, prn_flag, free_text_flag),
  by = "drug_exposure_id"
) |>
  mutate(
    both_present  = !is.na(baseline_dose) & !is.na(nlp_dose),
    dose_diff     = nlp_dose - baseline_dose,
    within_10pct  = both_present &
      abs(dose_diff) / pmax(baseline_dose, 0.01) < 0.10
  )

cat(sprintf("\nRecords with both doses: %d / %d\n",
            sum(cmp$both_present, na.rm = TRUE), nrow(cmp)))
cat(sprintf("Agreement within 10%%:    %.1f%%\n",
            100 * mean(cmp$within_10pct, na.rm = TRUE)))

cat("\nBaseline coverage by imputation method:\n")
cmp |>
  count(imputation_method, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

cat("\nNLP coverage by parsed_status:\n")
cmp |>
  count(parsed_status, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

cat("\nDose difference distribution (NLP − Baseline, both non-NA):\n")
cmp |>
  filter(both_present) |>
  pull(dose_diff) |>
  summary() |>
  print()

cat("\nLargest disagreements (top 20):\n")
cmp |>
  filter(both_present) |>
  arrange(desc(abs(dose_diff))) |>
  select(drug_exposure_id, drug_name_std, sig,
         baseline = baseline_dose, nlp = nlp_dose, diff = dose_diff,
         imputation_method, parsed_status) |>
  head(20) |>
  print()

# ---------------------------------------------------------------------------
# 7. Method comparison (episode level)
# ---------------------------------------------------------------------------
message("\n=== Baseline vs NLP comparison (episode level) ===")

epi_cmp <- inner_join(
  baseline_episodes |>
    select(person_id, drug_name_std, episode_start, episode_end,
           baseline_dose = median_daily_dose, n_records_b = n_records),
  nlp_episodes |>
    select(person_id, drug_name_std, episode_start, episode_end,
           nlp_dose = median_daily_dose, n_records_n = n_records),
  by = c("person_id", "drug_name_std", "episode_start", "episode_end")
)

cat(sprintf(
  "\nEpisodes matched on person+drug+start+end: %d\n  (baseline total: %d, NLP total: %d)\n",
  nrow(epi_cmp), nrow(baseline_episodes), nrow(nlp_episodes)
))

if (nrow(epi_cmp) > 0) {
  epi_cmp <- epi_cmp |>
    mutate(
      dose_diff    = nlp_dose - baseline_dose,
      within_10pct = abs(dose_diff) / pmax(baseline_dose, 0.01) < 0.10
    )

  cat(sprintf("Episode agreement within 10%%: %.1f%%\n",
              100 * mean(epi_cmp$within_10pct, na.rm = TRUE)))

  cat("\nEpisode-level dose difference (NLP − Baseline):\n")
  summary(epi_cmp$dose_diff) |> print()
}

# ---------------------------------------------------------------------------
# 8. (Optional) Gold standard evaluation
# ---------------------------------------------------------------------------
if (USE_SYNTHETIC) {
  message("\n=== Gold standard evaluation (synthetic data) ===")

  gold_std <- readr::read_csv(
    file.path(extdata, "synthetic_gold_standard.csv"),
    show_col_types = FALSE
  )

  cat("\n--- Baseline vs gold ---\n")
  eval_b <- evaluate_against_gold(baseline_episodes, gold_std)
  print(eval_b$summary[, c("coverage_pct", "MAE", "MBE", "RMSE")])

  cat("\n--- NLP vs gold ---\n")
  eval_n <- evaluate_against_gold(nlp_episodes, gold_std)
  print(eval_n$summary[, c("coverage_pct", "MAE", "MBE", "RMSE")])
}

# ---------------------------------------------------------------------------
# 9. Disconnect (live DB only)
# ---------------------------------------------------------------------------
if (!USE_SYNTHETIC) {
  disconnect_connector(con)
}

message("\n=== Analysis complete ===")
