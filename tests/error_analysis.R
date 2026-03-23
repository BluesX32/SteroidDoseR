# error_analysis.R
# Deep-dive into patients with large dose errors vs gold standard.
#
# Prerequisite: run run_analysis.R first (or source it) so the following
# objects are in your R session:
#   drug_df           — raw drug_exposure records
#   gold_std          — gold standard (with pred-equiv doses)
#   ev_baseline       — evaluate_against_gold() result for Baseline
#   ev_nlp            — evaluate_against_gold() result for NLP
#   ev_adv            — evaluate_against_gold() result for Advanced NLP
#   baseline_episodes — built episodes (Baseline)
#   nlp_episodes      — built episodes (NLP)
#   adv_nlp_episodes  — built episodes (Advanced NLP)
#
# Usage: Source interactively in RStudio after run_analysis.R.

if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
ABS_ERROR_THRESHOLD <- 10    # mg pred-equiv: flag if |error| > this
REL_ERROR_THRESHOLD <- 50    # %: flag if |relative error| > this
TOP_N_PATIENTS      <- 20    # show this many high-error patients

# ---------------------------------------------------------------------------
# Helper: extract high-error comparison rows across all three methods
# ---------------------------------------------------------------------------
flag_errors <- function(ev_result, method_label) {
  ev_result$comparison |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::mutate(method = method_label) |>
    dplyr::filter(
      absolute_error > ABS_ERROR_THRESHOLD |
      absolute_relative_error_pct > REL_ERROR_THRESHOLD
    )
}

high_err <- dplyr::bind_rows(
  flag_errors(ev_baseline, "Baseline"),
  flag_errors(ev_nlp,      "NLP"),
  flag_errors(ev_adv,      "Advanced NLP")
) |>
  dplyr::arrange(dplyr::desc(absolute_error))

cat(sprintf(
  "\nHigh-error episodes (|error| > %g mg OR |rel error| > %g%%):\n",
  ABS_ERROR_THRESHOLD, REL_ERROR_THRESHOLD
))
cat(sprintf("  Baseline: %d  |  NLP: %d  |  Advanced NLP: %d\n\n",
  sum(high_err$method == "Baseline"),
  sum(high_err$method == "NLP"),
  sum(high_err$method == "Advanced NLP")
))

# ---------------------------------------------------------------------------
# 1. Top-N high-error patient summary
# ---------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("1. TOP HIGH-ERROR EPISODES\n")
cat(strrep("-", 40), "\n")

high_err |>
  dplyr::select(
    method, patient_id, episode_start, episode_end,
    gold_dose, computed_dose, absolute_error,
    bias_error, absolute_relative_error_pct, agreement_category
  ) |>
  head(TOP_N_PATIENTS) |>
  print(n = TOP_N_PATIENTS)

# ---------------------------------------------------------------------------
# 2. Per-patient detail: for each high-error patient, show original records
# ---------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("2. ORIGINAL DRUG_DF RECORDS FOR HIGH-ERROR PATIENTS\n")
cat(strrep("-", 40), "\n")

high_pts <- unique(high_err$patient_id)
cat(sprintf("Unique high-error patients: %d\n\n", length(high_pts)))

for (pt in high_pts[seq_len(min(TOP_N_PATIENTS, length(high_pts)))]) {

  # Gold episodes for this patient
  gold_pt <- gold_std |>
    dplyr::filter(patient_id == pt) |>
    dplyr::select(patient_id, episode_start, episode_end,
                  drug_name_std, median_daily_dose_raw, median_daily_dose)

  # Raw drug_df records for this patient
  raw_pt <- drug_df |>
    dplyr::filter(person_id == pt) |>
    dplyr::arrange(drug_exposure_start_date) |>
    dplyr::select(
      person_id, drug_name_std, drug_exposure_start_date, drug_exposure_end_date,
      dplyr::any_of(c("sig", "drug_source_value", "daily_dose_mg_imputed",
                       "daily_dose_mg", "amount_value", "quantity",
                       "days_supply", "imputation_method"))
    )

  # Error rows for this patient across methods
  err_pt <- high_err |>
    dplyr::filter(patient_id == pt) |>
    dplyr::select(method, episode_start, episode_end,
                  gold_dose, computed_dose, absolute_error,
                  bias_error, agreement_category)

  cat(sprintf("\n--- Patient %s ---\n", pt))

  cat("  Gold standard episodes:\n")
  print(as.data.frame(gold_pt), row.names = FALSE)

  cat("  Error summary by method:\n")
  print(as.data.frame(err_pt), row.names = FALSE)

  cat("  Raw drug_df records:\n")
  print(as.data.frame(raw_pt), row.names = FALSE)

  cat(strrep("-", 60), "\n")
}

# ---------------------------------------------------------------------------
# 3. Error by drug name
# ---------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("3. ERROR BY DRUG (gold standard drug_name_std)\n")
cat(strrep("-", 40), "\n")

high_err |>
  dplyr::left_join(
    gold_std |> dplyr::select(patient_id, episode_start, episode_end, drug_name_std),
    by = c("patient_id", "episode_start", "episode_end")
  ) |>
  dplyr::group_by(method, drug_name_std) |>
  dplyr::summarise(
    n       = dplyr::n(),
    MAE     = round(mean(absolute_error, na.rm = TRUE), 2),
    MBE     = round(mean(bias_error,     na.rm = TRUE), 2),
    MAPE    = round(mean(absolute_relative_error_pct, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(method, dplyr::desc(n)) |>
  print(n = 50)

# ---------------------------------------------------------------------------
# 4. Error pattern: over- vs under-estimation
# ---------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("4. OVER- vs UNDER-ESTIMATION BY METHOD\n")
cat(strrep("-", 40), "\n")

high_err |>
  dplyr::group_by(method, error_direction) |>
  dplyr::summarise(
    n    = dplyr::n(),
    mean_abs_error = round(mean(absolute_error, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  print()

# ---------------------------------------------------------------------------
# 5. Export high-error table for manual review
# ---------------------------------------------------------------------------
out_path <- file.path(dirname(GOLD_STD_PATH), "high_error_episodes.csv")
high_err |>
  dplyr::left_join(
    gold_std |> dplyr::select(patient_id, episode_start, episode_end,
                               drug_name_std, median_daily_dose_raw),
    by = c("patient_id", "episode_start", "episode_end")
  ) |>
  readr::write_csv(out_path)

cat(sprintf("\nHigh-error table written to:\n  %s\n", out_path))
