# run_analysis.R
# Full SteroidDoseR analysis: Baseline, NLP, Advanced NLP.
# All comparisons are performed at the EPISODE level.
# Gold-standard evaluation matches computed episodes to gold episodes via
# date-overlap join (evaluate_against_gold), one row per gold episode.
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
#     Rscript tests/run_analysis.R
#
# Configuration
# -------------
#   Set environment variables (or populate a .env file) before running:
#     JDBC_DRIVER_PATH   path to folder containing JDBC driver JARs
#     OMOP_SERVER        database server address (e.g. myserver.esm.johnshopkins.edu)
#     OMOP_CDM_SCHEMA    CDM schema  (default: "Myositis_OMOP.dbo")
#     OMOP_RESULTS_SCHEMA result schema (default: "Myositis_OMOP.Results")
#
#   The script also works without a live database: set USE_SYNTHETIC = TRUE
#   below to run against the bundled 29-record synthetic dataset.


# This script is designed for interactive use in RStudio.
# R CMD check runs every .R file in tests/ via R CMD BATCH (non-interactive).
# Exit immediately and cleanly when not in an interactive session.
if (!interactive()) quit(status = 0L, save = "no")
setwd("H:/Myositis/DoseCalculation/SteroidDoseR")
devtools::install_local("./SteroidDoseR/", force = TRUE)

devtools::load_all()
devtools::document()
devtools::test()


library(SteroidDoseR)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
USE_SYNTHETIC  <- FALSE   # set TRUE to use bundled data; no DB required
ENV_FILE       <- ".env"  # path to .env file (relative to working directory)
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
GAP_DAYS       <- 30L

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
#                            con$connection, cohort_sql)$PERSON_ID)
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
}

# ---------------------------------------------------------------------------
# 2. Detect available fields
# ---------------------------------------------------------------------------
con <- detect_capabilities(con)
message("\nCapabilities:")
print(con$capabilities)

# ---------------------------------------------------------------------------
# 3. Fetch raw drug-exposure rows (shared by all methods)
# ---------------------------------------------------------------------------
message("\n=== Fetching drug_exposure ===")
drug_df <- with_connector(con, function(active) {
  fetch_drug_exposure(
    active,
    drug_concept_ids = STEROID_CONCEPT_IDS,
    person_ids       = COHORT_PERSON_IDS,   # STEP 1: cohort filter at SQL level
    start_date       = START_DATE,
    end_date         = END_DATE
  )
})
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
  expand_tapers     = FALSE
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

# Advanced NLP episodes — build directly from the adv_nlp_df result.
# build_episodes() accepts a plain data frame; dose_col = "daily_dose_mg"
# is the column produced by calc_daily_dose_nlp_advanced().
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

# Build a combined data frame for plotting
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
) |>
  dplyr::mutate(method = factor(method, levels = c("Baseline", "NLP", "Advanced NLP")))

# Histogram / density comparison
p_dist <- ggplot2::ggplot(
  dist_df,
  ggplot2::aes(x = median_daily_dose, fill = method, colour = method)
) +
  ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
  ggplot2::scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320, 640),
    labels = scales::label_number()
  ) +
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

# Descriptive summary per method
cat("\nDose distribution summary by method (mg prednisone-equivalent):\n")
dist_df |>
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
cat("\nGold standard preview:\n")
print(head(gold_std[, c("patient_id", "episode_start", "episode_end",
                         "median_daily_dose", "days_covered")]))

cat("\nGold standard dose distribution:\n")
print(summary(gold_std$median_daily_dose))

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

cat("\nBaseline agreement categories:\n")
ev_baseline$comparison |>
  dplyr::filter(!is.na(computed_dose)) |>
  dplyr::count(agreement_category, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

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

cat("\nNLP agreement categories:\n")
ev_nlp$comparison |>
  dplyr::filter(!is.na(computed_dose)) |>
  dplyr::count(agreement_category, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

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

cat("\nAdvanced NLP agreement categories:\n")
ev_adv$comparison |>
  dplyr::filter(!is.na(computed_dose)) |>
  dplyr::count(agreement_category, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

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

cat("EPISODE COUNTS BY METHOD\n")
cat(strrep("-", 40), "\n")
episode_counts <- tibble::tibble(
  Method    = c("Baseline", "NLP", "Advanced NLP"),
  Patients  = c(dplyr::n_distinct(baseline_episodes$person_id),
                dplyr::n_distinct(nlp_episodes$person_id),
                dplyr::n_distinct(adv_nlp_episodes$person_id)),
  Episodes  = c(nrow(baseline_episodes),
                nrow(nlp_episodes),
                nrow(adv_nlp_episodes)),
  Median_mg = c(stats::median(baseline_episodes$median_daily_dose, na.rm = TRUE),
                stats::median(nlp_episodes$median_daily_dose,       na.rm = TRUE),
                stats::median(adv_nlp_episodes$median_daily_dose,   na.rm = TRUE))
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
# 12. Disconnect (live DB only)
# ===========================================================================
if (!USE_SYNTHETIC) {
  disconnect_connector(con)
}

message("\n=== Analysis complete ===")
