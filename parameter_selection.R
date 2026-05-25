# parameter_selection.R
# SteroidDoseR — Adaptive Hierarchical Parameter Selection
#
# Selects optimal MATCH_TOL, DIFF_THRESHOLD, OVERRIDE_PREFER, and GAP_DAYS for
# the hierarchical method by minimising MAE on a held-out training set, then
# evaluates the selected parameters on the remaining test patients.
#
# Adaptive hierarchical decision tree (per record):
# ─────────────────────────────────────────────────────────────────────────────
#  bl_complete   sig_complete (steady)   Outcome
#  ──────────────────────────────────────────────────────────────────────────
#  TRUE          TRUE   |diff| <= MATCH_TOL           → cross_checked  (SIG)
#                       MATCH_TOL < |diff| <= DIFF_THR → blended (avg)
#                       |diff| > DIFF_THRESHOLD        → adaptive_override
#                                                         (NLP or baseline —
#                                                          learned from training)
#  TRUE          FALSE (absent / unparseable)          → baseline_only
#  FALSE         TRUE  (steady)                        → nlp_fills_baseline
#  FALSE         TRUE  (taper)                         → nlp_taper
#  FALSE         FALSE                                 → missing (NA)
# ─────────────────────────────────────────────────────────────────────────────
#
# Workflow
# --------
# STEP 1 — Load data from OMOP CDM (or synthetic).
# STEP 2 — Run hierarchical once (any params) to capture bl_dose / sig_dose /
#           sig_status — these are parameter-independent.
# STEP 3 — Load gold standard; convert to pred-equiv.
# STEP 4 — Identify matched patients; split train / test.
# STEP 5 — Grid search over MATCH_TOL_GRID × DIFF_THRESHOLD_GRID ×
#           OVERRIDE_PREFER_OPTS × GAP_DAYS_GRID on training patients → best combo.
# STEP 6 — Apply best params to full cohort; evaluate on test patients.
# STEP 7 — Save results to timestamped folder.
#
# Usage
# -----
#   Source interactively in RStudio, or:
#     Rscript parameter_selection.R
#
# Connection modes  (set USE_SYNTHETIC below)
# --------------------------------------------
#   Mode A — Synthetic data (no database required):   USE_SYNTHETIC = TRUE
#   Mode B / C — Live OMOP CDM:                       USE_SYNTHETIC = FALSE

devtools::install_local(getwd())
if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)
library(ggplot2)
library(purrr)

# ===========================================================================
# 0. Configuration
# ===========================================================================
USE_SYNTHETIC  <- FALSE
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
CONCURRENT_AGG <- "per_drug"     # "per_drug" or "sum_all"

# -- Train / test split -------------------------------------------------------
# Fraction of matched patients used for TRAINING (parameter selection).
# The remaining patients form the held-out TEST set for unbiased evaluation.
# Split is performed at the PATIENT level to prevent leakage.
TRAIN_FRACTION <- 0.30
RANDOM_SEED    <- 42L            # for reproducibility

# -- Grid search space --------------------------------------------------------
# MATCH_TOL_GRID:      |bl_dose - sig_dose| at or below this → cross_checked.
# DIFF_THRESHOLD_GRID: above this → adaptive_override; between → blended.
# OVERRIDE_PREFER_OPTS: which source to trust when |diff| > DIFF_THRESHOLD.
# GAP_DAYS_GRID:       max gap (days) between records before a new episode starts.
MATCH_TOL_GRID       <- c(0.01, 0.5, 1, 2, 5)    # mg/day
DIFF_THRESHOLD_GRID  <- c(2, 5, 10, 20, 50)       # mg/day
OVERRIDE_PREFER_OPTS <- c("nlp", "baseline")
GAP_DAYS_GRID        <- c(14L, 21L, 30L, 45L, 60L) # days

# -- Evaluation thresholds ----------------------------------------------------
DOSE_THRESHOLD_MG  <- 10    # absolute mg pred-equiv for binary agreement; NULL to disable
DOSE_THRESHOLD_PCT <- NULL  # relative %; NULL to disable

GOLD_STD_PATH  <- "/your/path/to/gold-standard"
OUTPUT_DIR     <- file.path(getwd(), "output_parameter_selection")

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
# 2. Compute parameter-independent intermediate results
# ===========================================================================
# Run hierarchical once with arbitrary params to capture bl_dose, sig_dose,
# sig_status, sig_taper_flag, and bl_method. These do not depend on
# MATCH_TOL or DIFF_THRESHOLD; only the final dose assignment does.
message("\n=== [1/4] Pre-computing hierarchical intermediates ===")

hier_raw <- calc_daily_dose_hierarchical(
  drug_df,
  diff_threshold    = 0,          # dummy — we re-apply the decision below
  match_tol         = -Inf,       # dummy — forces all to adaptive_override path
  max_daily_dose_mg = 2000,
  filter_oral       = TRUE
)

message(sprintf(
  "  Intermediate results: %d records | bl_dose available: %d | sig_dose available: %d",
  nrow(hier_raw),
  sum(!is.na(hier_raw$bl_dose)),
  sum(!is.na(hier_raw$sig_dose))
))

# ===========================================================================
# Helper: re-apply the adaptive decision rule to a pre-computed hier_df
# ===========================================================================

#' @param df        Data frame with bl_dose, sig_dose, sig_status, sig_taper_flag
#' @param match_tol Tolerance (mg/day): |diff| <= this → cross_checked
#' @param diff_thr  Threshold (mg/day): |diff| > this AND > match_tol → adaptive_override
#' @param override  "nlp" or "baseline" for the adaptive_override branch
apply_adaptive_decision <- function(df, match_tol, diff_thr, override) {
  bl    <- df$bl_dose
  sig   <- df$sig_dose
  stat  <- df$sig_status
  taper <- !is.na(df$sig_taper_flag) & df$sig_taper_flag

  bl_ok          <- !is.na(bl)
  sig_ok_steady  <- !is.na(sig) & !is.na(stat) & stat == "ok" & !taper
  sig_ok_taper   <- taper & !is.na(sig)

  dose_diff <- abs(bl - sig)

  daily_dose <- dplyr::case_when(
    bl_ok & sig_ok_steady & dose_diff <= match_tol    ~ sig,
    bl_ok & sig_ok_steady & dose_diff <= diff_thr     ~ (bl + sig) / 2,
    bl_ok & sig_ok_steady & dose_diff >  diff_thr     ~
      if (override == "nlp") sig else bl,
    bl_ok & !sig_ok_steady & !sig_ok_taper            ~ bl,
    !bl_ok & sig_ok_steady                            ~ sig,
    !bl_ok & sig_ok_taper                             ~ sig,
    TRUE                                              ~ NA_real_
  )

  method <- dplyr::case_when(
    bl_ok & sig_ok_steady & dose_diff <= match_tol    ~ "cross_checked",
    bl_ok & sig_ok_steady & dose_diff <= diff_thr     ~ "blended",
    bl_ok & sig_ok_steady & dose_diff >  diff_thr     ~ "adaptive_override",
    bl_ok & !sig_ok_steady & !sig_ok_taper            ~ "baseline_only",
    !bl_ok & sig_ok_steady                            ~ "nlp_fills_baseline",
    !bl_ok & sig_ok_taper                             ~ "nlp_taper",
    TRUE                                              ~ "missing"
  )

  df$daily_dose_mg      <- daily_dose
  df$hierarchical_method <- method
  df
}

# ===========================================================================
# Helper: evaluate a set of parameters against gold (subset of patients)
# ===========================================================================

eval_params_on_patients <- function(hier_df, gold_df, patient_ids,
                                    match_tol, diff_thr, override,
                                    gap_days, concurrent_agg) {
  # Apply adaptive decision to the full record set
  result_df <- apply_adaptive_decision(hier_df, match_tol, diff_thr, override)

  # Filter to the requested patient subset
  result_subset <- result_df |>
    dplyr::filter(as.integer(person_id) %in% as.integer(patient_ids))

  if (nrow(result_subset) == 0L) return(NA_real_)

  # Convert to pred-equiv and build episodes
  result_eq <- tryCatch(
    convert_pred_equiv(result_subset,
                       drug_col = "drug_name_std",
                       dose_col = "daily_dose_mg"),
    error = function(e) result_subset
  )
  if (!"pred_equiv_mg" %in% names(result_eq)) return(NA_real_)

  episodes <- tryCatch(
    build_episodes(result_eq,
                   end_col        = "drug_exposure_end_date",
                   dose_col       = "pred_equiv_mg",
                   gap_days       = gap_days,
                   concurrent_agg = concurrent_agg),
    error = function(e) NULL
  )
  if (is.null(episodes) || nrow(episodes) == 0L) return(NA_real_)

  # Gold subset
  gold_subset <- gold_df |>
    dplyr::filter(as.integer(patient_id) %in% as.integer(patient_ids))
  if (nrow(gold_subset) == 0L) return(NA_real_)

  # Evaluate; extract MAE
  ev <- tryCatch(
    evaluate_against_gold(episodes, gold_subset, gold_id_col = "patient_id"),
    error = function(e) NULL
  )
  if (is.null(ev)) return(NA_real_)
  ev$summary$MAE
}

# ===========================================================================
# 3. Load gold standard and convert to pred-equiv
# ===========================================================================
message("\n=== [2/4] Loading gold standard ===")

gold_std <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)

cat(sprintf(
  "Gold standard: %d episodes from %d patients\n",
  nrow(gold_std), dplyr::n_distinct(gold_std$patient_id)
))

# Drug name mapping for pred-equiv conversion (using oral-filtered records)
gold_drug_map <- hier_raw |>
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

# ===========================================================================
# 4. Identify matched patients and train / test split
# ===========================================================================
message("\n=== [3/4] Train / test split ===")

computed_pts <- unique(as.integer(hier_raw$person_id))
gold_pts     <- unique(as.integer(gold_std$patient_id))
matched_pts  <- intersect(computed_pts, gold_pts)

cat(sprintf(
  "  Computed patients: %d | Gold patients: %d | Matched: %d\n",
  length(computed_pts), length(gold_pts), length(matched_pts)
))

if (length(matched_pts) == 0L) {
  stop("No patients overlap between computed results and gold standard. ",
       "Check that person_id and patient_id use the same integer scale.")
}

set.seed(RANDOM_SEED)
n_train   <- max(1L, round(length(matched_pts) * TRAIN_FRACTION))
train_pts <- sort(sample(matched_pts, n_train))
test_pts  <- setdiff(matched_pts, train_pts)

cat(sprintf(
  "  Train patients: %d (%.0f%%) | Test patients: %d (%.0f%%)\n",
  length(train_pts), 100 * length(train_pts) / length(matched_pts),
  length(test_pts),  100 * length(test_pts)  / length(matched_pts)
))

# ===========================================================================
# 5. Grid search on training set
# ===========================================================================
message("\n=== [4/4] Grid search ===")

grid <- tidyr::expand_grid(
  match_tol       = MATCH_TOL_GRID,
  diff_threshold  = DIFF_THRESHOLD_GRID,
  override_prefer = OVERRIDE_PREFER_OPTS,
  gap_days        = GAP_DAYS_GRID
) |>
  # Constraint: diff_threshold must be > match_tol to make sense
  dplyr::filter(diff_threshold > match_tol)

n_grid <- nrow(grid)
cat(sprintf("  Grid size: %d combinations\n", n_grid))

grid_results <- purrr::pmap_dfr(
  grid,
  function(match_tol, diff_threshold, override_prefer, gap_days) {
    mae_train <- eval_params_on_patients(
      hier_raw, gold_std, train_pts,
      match_tol      = match_tol,
      diff_thr       = diff_threshold,
      override       = override_prefer,
      gap_days       = gap_days,
      concurrent_agg = CONCURRENT_AGG
    )
    tibble::tibble(
      match_tol       = match_tol,
      diff_threshold  = diff_threshold,
      override_prefer = override_prefer,
      gap_days        = gap_days,
      mae_train       = mae_train
    )
  }
)

# Show top 10 combos
cat("\nTop 10 parameter combos by training MAE:\n")
grid_ranked <- grid_results |>
  dplyr::filter(!is.na(mae_train)) |>
  dplyr::arrange(mae_train)

grid_ranked |>
  head(10) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3))) |>
  print()

if (nrow(grid_ranked) == 0L) {
  stop("Grid search produced no valid results. ",
       "Check that gold_std patient_ids overlap with drug_df person_ids.")
}

# Best parameters
best <- grid_ranked |> dplyr::slice(1L)
BEST_MATCH_TOL      <- best$match_tol
BEST_DIFF_THRESHOLD <- best$diff_threshold
BEST_OVERRIDE       <- best$override_prefer
BEST_GAP_DAYS       <- best$gap_days
BEST_MAE_TRAIN      <- best$mae_train

cat(sprintf(
  "\nBest parameters (min training MAE = %.3f mg):\n",
  BEST_MAE_TRAIN
))
cat(sprintf("  MATCH_TOL:       %g mg/day\n", BEST_MATCH_TOL))
cat(sprintf("  DIFF_THRESHOLD:  %g mg/day\n", BEST_DIFF_THRESHOLD))
cat(sprintf("  OVERRIDE_PREFER: %s\n",         BEST_OVERRIDE))
cat(sprintf("  GAP_DAYS:        %d days\n",    BEST_GAP_DAYS))

# ===========================================================================
# 6. Evaluate best parameters on test set
# ===========================================================================
message("\n=== Evaluating best parameters on test set ===")

if (length(test_pts) == 0L) {
  warning("No test patients — skipping held-out evaluation.")
  BEST_MAE_TEST <- NA_real_
} else {
  BEST_MAE_TEST <- eval_params_on_patients(
    hier_raw, gold_std, test_pts,
    match_tol      = BEST_MATCH_TOL,
    diff_thr       = BEST_DIFF_THRESHOLD,
    override       = BEST_OVERRIDE,
    gap_days       = BEST_GAP_DAYS,
    concurrent_agg = CONCURRENT_AGG
  )
  cat(sprintf("  Test MAE (held-out %.0f%%): %.3f mg\n",
              100 * (1 - TRAIN_FRACTION), BEST_MAE_TEST))
}

# ===========================================================================
# 7. Apply best parameters to full cohort and build final episodes
# ===========================================================================
message("\n=== Building final episodes with best parameters ===")

best_df <- apply_adaptive_decision(
  hier_raw,
  match_tol = BEST_MATCH_TOL,
  diff_thr  = BEST_DIFF_THRESHOLD,
  override  = BEST_OVERRIDE
)

# Method-choice breakdown
cat("\nAdaptive hierarchical_method breakdown:\n")
method_tbl <- best_df |>
  dplyr::count(hierarchical_method, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(method_tbl), row.names = FALSE)

cat(sprintf(
  "\nRecords with a computable dose: %d / %d (%.1f%%)\n",
  sum(!is.na(best_df$daily_dose_mg)),
  nrow(best_df),
  100 * mean(!is.na(best_df$daily_dose_mg))
))

# Convert and build episodes
best_eq <- convert_pred_equiv(best_df,
                              drug_col = "drug_name_std",
                              dose_col = "daily_dose_mg")

best_episodes <- build_episodes(
  best_eq,
  end_col        = "drug_exposure_end_date",
  dose_col       = "pred_equiv_mg",
  gap_days       = BEST_GAP_DAYS,
  concurrent_agg = CONCURRENT_AGG
)

cat(sprintf(
  "\nFinal episodes: %d episodes from %d patients\n",
  nrow(best_episodes), dplyr::n_distinct(best_episodes$person_id)
))

# Full-cohort evaluation
message("\n=== Full-cohort evaluation vs gold standard ===")
ev_best <- tryCatch(
  evaluate_against_gold(best_episodes, gold_std, gold_id_col = "patient_id"),
  error = function(e) { warning(e$message); NULL }
)

if (!is.null(ev_best)) {
  cat(sprintf(
    "Full cohort: %d common patients | %d/%d gold episodes matched (%.1f%% coverage)\n",
    length(intersect(unique(best_episodes$person_id), unique(gold_std$patient_id))),
    ev_best$summary$n_matched_periods,
    ev_best$summary$n_gold_periods,
    ev_best$summary$coverage_pct
  ))
  cat("Full-cohort metrics:\n")
  print(as.data.frame(ev_best$summary))
}

# ===========================================================================
# 8. Dose distribution plot
# ===========================================================================
message("\n=== Dose distributions ===")

dist_df_best <- best_episodes |>
  dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
  dplyr::transmute(person_id, drug_name_std, median_daily_dose,
                   method = "Adaptive Hierarchical")

gold_dist_df <- gold_std |>
  dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
  dplyr::transmute(person_id = as.integer(patient_id),
                   drug_name_std = dplyr::coalesce(drug_name_std, "unknown"),
                   median_daily_dose, method = "Gold")

dist_df_all <- dplyr::bind_rows(dist_df_best, gold_dist_df) |>
  dplyr::mutate(method = factor(method,
                  levels = c("Adaptive Hierarchical", "Gold")))

p_dist <- ggplot2::ggplot(
  dist_df_all,
  ggplot2::aes(x = median_daily_dose, fill = method, colour = method)
) +
  ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
  ggplot2::scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320),
    labels = scales::label_number()
  ) +
  ggplot2::scale_fill_manual(values   = c("Adaptive Hierarchical" = "#CC79A7",
                                          "Gold"                  = "#333333")) +
  ggplot2::scale_colour_manual(values = c("Adaptive Hierarchical" = "#CC79A7",
                                          "Gold"                  = "#333333")) +
  ggplot2::facet_wrap(~ method, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title    = "Adaptive hierarchical vs gold: dose distribution",
    subtitle = sprintf(
      "Best params: MATCH_TOL=%g | DIFF_THR=%g | OVERRIDE=%s | GAP=%d days",
      BEST_MATCH_TOL, BEST_DIFF_THRESHOLD, BEST_OVERRIDE, BEST_GAP_DAYS
    ),
    x = "Median daily dose (mg pred-equiv)",
    y = "Density"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

print(p_dist)

# Scatter plot (best method vs gold — full cohort)
if (!is.null(ev_best)) {
  scatter_df <- ev_best$comparison |>
    dplyr::filter(!is.na(computed_dose))

  p_scatter <- ggplot2::ggplot(
    scatter_df, ggplot2::aes(x = gold_dose, y = computed_dose)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(alpha = 0.5, size = 1.8, colour = "#CC79A7") +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
                         colour = "#d6604d", linewidth = 0.8) +
    ggplot2::labs(
      title    = "Adaptive hierarchical dose vs gold standard",
      subtitle = "Dashed = perfect agreement",
      x        = "Gold standard median daily dose (mg pred-equiv)",
      y        = "Adaptive hierarchical median daily dose (mg pred-equiv)"
    ) +
    ggplot2::theme_bw()

  print(p_scatter)
}

# ===========================================================================
# 9. Training-MAE heatmap (for visual diagnostics)
# ===========================================================================
message("\n=== Parameter sensitivity heatmap ===")

heatmap_df <- grid_results |>
  dplyr::filter(!is.na(mae_train)) |>
  dplyr::mutate(
    facet_label    = sprintf("override: %s | gap: %d d", override_prefer, gap_days),
    diff_threshold = factor(diff_threshold),
    match_tol      = factor(match_tol)
  )

if (nrow(heatmap_df) > 0L) {
  p_heatmap <- ggplot2::ggplot(
    heatmap_df,
    ggplot2::aes(x = match_tol, y = diff_threshold, fill = mae_train)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = round(mae_train, 1)),
                       size = 2.5, colour = "white") +
    ggplot2::scale_fill_viridis_c(direction = -1, option = "magma",
                                  name = "Training\nMAE (mg)") +
    ggplot2::facet_wrap(~ facet_label, ncol = length(OVERRIDE_PREFER_OPTS)) +
    ggplot2::labs(
      title    = "Training MAE by parameter combination",
      subtitle = sprintf(
        "Lower is better | Best: MATCH_TOL=%g, DIFF_THR=%g, OVERRIDE=%s, GAP=%d days (MAE=%.2f)",
        BEST_MATCH_TOL, BEST_DIFF_THRESHOLD, BEST_OVERRIDE, BEST_GAP_DAYS, BEST_MAE_TRAIN
      ),
      x = "MATCH_TOL (mg/day)",
      y = "DIFF_THRESHOLD (mg/day)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 8))

  print(p_heatmap)
}

# ===========================================================================
# 10. Report
# ===========================================================================
message("\n\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS REPORT — SteroidDoseR Adaptive Parameter Selection\n")
cat(strrep("=", 70), "\n\n")

cat("CONFIGURATION\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Train fraction:   %.0f%% (%d patients)\n",
            100 * TRAIN_FRACTION, length(train_pts)))
cat(sprintf("  Test fraction:    %.0f%% (%d patients)\n",
            100 * (1 - TRAIN_FRACTION), length(test_pts)))
cat(sprintf("  Random seed:      %d\n", RANDOM_SEED))
cat(sprintf("  Grid size:        %d combinations evaluated\n", n_grid))
cat(sprintf("  Study window:     %s to %s\n", START_DATE, END_DATE))
cat(sprintf("  Concurrent agg:   %s\n\n",     CONCURRENT_AGG))

cat("BEST PARAMETERS\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  MATCH_TOL:        %g mg/day\n", BEST_MATCH_TOL))
cat(sprintf("  DIFF_THRESHOLD:   %g mg/day\n", BEST_DIFF_THRESHOLD))
cat(sprintf("  OVERRIDE_PREFER:  %s\n",         BEST_OVERRIDE))
cat(sprintf("  GAP_DAYS:         %d days\n",    BEST_GAP_DAYS))
cat(sprintf("  Training MAE:     %.3f mg (on %d train patients)\n",
            BEST_MAE_TRAIN, length(train_pts)))
cat(sprintf("  Test MAE:         %s (on %d test patients)\n",
            if (is.na(BEST_MAE_TEST)) "N/A (no test patients)"
            else sprintf("%.3f mg", BEST_MAE_TEST),
            length(test_pts)))

if (!is.na(BEST_MAE_TEST) && !is.na(BEST_MAE_TRAIN)) {
  generalisation_gap <- BEST_MAE_TEST - BEST_MAE_TRAIN
  cat(sprintf(
    "  Generalisation:   %+.3f mg (test - train; near zero = well-generalised)\n\n",
    generalisation_gap
  ))
}

cat("PATIENT COUNTS\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  All computed patients:     %d\n", length(computed_pts)))
cat(sprintf("  Gold standard patients:    %d\n", length(gold_pts)))
cat(sprintf("  Matched patients:          %d\n", length(matched_pts)))
cat(sprintf("  Train patients:            %d\n", length(train_pts)))
cat(sprintf("  Test patients:             %d\n\n", length(test_pts)))

cat("ADAPTIVE HIERARCHICAL BRANCH BREAKDOWN (full cohort, best params)\n")
cat(strrep("-", 40), "\n")
branch_desc <- c(
  cross_checked      = "Both estimates matched (|diff| <= MATCH_TOL) → SIG value",
  blended            = "Small discrepancy → average of baseline and NLP",
  adaptive_override  = sprintf("Large discrepancy (|diff| > DIFF_THR) → %s value (learned)", BEST_OVERRIDE),
  baseline_only      = "SIG absent or unparseable → baseline value",
  nlp_fills_baseline = "Baseline incomplete, SIG complete (steady dosing) → NLP",
  nlp_taper          = "Baseline incomplete, SIG is taper → NLP taper",
  missing            = "Neither baseline nor SIG could compute a dose → NA",
  implausible        = "Dose > max_daily_dose_mg cap → NA"
)
for (m in method_tbl$hierarchical_method) {
  row  <- method_tbl[method_tbl$hierarchical_method == m, ]
  desc <- if (m %in% names(branch_desc)) branch_desc[[m]] else ""
  cat(sprintf("  %-22s %5d (%5.1f%%)  %s\n", m, row$n, row$pct, desc))
}
cat("\n")

if (!is.null(ev_best)) {
  cat("FULL-COHORT ACCURACY vs GOLD STANDARD\n")
  cat(strrep("-", 40), "\n")
  cat(sprintf("  Coverage:  %.1f%% (%d / %d gold episodes matched)\n",
              ev_best$summary$coverage_pct,
              ev_best$summary$n_matched_periods,
              ev_best$summary$n_gold_periods))
  cat(sprintf("  MAE:       %.2f mg\n",  ev_best$summary$MAE))
  cat(sprintf("  MBE:       %.2f mg\n",  ev_best$summary$MBE))
  cat(sprintf("  RMSE:      %.2f mg\n",  ev_best$summary$RMSE))
  cat(sprintf("  MAPE:      %.1f%%\n",   ev_best$summary$MAPE))
  cat(sprintf("  Pearson r: %.3f\n\n",   ev_best$summary$pearson_corr))
}

cat("GRID SEARCH SUMMARY\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  %d combinations evaluated\n", n_grid))
cat(sprintf("  MAE range: %.2f – %.2f mg\n",
            min(grid_ranked$mae_train, na.rm = TRUE),
            max(grid_ranked$mae_train, na.rm = TRUE)))
cat("\n  Top 5 combos:\n")
grid_ranked |>
  head(5) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3))) |>
  print()

cat(strrep("=", 70), "\n")

# ===========================================================================
# 11. Save results to timestamped run folder
# ===========================================================================
message("\n=== Saving results ===")

RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

# ── params.txt ───────────────────────────────────────────────────────────────
writeLines(c(
  "SteroidDoseR — Run Parameters",
  strrep("=", 40),
  sprintf("Script:              parameter_selection.R"),
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
  "Parameter selection",
  strrep("-", 40),
  sprintf("TRAIN_FRACTION:      %.2f", TRAIN_FRACTION),
  sprintf("RANDOM_SEED:         %d",   RANDOM_SEED),
  sprintf("Matched patients:    %d",   length(matched_pts)),
  sprintf("Train patients:      %d",   length(train_pts)),
  sprintf("Test patients:       %d",   length(test_pts)),
  sprintf("Grid size:           %d combinations", n_grid),
  sprintf("MATCH_TOL_GRID:      %s", paste(MATCH_TOL_GRID,      collapse = ", ")),
  sprintf("DIFF_THRESHOLD_GRID: %s", paste(DIFF_THRESHOLD_GRID, collapse = ", ")),
  sprintf("OVERRIDE_PREFER_OPTS:%s", paste(OVERRIDE_PREFER_OPTS, collapse = ", ")),
  sprintf("GAP_DAYS_GRID:       %s", paste(GAP_DAYS_GRID,        collapse = ", ")),
  "",
  "Best parameters (from training set)",
  strrep("-", 40),
  sprintf("BEST_MATCH_TOL:      %g mg/day",  BEST_MATCH_TOL),
  sprintf("BEST_DIFF_THRESHOLD: %g mg/day",  BEST_DIFF_THRESHOLD),
  sprintf("BEST_OVERRIDE:       %s",          BEST_OVERRIDE),
  sprintf("BEST_GAP_DAYS:       %d days",    BEST_GAP_DAYS),
  sprintf("Training MAE:        %.3f mg",    BEST_MAE_TRAIN),
  sprintf("Test MAE:            %s",
          if (is.na(BEST_MAE_TEST)) "N/A" else sprintf("%.3f mg", BEST_MAE_TEST)),
  "",
  "Episode building",
  strrep("-", 40),
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

# ── Grid search results ───────────────────────────────────────────────────────
readr::write_csv(grid_results,  file.path(RUN_DIR, "grid_search_results.csv"))
readr::write_csv(grid_ranked,   file.path(RUN_DIR, "grid_search_ranked.csv"))

# ── Best-params record-level and episode-level data ──────────────────────────
readr::write_csv(best_df,       file.path(RUN_DIR, "records_adaptive.csv"))
readr::write_csv(best_episodes, file.path(RUN_DIR, "episodes_adaptive.csv"))

# ── Gold standard ─────────────────────────────────────────────────────────────
readr::write_csv(gold_std,      file.path(RUN_DIR, "gold_standard.csv"))

# ── Train / test split ────────────────────────────────────────────────────────
readr::write_csv(
  tibble::tibble(person_id = train_pts, split = "train"),
  file.path(RUN_DIR, "patient_split_train.csv")
)
readr::write_csv(
  tibble::tibble(person_id = test_pts, split = "test"),
  file.path(RUN_DIR, "patient_split_test.csv")
)

# ── Evaluation comparison table ───────────────────────────────────────────────
if (!is.null(ev_best)) {
  readr::write_csv(ev_best$comparison, file.path(RUN_DIR, "comparison_adaptive.csv"))
}

# ── Method breakdown ──────────────────────────────────────────────────────────
readr::write_csv(method_tbl,    file.path(RUN_DIR, "branch_summary.csv"))

# ── Figures ───────────────────────────────────────────────────────────────────
ggplot2::ggsave(file.path(RUN_DIR, "plot_distribution.png"),
                p_dist, width = 8, height = 6, dpi = 150)

if (nrow(heatmap_df) > 0L) {
  ggplot2::ggsave(file.path(RUN_DIR, "plot_parameter_heatmap.png"),
                  p_heatmap,
                  width  = 5 * length(OVERRIDE_PREFER_OPTS),
                  height = 3 * length(GAP_DAYS_GRID),
                  dpi    = 150)
}

if (!is.null(ev_best)) {
  ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter_vs_gold.png"),
                  p_scatter, width = 7, height = 6, dpi = 150)
}

message(sprintf("Results saved to: %s", RUN_DIR))

# ===========================================================================
# 12. Disconnect (live DB only)
# ===========================================================================
if (!USE_SYNTHETIC) {
  if (inherits(conn, "DatabaseConnectorConnection")) {
    DatabaseConnector::disconnect(conn)
  } else {
    DBI::dbDisconnect(conn)
  }
}

message("\n=== Parameter selection complete ===")
