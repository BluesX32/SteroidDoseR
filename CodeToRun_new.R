# CodeToRun_new.R
# SteroidDoseR — Gold-Anchored Episode Comparison
#
# Replaces the gap-bridging episode construction with a gold-anchored approach:
#
#   For each gold-standard episode, find every computed drug-exposure record
#   that overlaps it.  Clip each record to the gold window, day-expand, sum
#   doses per calendar day (handles concurrent records correctly), then:
#
#     cumulative_dose  = sum of per-day doses within the gold window
#     avg_daily_dose   = cumulative_dose / gold_duration_days
#
#   The denominator is always the full gold duration, so days with no matching
#   record contribute 0 dose (not ignored).  This makes coverage gaps visible
#   in the avg_daily_dose rather than hiding them.
#
# Compared side-by-side with the original gap-bridging method so both
# approaches can be evaluated against the same gold standard.
#
# Usage
# -----
#   Source interactively in RStudio, or:
#     Rscript CodeToRun_new.R
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

# ===========================================================================
# 0. Configuration
# ===========================================================================
USE_SYNTHETIC  <- FALSE
START_DATE     <- "2015-01-01"
END_DATE       <- "2025-12-31"
CONCURRENT_AGG <- "per_drug"

# -- Hierarchical method parameters ------------------------------------------
DIFF_THRESHOLD <- 5
MATCH_TOL      <- 0.01

# -- Gold-anchored comparison ------------------------------------------------
# MIN_COVERAGE_PCT: gold episodes where computed records cover fewer than this
# fraction of days are flagged as low-coverage (dose estimate unreliable).
MIN_COVERAGE_PCT <- 50   # percent

GOLD_STD_PATH  <- "/your/path/to/gold-standard"
OUTPUT_DIR     <- file.path(getwd(), "output_new")

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
  # conn <- DBI::dbConnect(odbc::odbc(), ...)
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
  "Fetched %d rows | %d unique persons",
  nrow(drug_df), length(unique(drug_df$person_id))
))

drug_df <- drug_df |>
  dplyr::mutate(drug_name_std = standardize_drug_name(drug_concept_name))

# ===========================================================================
# 2. Compute record-level doses (hierarchical method)
# ===========================================================================
message("\n=== Computing record-level doses (hierarchical) ===")

hier_df <- calc_daily_dose_hierarchical(
  drug_df,
  diff_threshold    = DIFF_THRESHOLD,
  match_tol         = MATCH_TOL,
  max_daily_dose_mg = 2000,
  filter_oral       = TRUE
)

hier_df_eq <- convert_pred_equiv(
  hier_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg"
)

message(sprintf(
  "  Records with dose: %d / %d (%.1f%%)",
  sum(!is.na(hier_df_eq$pred_equiv_mg)),
  nrow(hier_df_eq),
  100 * mean(!is.na(hier_df_eq$pred_equiv_mg))
))

# ===========================================================================
# 3. Load gold standard and convert to pred-equiv
# ===========================================================================
message("\n=== Loading gold standard ===")

gold_std <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)
gold_std$episode_start <- as.Date(gold_std$episode_start)
gold_std$episode_end   <- as.Date(gold_std$episode_end)

cat(sprintf(
  "Gold standard: %d episodes from %d patients\n",
  nrow(gold_std), dplyr::n_distinct(gold_std$patient_id)
))

# Drug name mapping for pred-equiv conversion
gold_drug_map <- hier_df_eq |>
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
    median_daily_dose = dplyr::coalesce(gold_pred_equiv_mg, median_daily_dose),
    gold_duration_days = as.integer(episode_end - episode_start) + 1L
  )

# ===========================================================================
# 4. Gold-anchored comparison  (new method)
# ===========================================================================
# For each gold episode, clip every overlapping computed record to the gold
# window, day-expand, sum doses per calendar day (handles concurrent records),
# then aggregate:
#   cumulative_dose  = sum of per-day totals within the gold window
#   avg_daily_dose   = cumulative_dose / gold_duration_days
#   coverage_days    = number of days with at least one record
# ===========================================================================
message("\n=== Gold-anchored comparison (new method) ===")

compute_gold_anchored <- function(records_df, gold_df) {
  # records_df needs: person_id, drug_exposure_start_date,
  #                   drug_exposure_end_date, pred_equiv_mg
  # gold_df needs:   patient_id, episode_start, episode_end

  # Step 1: cross-join patients, filter to overlapping pairs
  overlapping <- records_df |>
    dplyr::transmute(
      pt_id      = as.integer(person_id),
      rec_start  = drug_exposure_start_date,
      rec_end    = drug_exposure_end_date,
      dose       = pred_equiv_mg
    ) |>
    dplyr::inner_join(
      gold_df |>
        dplyr::transmute(
          pt_id      = as.integer(patient_id),
          g_start    = episode_start,
          g_end      = episode_end,
          gold_dur   = gold_duration_days,
          gold_dose  = median_daily_dose
        ),
      by = "pt_id", relationship = "many-to-many"
    ) |>
    dplyr::filter(rec_start <= g_end, rec_end >= g_start)

  if (nrow(overlapping) == 0L) {
    warning("No records overlap any gold episode.")
    return(gold_df |>
             dplyr::transmute(
               patient_id           = as.integer(patient_id),
               episode_start, episode_end,
               gold_duration_days, gold_dose = median_daily_dose,
               n_records            = 0L,
               coverage_days        = 0L,
               coverage_pct         = 0,
               cumulative_dose_mg   = NA_real_,
               avg_daily_dose_mg    = NA_real_
             ))
  }

  # Step 2: clip each record to the gold window
  overlapping <- overlapping |>
    dplyr::mutate(
      clip_start = pmax(rec_start, g_start),
      clip_end   = pmin(rec_end,   g_end)
    )

  # Step 3: day-expand clipped records → one row per (pt_id, g_start, day)
  day_rows <- overlapping |>
    dplyr::rowwise() |>
    dplyr::mutate(
      day = list(seq.Date(clip_start, clip_end, by = "day"))
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = day)

  # Step 4: sum doses per (patient, gold episode, calendar day)
  # Handles concurrent prescriptions on the same day correctly
  daily_totals <- day_rows |>
    dplyr::group_by(pt_id, g_start, g_end, gold_dur, gold_dose, day) |>
    dplyr::summarise(
      day_dose = sum(dose, na.rm = TRUE),
      .groups  = "drop"
    )

  # Step 5: aggregate per gold episode
  episode_summary <- daily_totals |>
    dplyr::group_by(pt_id, g_start, g_end, gold_dur, gold_dose) |>
    dplyr::summarise(
      coverage_days      = dplyr::n(),
      cumulative_dose_mg = sum(day_dose, na.rm = TRUE),
      .groups            = "drop"
    ) |>
    dplyr::mutate(
      coverage_pct      = round(100 * coverage_days / gold_dur, 1),
      avg_daily_dose_mg = round(cumulative_dose_mg / gold_dur, 3)
    )

  # Step 6: join back record count per gold episode
  n_records_per_ep <- overlapping |>
    dplyr::group_by(pt_id, g_start, g_end) |>
    dplyr::summarise(n_records = dplyr::n(), .groups = "drop")

  episode_summary <- episode_summary |>
    dplyr::left_join(n_records_per_ep, by = c("pt_id", "g_start", "g_end"))

  # Step 7: left-join back to all gold episodes (keep unmatched gold as NA)
  gold_df |>
    dplyr::transmute(
      patient_id         = as.integer(patient_id),
      episode_start      = episode_start,
      episode_end        = episode_end,
      gold_duration_days = gold_duration_days,
      gold_dose          = median_daily_dose
    ) |>
    dplyr::left_join(
      episode_summary |>
        dplyr::transmute(
          patient_id         = pt_id,
          episode_start      = g_start,
          episode_end        = g_end,
          n_records, coverage_days, coverage_pct,
          cumulative_dose_mg, avg_daily_dose_mg
        ),
      by = c("patient_id", "episode_start", "episode_end")
    ) |>
    dplyr::mutate(
      n_records     = dplyr::coalesce(n_records, 0L),
      coverage_days = dplyr::coalesce(coverage_days, 0L),
      coverage_pct  = dplyr::coalesce(coverage_pct,  0)
    )
}

new_comparison <- compute_gold_anchored(hier_df_eq, gold_std)

# Error metrics
new_matched <- new_comparison |> dplyr::filter(!is.na(avg_daily_dose_mg))

new_comparison <- new_comparison |>
  dplyr::mutate(
    absolute_error             = abs(avg_daily_dose_mg - gold_dose),
    bias_error                 = avg_daily_dose_mg - gold_dose,
    relative_error_pct         = (avg_daily_dose_mg - gold_dose) / gold_dose * 100,
    absolute_relative_error_pct = abs(relative_error_pct),
    low_coverage               = coverage_pct < MIN_COVERAGE_PCT,
    agreement_category = dplyr::case_when(
      absolute_relative_error_pct <= 5  ~ "Exact (<=5%)",
      absolute_relative_error_pct <= 20 ~ "Good (<=20%)",
      absolute_relative_error_pct <= 50 ~ "Moderate (<=50%)",
      !is.na(absolute_relative_error_pct) ~ "Poor (>50%)",
      TRUE ~ NA_character_
    )
  )

cat("\nGold-anchored comparison summary:\n")
cat(sprintf("  Gold episodes:   %d\n", nrow(new_comparison)))
cat(sprintf("  Matched:         %d (%.1f%%)\n",
            sum(!is.na(new_comparison$avg_daily_dose_mg)),
            100 * mean(!is.na(new_comparison$avg_daily_dose_mg))))
cat(sprintf("  Low coverage (<%.0f%%): %d episodes\n",
            MIN_COVERAGE_PCT,
            sum(new_comparison$low_coverage, na.rm = TRUE)))
cat(sprintf("  MAE:             %.2f mg\n",
            mean(new_comparison$absolute_error, na.rm = TRUE)))
cat(sprintf("  RMSE:            %.2f mg\n",
            sqrt(mean(new_comparison$bias_error^2, na.rm = TRUE))))
cat(sprintf("  MAPE:            %.1f%%\n",
            mean(new_comparison$absolute_relative_error_pct, na.rm = TRUE)))

cat("\nCoverage distribution:\n")
print(summary(new_comparison$coverage_pct))

cat("\nAgreement category breakdown:\n")
new_comparison |>
  dplyr::filter(!is.na(agreement_category)) |>
  dplyr::count(agreement_category) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

# ===========================================================================
# 5. Gap-bridging method  (old method, for comparison)
# ===========================================================================
message("\n=== Gap-bridging episodes (old method, for comparison) ===")

# Default GAP_DAYS = 30 kept for comparison
old_episodes <- build_episodes(
  hier_df_eq,
  end_col        = "drug_exposure_end_date",
  dose_col       = "pred_equiv_mg",
  gap_days       = 30L,
  concurrent_agg = CONCURRENT_AGG
)

old_ev <- evaluate_against_gold(old_episodes, gold_std, gold_id_col = "patient_id")

cat("\nGap-bridging summary:\n")
cat(sprintf("  Computed episodes: %d from %d patients\n",
            nrow(old_episodes), dplyr::n_distinct(old_episodes$person_id)))
cat(sprintf("  Coverage: %.1f%% | MAE: %.2f mg | RMSE: %.2f mg | MAPE: %.1f%%\n",
            old_ev$summary$coverage_pct,
            old_ev$summary$MAE,
            old_ev$summary$RMSE,
            old_ev$summary$MAPE))

# ===========================================================================
# 6. Side-by-side comparison
# ===========================================================================
message("\n=== Method comparison ===")

metrics_tbl <- tibble::tibble(
  Method          = c("Gold-anchored (new)", "Gap-bridging 30d (old)"),
  N_gold_episodes = c(nrow(new_comparison),
                       old_ev$summary$n_gold_periods),
  N_matched       = c(sum(!is.na(new_comparison$avg_daily_dose_mg)),
                       old_ev$summary$n_matched_periods),
  Coverage_pct    = round(c(
    100 * mean(!is.na(new_comparison$avg_daily_dose_mg)),
    old_ev$summary$coverage_pct), 1),
  MAE_mg          = round(c(
    mean(new_comparison$absolute_error,              na.rm = TRUE),
    old_ev$summary$MAE), 2),
  RMSE_mg         = round(c(
    sqrt(mean(new_comparison$bias_error^2,           na.rm = TRUE)),
    old_ev$summary$RMSE), 2),
  MBE_mg          = round(c(
    mean(new_comparison$bias_error,                  na.rm = TRUE),
    old_ev$summary$MBE), 2),
  MAPE_pct        = round(c(
    mean(new_comparison$absolute_relative_error_pct, na.rm = TRUE),
    old_ev$summary$MAPE), 1)
)

cat("\nMethod comparison:\n")
print(as.data.frame(metrics_tbl), row.names = FALSE)

# ===========================================================================
# 7. Plots
# ===========================================================================
message("\n=== Generating plots ===")

agree_levels <- c("Exact (<=5%)", "Good (<=20%)", "Moderate (<=50%)", "Poor (>50%)")
agree_colors <- c("#2166AC", "#92C5DE", "#F4A582", "#D6604D")

# ── Plot 1: Scatter — new method vs gold ─────────────────────────────────────
p_scatter_new <- new_comparison |>
  dplyr::filter(!is.na(avg_daily_dose_mg)) |>
  ggplot2::ggplot(ggplot2::aes(x = gold_dose, y = avg_daily_dose_mg,
                               colour = low_coverage)) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(alpha = 0.55, size = 1.8) +
  ggplot2::scale_colour_manual(
    values = c("TRUE" = "#D55E00", "FALSE" = "#2271B3"),
    labels = c("TRUE" = sprintf("< %d%% covered", MIN_COVERAGE_PCT),
               "FALSE" = sprintf(">= %d%% covered", MIN_COVERAGE_PCT)),
    name = "Record\ncoverage"
  ) +
  ggplot2::geom_smooth(data = ~ dplyr::filter(., !low_coverage),
                       method = "lm", se = TRUE,
                       colour = "#d6604d", linewidth = 0.8) +
  ggplot2::labs(
    title    = "Gold-anchored avg daily dose vs gold standard",
    subtitle = sprintf("Orange = low coverage (< %d%% of gold window covered by records)",
                       MIN_COVERAGE_PCT),
    x        = "Gold standard daily dose (mg pred-equiv)",
    y        = "Computed avg daily dose (mg pred-equiv)"
  ) +
  ggplot2::theme_bw()

print(p_scatter_new)

# ── Plot 2: Side-by-side scatter (new vs old) ─────────────────────────────────
old_scatter <- old_ev$comparison |>
  dplyr::filter(!is.na(computed_dose)) |>
  dplyr::transmute(gold_dose, computed_dose, method = "Gap-bridging 30d (old)")

new_scatter <- new_comparison |>
  dplyr::filter(!is.na(avg_daily_dose_mg)) |>
  dplyr::transmute(gold_dose, computed_dose = avg_daily_dose_mg,
                   method = "Gold-anchored (new)")

scatter_both <- dplyr::bind_rows(old_scatter, new_scatter) |>
  dplyr::mutate(method = factor(method,
                  levels = c("Gold-anchored (new)", "Gap-bridging 30d (old)")))

p_scatter_both <- ggplot2::ggplot(
  scatter_both, ggplot2::aes(x = gold_dose, y = computed_dose)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(alpha = 0.45, size = 1.6, colour = "#2271B3") +
  ggplot2::geom_smooth(method = "lm", se = TRUE,
                       colour = "#d6604d", linewidth = 0.8) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Computed dose vs gold: new vs old episode method",
    subtitle = "Dashed = perfect agreement",
    x        = "Gold standard daily dose (mg pred-equiv)",
    y        = "Computed daily dose (mg pred-equiv)"
  ) +
  ggplot2::theme_bw()

print(p_scatter_both)

# ── Plot 3: Agreement category comparison ─────────────────────────────────────
agree_new <- new_comparison |>
  dplyr::filter(!is.na(agreement_category)) |>
  dplyr::count(agreement_category) |>
  dplyr::mutate(pct = 100 * n / sum(n), method = "Gold-anchored (new)")

agree_old <- old_ev$comparison |>
  dplyr::filter(!is.na(agreement_category)) |>
  dplyr::count(agreement_category) |>
  dplyr::mutate(pct = 100 * n / sum(n), method = "Gap-bridging 30d (old)")

agree_both <- dplyr::bind_rows(agree_new, agree_old) |>
  dplyr::mutate(
    agreement_category = factor(agreement_category, levels = rev(agree_levels)),
    method = factor(method,
                    levels = c("Gold-anchored (new)", "Gap-bridging 30d (old)"))
  )

p_agree <- ggplot2::ggplot(
  agree_both, ggplot2::aes(x = method, y = pct, fill = agreement_category)
) +
  ggplot2::geom_col(position = "stack", width = 0.6) +
  ggplot2::scale_fill_manual(
    values = setNames(rev(agree_colors), rev(agree_levels)),
    name   = "Agreement"
  ) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct)),
                     position = ggplot2::position_stack(vjust = 0.5),
                     size = 3.2, colour = "white", fontface = "bold") +
  ggplot2::labs(
    title = "Agreement distribution: new vs old method",
    x = NULL, y = "% of matched gold episodes"
  ) +
  ggplot2::theme_bw()

print(p_agree)

# ── Plot 4: Coverage distribution (new method) ───────────────────────────────
p_coverage <- new_comparison |>
  dplyr::filter(!is.na(coverage_pct)) |>
  ggplot2::ggplot(ggplot2::aes(x = coverage_pct)) +
  ggplot2::geom_histogram(binwidth = 5, fill = "#2271B3", colour = "white") +
  ggplot2::geom_vline(xintercept = MIN_COVERAGE_PCT,
                      linetype = "dashed", colour = "#D55E00", linewidth = 0.8) +
  ggplot2::annotate("text", x = MIN_COVERAGE_PCT + 1, y = Inf,
                    label = sprintf("Threshold: %d%%", MIN_COVERAGE_PCT),
                    hjust = 0, vjust = 1.5, colour = "#D55E00", size = 3.5) +
  ggplot2::labs(
    title    = "Record coverage per gold episode (new method)",
    subtitle = "Coverage = days with at least one computed record / gold duration",
    x        = "Coverage (%)",
    y        = "Number of gold episodes"
  ) +
  ggplot2::theme_bw()

print(p_coverage)

# ── Plot 5: Error vs coverage (does low coverage → high error?) ───────────────
p_cov_err <- new_comparison |>
  dplyr::filter(!is.na(absolute_error)) |>
  ggplot2::ggplot(ggplot2::aes(x = coverage_pct, y = absolute_error,
                               colour = low_coverage)) +
  ggplot2::geom_point(alpha = 0.5, size = 1.6) +
  ggplot2::scale_colour_manual(
    values = c("TRUE" = "#D55E00", "FALSE" = "#2271B3"),
    labels = c("TRUE" = "Low coverage", "FALSE" = "Adequate coverage"),
    name = NULL
  ) +
  ggplot2::geom_vline(xintercept = MIN_COVERAGE_PCT,
                      linetype = "dashed", colour = "#D55E00") +
  ggplot2::labs(
    title    = "Absolute dose error vs record coverage",
    subtitle = "Low coverage does not necessarily mean high error",
    x        = "Coverage (% of gold window days covered)",
    y        = "Absolute error (mg pred-equiv)"
  ) +
  ggplot2::theme_bw()

print(p_cov_err)

# ===========================================================================
# 8. Report
# ===========================================================================
message("\n\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS REPORT — Gold-Anchored Episode Comparison\n")
cat(strrep("=", 70), "\n\n")

cat("CONFIGURATION\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Study window:       %s to %s\n", START_DATE, END_DATE))
cat(sprintf("  MIN_COVERAGE_PCT:   %d%%  (flag threshold)\n", MIN_COVERAGE_PCT))
cat(sprintf("  Records with dose:  %d / %d\n",
            sum(!is.na(hier_df_eq$pred_equiv_mg)), nrow(hier_df_eq)))
cat(sprintf("  Gold episodes:      %d from %d patients\n\n",
            nrow(gold_std), dplyr::n_distinct(gold_std$patient_id)))

cat("METHOD COMPARISON\n")
cat(strrep("-", 40), "\n")
print(as.data.frame(metrics_tbl), row.names = FALSE)

cat("\nINTERPRETATION\n")
cat(strrep("-", 40), "\n")

new_mae <- mean(new_comparison$absolute_error, na.rm = TRUE)
old_mae <- old_ev$summary$MAE
delta   <- new_mae - old_mae
cat(sprintf(
  "  MAE: gold-anchored = %.2f mg | gap-bridging = %.2f mg | delta = %+.2f mg\n",
  new_mae, old_mae, delta
))

n_low_cov <- sum(new_comparison$low_coverage, na.rm = TRUE)
cat(sprintf(
  "  Low-coverage episodes (< %d%%): %d / %d (%.1f%%) — ",
  MIN_COVERAGE_PCT, n_low_cov, nrow(new_comparison),
  100 * n_low_cov / nrow(new_comparison)
))
if (n_low_cov > 0L) {
  low_mae <- mean(new_comparison$absolute_error[new_comparison$low_coverage],
                  na.rm = TRUE)
  hi_mae  <- mean(new_comparison$absolute_error[!new_comparison$low_coverage],
                  na.rm = TRUE)
  cat(sprintf("MAE low=%.2f mg vs adequate=%.2f mg\n", low_mae, hi_mae))
} else {
  cat("none\n")
}

cat(strrep("=", 70), "\n")

# ===========================================================================
# 9. Save results
# ===========================================================================
message("\n=== Saving results ===")

RUN_DIR <- file.path(OUTPUT_DIR, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
dir.create(RUN_DIR, recursive = TRUE)

writeLines(c(
  "SteroidDoseR — Run Parameters",
  strrep("=", 40),
  sprintf("Script:              CodeToRun_new.R"),
  sprintf("Run time:            %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Data",
  strrep("-", 40),
  sprintf("USE_SYNTHETIC:       %s", USE_SYNTHETIC),
  sprintf("START_DATE:          %s", START_DATE),
  sprintf("END_DATE:            %s", END_DATE),
  sprintf("STEROID_CONCEPT_IDS: %d concept IDs", length(STEROID_CONCEPT_IDS)),
  sprintf("Drug-exposure rows:  %d", nrow(drug_df)),
  sprintf("Unique patients:     %d", dplyr::n_distinct(drug_df$person_id)),
  "",
  "Hierarchical method",
  strrep("-", 40),
  sprintf("DIFF_THRESHOLD:      %g mg/day", DIFF_THRESHOLD),
  sprintf("MATCH_TOL:           %g mg/day", MATCH_TOL),
  "",
  "Gold-anchored comparison",
  strrep("-", 40),
  sprintf("MIN_COVERAGE_PCT:    %d%%", MIN_COVERAGE_PCT),
  sprintf("Gold episodes:       %d", nrow(gold_std)),
  sprintf("Gold patients:       %d", dplyr::n_distinct(gold_std$patient_id)),
  "",
  "Results",
  strrep("-", 40),
  sprintf("New MAE:             %.2f mg", new_mae),
  sprintf("Old MAE:             %.2f mg", old_mae),
  sprintf("Delta MAE:           %+.2f mg", delta),
  sprintf("Low-coverage epis:   %d (%.1f%%)",
          n_low_cov, 100 * n_low_cov / nrow(new_comparison))
), con = file.path(RUN_DIR, "params.txt"))

readr::write_csv(new_comparison,       file.path(RUN_DIR, "gold_anchored_comparison.csv"))
readr::write_csv(old_ev$comparison,    file.path(RUN_DIR, "gap_bridging_comparison.csv"))
readr::write_csv(metrics_tbl,          file.path(RUN_DIR, "method_comparison.csv"))
readr::write_csv(hier_df_eq,           file.path(RUN_DIR, "records_hierarchical.csv"))

ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter_new.png"),
                p_scatter_new,  width = 8, height = 6, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter_both.png"),
                p_scatter_both, width = 10, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_agreement.png"),
                p_agree,        width = 7, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_coverage.png"),
                p_coverage,     width = 7, height = 5, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_error_vs_coverage.png"),
                p_cov_err,      width = 7, height = 5, dpi = 150)

message(sprintf("Results saved to: %s", RUN_DIR))

# ===========================================================================
# 10. Disconnect (live DB only)
# ===========================================================================
if (!USE_SYNTHETIC && exists("conn")) {
  if (inherits(conn, "DatabaseConnectorConnection"))
    DatabaseConnector::disconnect(conn)
  else
    DBI::dbDisconnect(conn)
}

message("\n=== Analysis complete ===")
