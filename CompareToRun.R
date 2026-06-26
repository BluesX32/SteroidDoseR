# CompareToRun.R
# SteroidDoseR — Three-Way Comparison Against Gold Standard
#
# Compares Baseline, NLP, and LLM episode results against a manually reviewed
# gold standard.  Produces dose-distribution plots, scatter plots, Bland-Altman
# plots, sensitivity analyses, a structured report, and an interactive dashboard.
#
# Expects the following objects in the R session (set by RunAll.R / CodeToRun.R
# / LLMtoRun.R), or reads the corresponding CSVs from RUN_DIR if they are absent:
#
#   baseline_episodes, nlp_episodes, llm_episodes   — episode data frames
#   drug_df                                          — raw drug-exposure records
#   nlp_df                                           — NLP record-level output
#   RUN_DIR, GOLD_STD_PATH, GOLD_NEG_PATH, GAP_DAYS

if (!exists(".RUNALL_ACTIVE")) {
  if (!interactive()) quit(status = 0L, save = "no")
  library(SteroidDoseR)
  library(dplyr)
  library(ggplot2)
}

# ---------------------------------------------------------------------------
# 0. Configuration defaults (RunAll.R values take precedence)
# ---------------------------------------------------------------------------
if (!exists("GOLD_STD_PATH"))  GOLD_STD_PATH  <- "/your/path/to/gold-standard.csv"
if (!exists("GOLD_NEG_PATH"))  GOLD_NEG_PATH  <- "/your/path/to/gold-negative.csv"
if (!exists("GAP_DAYS"))       GAP_DAYS       <- 30L
if (!exists("OUTPUT_DIR"))     OUTPUT_DIR     <- file.path(getwd(), "output")
if (!exists("RUN_DIR")) {
  # Standalone: load from the most recent run folder
  run_dirs <- sort(list.dirs(OUTPUT_DIR, recursive = FALSE), decreasing = TRUE)
  if (length(run_dirs) == 0L)
    stop("No run folders in OUTPUT_DIR. Run CodeToRun.R + LLMtoRun.R first.")
  RUN_DIR <- run_dirs[[1L]]
  message("CompareToRun.R: loading from ", RUN_DIR)
}

# ---------------------------------------------------------------------------
# 1. Load episode objects (from memory or CSV fallback)
# ---------------------------------------------------------------------------
.load_or_use <- function(obj_name, csv_name) {
  if (exists(obj_name)) return(get(obj_name))
  path <- file.path(RUN_DIR, csv_name)
  if (!file.exists(path))
    stop(sprintf("'%s' not in memory and '%s' not found.", obj_name, path))
  readr::read_csv(path, show_col_types = FALSE)
}

baseline_episodes <- .load_or_use("baseline_episodes", "episodes_baseline.csv")
nlp_episodes      <- .load_or_use("nlp_episodes",      "episodes_nlp.csv")
llm_episodes      <- tryCatch(
  .load_or_use("llm_episodes", "episodes_llm.csv"),
  error = function(e) {
    message("LLM episodes not available — two-method comparison only.")
    NULL
  }
)

# ---------------------------------------------------------------------------
# 2. Load gold standard
# ---------------------------------------------------------------------------
message("\n=== Loading gold standard ===")

gold_std_raw <- readr::read_csv(GOLD_STD_PATH, show_col_types = FALSE)
gold_std     <- parse_steroid_gold(gold_std_raw)

cat(sprintf("Gold standard: %d records from %d patients\n",
            nrow(gold_std), dplyr::n_distinct(gold_std$person_id)))
cat("\nParse status breakdown:\n")
print(table(gold_std$parse_status, useNA = "ifany"))
cat("\nDaily mg equivalent distribution (parseable):\n")
print(summary(gold_std$dose_daily_mg_equiv[gold_std$parse_status == "ok"]))

gold_std_ok <- gold_std |> dplyr::filter(parse_status == "ok")

# ---------------------------------------------------------------------------
# 3. Helpers
# ---------------------------------------------------------------------------
METHOD_COLORS <- c(
  "Baseline" = "#2271B3",
  "NLP"      = "#009E73",
  "LLM"      = "#E69F00",
  "Gold"     = "#333333"
)

.flag_summary <- function(ep, label) {
  n_impl  <- sum(ep$dose_implausible, na.rm = TRUE)
  n_pulse <- sum(ep$pulse_episode,    na.rm = TRUE)
  cat(sprintf(
    "\n%s flags: %d dose_implausible (<1 mg/day, %.1f%%)  |  %d pulse (>100 mg/day, %.1f%%)  of %d episodes\n",
    label,
    n_impl,  100 * n_impl  / nrow(ep),
    n_pulse, 100 * n_pulse / nrow(ep),
    nrow(ep)
  ))
}

.run_comparison <- function(episodes_df, label) {
  if (is.null(episodes_df) || nrow(episodes_df) == 0L) {
    message("  Skipping ", label, " — no episodes.")
    return(NULL)
  }
  n_impl <- sum(episodes_df$dose_implausible, na.rm = TRUE)
  if (n_impl > 0L)
    cat(sprintf("  [%s] Excluding %d dose_implausible episodes\n", label, n_impl))

  episodes_clean <- episodes_df |> dplyr::filter(!.data$dose_implausible)

  ev <- evaluate_against_gold(episodes_clean, gold_std_ok,
                              gold_dose_col = "dose_daily_mg_equiv")
  s  <- ev$summary
  cat(sprintf(
    "\n%s: %d/%d gold patients (%.1f%% detection) | %d/%d episodes matched (%.1f%% coverage)\n",
    label,
    s$n_common_patients, s$n_gold_patients,  s$detection_coverage_pct,
    s$n_matched_periods, s$n_gold_periods,   s$coverage_pct
  ))
  cat(sprintf("  MAE=%.2f  MBE=%.2f  RMSE=%.2f  MAPE=%.1f%%  r=%.3f\n",
              s$MAE, s$MBE, s$RMSE, s$MAPE, s$pearson_corr))

  n_pulse <- sum(episodes_df$pulse_episode, na.rm = TRUE)
  if (n_pulse > 0L) {
    ev_np <- evaluate_against_gold(
      episodes_clean |> dplyr::filter(!.data$pulse_episode),
      gold_std_ok, gold_dose_col = "dose_daily_mg_equiv"
    )
    cat(sprintf("  Excl. pulse (>100 mg, n=%d): MAE=%.2f  MBE=%.2f  Coverage=%.1f%%\n",
                n_pulse, ev_np$summary$MAE, ev_np$summary$MBE, ev_np$summary$coverage_pct))
  }
  cat("By dose range:\n")
  print(as.data.frame(ev$stratified$by_dose_range))
  ev
}

# ---------------------------------------------------------------------------
# 4. Plausibility flags
# ---------------------------------------------------------------------------
message("\n=== Plausibility flags ===")
.flag_summary(baseline_episodes, "Baseline")
.flag_summary(nlp_episodes,      "NLP")
if (!is.null(llm_episodes)) .flag_summary(llm_episodes, "LLM")

# ---------------------------------------------------------------------------
# 5. Dose distributions
# ---------------------------------------------------------------------------
message("\n=== Dose distributions ===")

make_dist_df <- function(ep, label) {
  if (is.null(ep) || nrow(ep) == 0L) return(NULL)
  ep |>
    dplyr::filter(!is.na(median_daily_dose), median_daily_dose > 0) |>
    dplyr::select(person_id, drug_name_std, median_daily_dose) |>
    dplyr::mutate(method = label)
}

gold_dist_df <- gold_std |>
  dplyr::filter(!is.na(dose_daily_mg_equiv), dose_daily_mg_equiv > 0) |>
  dplyr::transmute(
    person_id         = as.integer(person_id),
    drug_name_std     = dplyr::coalesce(drug_name_std, "unknown"),
    median_daily_dose = dose_daily_mg_equiv,
    method            = "Gold"
  )

method_levels <- c("Baseline", "NLP", "LLM", "Gold")
dist_df_all <- dplyr::bind_rows(
  make_dist_df(baseline_episodes, "Baseline"),
  make_dist_df(nlp_episodes,      "NLP"),
  make_dist_df(llm_episodes,      "LLM"),
  gold_dist_df
) |> dplyr::mutate(method = factor(method, levels = method_levels))

cat("\nDose summary by method (mg prednisone-equivalent/day):\n")
dist_df_all |>
  dplyr::filter(method != "Gold") |>
  dplyr::group_by(method) |>
  dplyr::summarise(
    n_episodes = dplyr::n(),
    n_patients = dplyr::n_distinct(person_id),
    q25 = stats::quantile(median_daily_dose, 0.25),
    median = stats::median(median_daily_dose),
    mean   = mean(median_daily_dose),
    q75 = stats::quantile(median_daily_dose, 0.75),
    .groups = "drop"
  ) |> print()

p_dist <- ggplot2::ggplot(
  dist_df_all,
  ggplot2::aes(x = median_daily_dose, fill = method, colour = method)
) +
  ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
  ggplot2::scale_x_log10(breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320, 640),
                         labels = scales::label_number()) +
  ggplot2::scale_fill_manual(values   = METHOD_COLORS) +
  ggplot2::scale_colour_manual(values = METHOD_COLORS) +
  ggplot2::facet_wrap(~ method, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title    = "Median daily prednisone-equivalent dose by method",
    subtitle = "One point per patient-drug episode; x-axis log10",
    x = "Median daily dose (mg pred-equiv)", y = "Density"
  ) +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")
print(p_dist)

# ---------------------------------------------------------------------------
# 6. Episode-level comparison vs gold standard
# ---------------------------------------------------------------------------
message("\n=== Episode-level comparisons vs gold standard ===")

ev_baseline <- .run_comparison(baseline_episodes, "Baseline")
ev_nlp      <- .run_comparison(nlp_episodes,      "NLP")
ev_llm      <- .run_comparison(llm_episodes,       "LLM")

# ---------------------------------------------------------------------------
# 7. NLP error decomposition by parse category
# ---------------------------------------------------------------------------
message("\n=== NLP error decomposition by parse category ===")

if (!is.null(ev_nlp) && nrow(ev_nlp$stratified$by_sig_status) > 0L) {
  cat("\nNLP: error metrics by parsed_status:\n")
  print(as.data.frame(
    ev_nlp$stratified$by_sig_status |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  ), row.names = FALSE)
}

if (!is.null(ev_nlp) &&
    "parsed_status" %in% names(nlp_episodes) &&
    "computed_episode_start" %in% names(ev_nlp$comparison)) {
  .comp_with_parse <- ev_nlp$comparison |>
    dplyr::left_join(
      nlp_episodes |>
        dplyr::select("person_id", "episode_start", "parsed_status") |>
        dplyr::rename(computed_episode_start = "episode_start"),
      by = c("person_id", "computed_episode_start")
    )
  cat("\nNLP: full error decomposition (incl. unmatched):\n")
  print(as.data.frame(
    stratify_errors_by_parse(.comp_with_parse, "parsed_status")
  ), row.names = FALSE)
}

# ---------------------------------------------------------------------------
# 8. Binary detection evaluation (Baseline, NLP, LLM)
# ---------------------------------------------------------------------------
GOLD_NEG_ID_COL <- "person_id"

if (!is.null(GOLD_NEG_PATH) && file.exists(GOLD_NEG_PATH)) {
  message("\n=== Binary detection evaluation ===")
  gold_neg <- readr::read_csv(GOLD_NEG_PATH, show_col_types = FALSE)
  cat(sprintf("Gold negative: %d confirmed non-users\n", nrow(gold_neg)))

  .run_detection <- function(episodes_df, label) {
    if (is.null(episodes_df) || nrow(episodes_df) == 0L) return(NULL)
    det <- evaluate_detection(
      computed_df         = episodes_df,
      gold_positive_df    = gold_std_ok,
      gold_negative_df    = gold_neg,
      detection_threshold = 0,
      obs_window_source   = "computed",
      gold_neg_id_col     = GOLD_NEG_ID_COL
    )
    m <- det$metrics
    cat(sprintf(
      "\n%s:\n  TP=%d  FN=%d  FP=%d  TN=%d\n  Sens=%.3f  Spec=%.3f  PPV=%.3f  NPV=%.3f  F1=%.3f  Kappa=%.3f\n",
      label, m$TP, m$FN, m$FP, m$TN,
      m$sensitivity, m$specificity, m$PPV, m$NPV, m$F1, m$kappa
    ))
    det
  }

  det_baseline <- .run_detection(baseline_episodes, "Baseline")
  det_nlp      <- .run_detection(nlp_episodes,      "NLP")
  det_llm      <- .run_detection(llm_episodes,       "LLM")
} else {
  message("\nSkipping binary detection — GOLD_NEG_PATH not set or file not found.")
  det_baseline <- det_nlp <- det_llm <- NULL
}

# ---------------------------------------------------------------------------
# 9. Sensitivity analyses (NLP as reference — most complete record-level data)
# ---------------------------------------------------------------------------
message("\n=== Sensitivity analyses ===")

if (exists("nlp_df") && nrow(nlp_df) > 0L) {
  gap_sens_result <- gap_sensitivity(
    nlp_df, gap_grid = c(0L, 7L, 14L, 30L, 60L, 90L),
    end_col = "drug_exposure_end_date", dose_col = "daily_dose_mg"
  )
  cat("\nGap window sensitivity (NLP method):\n")
  print(as.data.frame(
    gap_sens_result |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 1)))
  ), row.names = FALSE)

  equiv_sens_result <- dplyr::bind_rows(
    {
      c1 <- convert_pred_equiv(nlp_df, dose_col = "daily_dose_mg",
                               drug_col = "drug_name_std",
                               equiv_table = pred_equiv_table_conservative)
      e1 <- build_episodes(c1, end_col = "drug_exposure_end_date",
                           dose_col = "pred_equiv_mg", gap_days = GAP_DAYS)
      tibble::tibble(equiv_table = "Conservative",
                     n_episodes  = nrow(e1),
                     median_mg   = round(stats::median(e1$mean_daily_dose, na.rm = TRUE), 1),
                     mean_mg     = round(mean(e1$mean_daily_dose, na.rm = TRUE), 1))
    },
    {
      c2 <- convert_pred_equiv(nlp_df, dose_col = "daily_dose_mg",
                               drug_col = "drug_name_std")
      e2 <- build_episodes(c2, end_col = "drug_exposure_end_date",
                           dose_col = "pred_equiv_mg", gap_days = GAP_DAYS)
      tibble::tibble(equiv_table = "Default",
                     n_episodes  = nrow(e2),
                     median_mg   = round(stats::median(e2$mean_daily_dose, na.rm = TRUE), 1),
                     mean_mg     = round(mean(e2$mean_daily_dose, na.rm = TRUE), 1))
    },
    {
      c3 <- convert_pred_equiv(nlp_df, dose_col = "daily_dose_mg",
                               drug_col = "drug_name_std",
                               equiv_table = pred_equiv_table_aggressive)
      e3 <- build_episodes(c3, end_col = "drug_exposure_end_date",
                           dose_col = "pred_equiv_mg", gap_days = GAP_DAYS)
      tibble::tibble(equiv_table = "Aggressive",
                     n_episodes  = nrow(e3),
                     median_mg   = round(stats::median(e3$mean_daily_dose, na.rm = TRUE), 1),
                     mean_mg     = round(mean(e3$mean_daily_dose, na.rm = TRUE), 1))
    }
  )
  cat("\nPreднisone-equivalence table sensitivity (NLP):\n")
  print(as.data.frame(equiv_sens_result), row.names = FALSE)
} else {
  message("nlp_df not available — skipping sensitivity analyses.")
  gap_sens_result <- equiv_sens_result <- NULL
}

# ---------------------------------------------------------------------------
# 10. Scatter plots (method vs gold)
# ---------------------------------------------------------------------------
message("\n=== Scatter plots ===")

make_scatter_df <- function(ev, label) {
  if (is.null(ev)) return(NULL)
  ev$comparison |>
    dplyr::filter(!is.na(computed_dose)) |>
    dplyr::transmute(person_id, gold_dose, method_dose = computed_dose, method = label)
}

scatter_df <- dplyr::bind_rows(
  make_scatter_df(ev_baseline, "Baseline"),
  make_scatter_df(ev_nlp,      "NLP"),
  make_scatter_df(ev_llm,      "LLM")
) |> dplyr::mutate(method = factor(method, levels = c("Baseline", "NLP", "LLM")))

p_scatter <- ggplot2::ggplot(scatter_df,
                             ggplot2::aes(x = gold_dose, y = method_dose)) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(ggplot2::aes(colour = method), alpha = 0.5, size = 1.8) +
  ggplot2::geom_smooth(method = "lm", se = TRUE,
                       colour = "#d6604d", linewidth = 0.8) +
  ggplot2::scale_colour_manual(values = METHOD_COLORS) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Method dose vs gold standard (overlapping time window)",
    subtitle = "Dashed line = perfect agreement",
    x = "Gold standard median daily dose (mg pred-equiv)",
    y = "Method median daily dose (mg pred-equiv)"
  ) +
  ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")
print(p_scatter)

# ---------------------------------------------------------------------------
# 11. Bland-Altman plots
# ---------------------------------------------------------------------------
message("\n=== Bland-Altman plots ===")

ba_df <- scatter_df |>
  dplyr::filter(!is.na(gold_dose), !is.na(method_dose)) |>
  dplyr::mutate(mean_dose = (method_dose + gold_dose) / 2,
                diff      = method_dose - gold_dose)

ba_limits <- ba_df |>
  dplyr::group_by(method) |>
  dplyr::summarise(
    n = dplyr::n(), bias = mean(diff), sd_diff = stats::sd(diff),
    loa_lo = bias - 1.96 * sd_diff, loa_hi = bias + 1.96 * sd_diff,
    .groups = "drop"
  )
cat("\nBland-Altman limits of agreement:\n")
print(as.data.frame(ba_limits |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))))

p_ba <- ggplot2::ggplot(ba_df, ggplot2::aes(x = mean_dose, y = diff)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "solid",
                      colour = "grey70", linewidth = 0.5) +
  ggplot2::geom_hline(data = ba_limits, ggplot2::aes(yintercept = bias),
                      linetype = "dashed", colour = "#d6604d", linewidth = 0.8) +
  ggplot2::geom_hline(data = ba_limits, ggplot2::aes(yintercept = loa_lo),
                      linetype = "dotted", colour = "#4393c3", linewidth = 0.7) +
  ggplot2::geom_hline(data = ba_limits, ggplot2::aes(yintercept = loa_hi),
                      linetype = "dotted", colour = "#4393c3", linewidth = 0.7) +
  ggplot2::geom_point(ggplot2::aes(colour = method), alpha = 0.45, size = 1.8) +
  ggplot2::scale_colour_manual(values = METHOD_COLORS) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::labs(
    title    = "Bland-Altman: method dose minus gold standard",
    subtitle = "Red dashed = mean bias; blue dotted = ±1.96 SD limits of agreement",
    x = "Mean of method and gold (mg pred-equiv)",
    y = "Method − Gold (mg pred-equiv)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                 legend.position = "none")
print(p_ba)

# ---------------------------------------------------------------------------
# 12. Report
# ---------------------------------------------------------------------------
message("\n\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS REPORT — SteroidDoseR Three-Method Comparison\n")
cat(strrep("=", 70), "\n\n")

ev_list   <- list(Baseline = ev_baseline, NLP = ev_nlp, LLM = ev_llm)
ev_valid  <- Filter(Negate(is.null), ev_list)

episode_counts <- tibble::tibble(
  Source    = c("Baseline", "NLP", "LLM", "Gold Standard"),
  Patients  = c(dplyr::n_distinct(baseline_episodes$person_id),
                dplyr::n_distinct(nlp_episodes$person_id),
                if (!is.null(llm_episodes)) dplyr::n_distinct(llm_episodes$person_id) else NA_integer_,
                dplyr::n_distinct(gold_std$person_id)),
  Episodes  = c(nrow(baseline_episodes), nrow(nlp_episodes),
                if (!is.null(llm_episodes)) nrow(llm_episodes) else NA_integer_,
                nrow(gold_std)),
  Matched   = c(ev_baseline$summary$n_matched_periods,
                ev_nlp$summary$n_matched_periods,
                if (!is.null(ev_llm)) ev_llm$summary$n_matched_periods else NA_integer_,
                NA_integer_),
  Coverage  = c(round(ev_baseline$summary$coverage_pct, 1),
                round(ev_nlp$summary$coverage_pct,      1),
                if (!is.null(ev_llm)) round(ev_llm$summary$coverage_pct, 1) else NA_real_,
                NA_real_),
  Median_mg = c(stats::median(baseline_episodes$median_daily_dose, na.rm = TRUE),
                stats::median(nlp_episodes$median_daily_dose,       na.rm = TRUE),
                if (!is.null(llm_episodes))
                  stats::median(llm_episodes$median_daily_dose, na.rm = TRUE) else NA_real_,
                stats::median(gold_std$dose_daily_mg_equiv,     na.rm = TRUE))
)
cat("EPISODE COUNTS\n"); cat(strrep("-", 40), "\n")
print(as.data.frame(episode_counts), row.names = FALSE)

metrics_tbl <- tibble::tibble(
  Method       = names(ev_valid),
  Coverage_pct = round(vapply(ev_valid, \(e) e$summary$coverage_pct,   numeric(1)), 1),
  MAE_mg       = round(vapply(ev_valid, \(e) e$summary$MAE,            numeric(1)), 2),
  MBE_mg       = round(vapply(ev_valid, \(e) e$summary$MBE,            numeric(1)), 2),
  RMSE_mg      = round(vapply(ev_valid, \(e) e$summary$RMSE,           numeric(1)), 2),
  MAPE_pct     = round(vapply(ev_valid, \(e) e$summary$MAPE,           numeric(1)), 1),
  Pearson_r    = round(vapply(ev_valid, \(e) e$summary$pearson_corr,   numeric(1)), 3),
  Spearman_rho = round(vapply(ev_valid, \(e) e$summary$spearman_corr,  numeric(1)), 3)
)
cat("\nGOLD STANDARD COMPARISON\n"); cat(strrep("-", 40), "\n")
print(as.data.frame(metrics_tbl), row.names = FALSE)

best_cov <- metrics_tbl$Method[which.max(metrics_tbl$Coverage_pct)]
best_mae <- metrics_tbl$Method[which.min(metrics_tbl$MAE_mg)]
cat(sprintf(
  "\nHighest coverage: %s (%.1f%%)   |   Lowest MAE: %s (%.2f mg)\n",
  best_cov, max(metrics_tbl$Coverage_pct),
  best_mae, min(metrics_tbl$MAE_mg)
))
cat(strrep("=", 70), "\n")

# ---------------------------------------------------------------------------
# 13. Save
# ---------------------------------------------------------------------------
message("\n=== Saving comparison results ===")

readr::write_csv(episode_counts,  file.path(RUN_DIR, "episode_counts.csv"))
readr::write_csv(metrics_tbl,     file.path(RUN_DIR, "metrics_table.csv"))
readr::write_csv(dist_df_all,     file.path(RUN_DIR, "plot_data_dose_distribution.csv"))
readr::write_csv(scatter_df,      file.path(RUN_DIR, "plot_data_scatter.csv"))
readr::write_csv(ba_df,           file.path(RUN_DIR, "plot_data_bland_altman.csv"))
readr::write_csv(ba_limits,       file.path(RUN_DIR, "bland_altman_limits.csv"))
if (!is.null(ev_baseline))
  readr::write_csv(ev_baseline$comparison, file.path(RUN_DIR, "comparison_baseline.csv"))
if (!is.null(ev_nlp))
  readr::write_csv(ev_nlp$comparison,      file.path(RUN_DIR, "comparison_nlp.csv"))
if (!is.null(ev_llm))
  readr::write_csv(ev_llm$comparison,      file.path(RUN_DIR, "comparison_llm.csv"))
if (!is.null(gap_sens_result))
  readr::write_csv(gap_sens_result,         file.path(RUN_DIR, "sensitivity_gap.csv"))
if (!is.null(equiv_sens_result))
  readr::write_csv(equiv_sens_result,       file.path(RUN_DIR, "sensitivity_equiv.csv"))

ggplot2::ggsave(file.path(RUN_DIR, "plot_dose_distribution.png"), p_dist,
                width = 8,  height = 10, dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_scatter.png"),           p_scatter,
                width = 12, height = 5,  dpi = 150)
ggplot2::ggsave(file.path(RUN_DIR, "plot_bland_altman.png"),      p_ba,
                width = 12, height = 5,  dpi = 150)

message(sprintf("Comparison results saved → %s", RUN_DIR))

# ---------------------------------------------------------------------------
# 14. Interactive dashboard (all three methods)
# ---------------------------------------------------------------------------
message("\n=== Launching interactive dose review dashboard ===")
message("(Close the browser tab or press Escape in R to stop.)")

episode_list <- list("Baseline" = baseline_episodes, "NLP" = nlp_episodes)
raw_list     <- list("Baseline" = baseline_df,        "NLP" = nlp_df)
if (!is.null(llm_episodes) && nrow(llm_episodes) > 0L) {
  episode_list[["LLM"]] <- llm_episodes
  if (exists("llm_df") && nrow(llm_df) > 0L) raw_list[["LLM"]] <- llm_df
}

launch_dose_dashboard(
  episode_list = episode_list,
  raw_list     = raw_list,
  gold_std     = gold_std
)

message("=== CompareToRun.R complete ===")
