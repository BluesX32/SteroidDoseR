# eval.R
# Gold-standard comparison — ported from
# DosageCalculation/Comparisons/Comparisons_Sep28.qmd (lines 52-178).

#' Compare computed dose episodes against a gold standard
#'
#' Matches computed episodes to gold-standard episodes using a date-overlap
#' join (any overlap of at least `min_overlap_days`), then computes
#' accuracy metrics that align with the OHDSI 2025 poster framing:
#' coverage (# usable / computable episodes), MAE, and MBE.
#'
#' @param computed_df A data frame of computed episode summaries. Must contain
#'   `person_id`, `episode_start`, `episode_end`, and `median_daily_dose`.
#'   Typically the output of [build_episodes()] after [convert_pred_equiv()].
#' @param gold_df A data frame of gold-standard episodes. Must contain
#'   `patient_id`, `episode_start`, `episode_end`, and `median_daily_dose`.
#' @param min_overlap_days `integer(1)`. Minimum calendar-day overlap required
#'   to consider a computed episode as matching a gold episode. Default: `1L`.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`$comparison`}{One row per gold episode, with columns:
#'     `patient_id`, `episode_start`, `episode_end`,
#'     `gold_dose`, `computed_dose`,
#'     `overlap_days`, `gold_duration`, `overlap_pct`,
#'     `absolute_error`, `bias_error`, `relative_error_pct`,
#'     `absolute_relative_error_pct`, `agreement_category`, `error_direction`.}
#'   \item{`$summary`}{One-row summary tibble:
#'     `n_gold_periods`, `n_matched_periods`, `coverage_pct`,
#'     `MAE`, `MBE`, `RMSE`, `median_AE`, `MAPE`, `mean_relative_bias_pct`,
#'     `pearson_corr`, `spearman_corr`.}
#'   \item{`$stratified`}{Stratified metrics by `dose_range` and
#'     `sig_category` (if `parsed_status` is present in `computed_df`),
#'     plus by taper status.}
#' }
#'
#' @export
#'
#' @examples
#' computed <- tibble::tibble(
#'   person_id         = 1L,
#'   episode_start     = as.Date("2023-01-01"),
#'   episode_end       = as.Date("2023-06-30"),
#'   median_daily_dose = 10
#' )
#' gold <- tibble::tibble(
#'   patient_id        = 1L,
#'   episode_start     = as.Date("2023-01-01"),
#'   episode_end       = as.Date("2023-06-30"),
#'   median_daily_dose = 10
#' )
#' evaluate_against_gold(computed, gold)
evaluate_against_gold <- function(computed_df,
                                  gold_df,
                                  min_overlap_days = 1L) {

  assert_required_cols(computed_df,
    c("person_id", "episode_start", "episode_end", "median_daily_dose"),
    "computed_df")
  assert_required_cols(gold_df,
    c("patient_id", "episode_start", "episode_end", "median_daily_dose"),
    "gold_df")

  # --- normalise dates -------------------------------------------------------
  comp <- computed_df |>
    dplyr::mutate(
      episode_start = safe_as_date(.data$episode_start),
      episode_end   = safe_as_date(.data$episode_end)
    )

  gold <- gold_df |>
    dplyr::mutate(
      episode_start = safe_as_date(.data$episode_start),
      episode_end   = safe_as_date(.data$episode_end),
      gold_dose     = safe_as_numeric(.data$median_daily_dose)
    ) |>
    dplyr::select("patient_id", "episode_start", "episode_end", "gold_dose")

  # --- overlap join (no external fuzzyjoin dependency) ----------------------
  # For each gold episode, find all computed episodes for the same patient
  # that overlap by at least min_overlap_days.
  merged <- gold |>
    dplyr::rename(g_start = "episode_start", g_end = "episode_end") |>
    dplyr::left_join(
      comp |> dplyr::rename(c_start = "episode_start", c_end = "episode_end"),
      by   = c("patient_id" = "person_id"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      overlap_start = pmax(.data$g_start, .data$c_start, na.rm = FALSE),
      overlap_end   = pmin(.data$g_end,   .data$c_end,   na.rm = FALSE),
      overlap_days  = as.integer(.data$overlap_end - .data$overlap_start) + 1L,
      overlap_days  = dplyr::if_else(.data$overlap_days < 1L, 0L, .data$overlap_days)
    ) |>
    dplyr::filter(.data$overlap_days >= min_overlap_days) |>
    # For each gold episode: keep the computed window with the most overlap
    dplyr::group_by(.data$patient_id, .data$g_start, .data$g_end) |>
    dplyr::arrange(dplyr::desc(.data$overlap_days)) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  # Build comparison table (one row per gold episode, even if unmatched)
  comparison <- gold |>
    dplyr::left_join(
      merged |>
        dplyr::transmute(
          patient_id    = .data$patient_id,
          episode_start = .data$g_start,
          episode_end   = .data$g_end,
          computed_dose = safe_as_numeric(.data$median_daily_dose),
          overlap_days  = .data$overlap_days
        ),
      by = c("patient_id", "episode_start" = "episode_start",
             "episode_end"  = "episode_end")
    ) |>
    dplyr::mutate(
      gold_duration = as.integer(.data$episode_end - .data$episode_start) + 1L,
      overlap_pct   = 100 * .data$overlap_days / .data$gold_duration,

      absolute_error             = abs(.data$computed_dose - .data$gold_dose),
      bias_error                 = .data$computed_dose - .data$gold_dose,
      relative_error_pct         = (.data$computed_dose - .data$gold_dose) /
                                     .data$gold_dose * 100,
      absolute_relative_error_pct = abs(.data$relative_error_pct),

      agreement_category = dplyr::case_when(
        .data$absolute_relative_error_pct <= 5  ~ "Exact (<=5%)",
        .data$absolute_relative_error_pct <= 20 ~ "Good (<=20%)",
        .data$absolute_relative_error_pct <= 50 ~ "Moderate (<=50%)",
        !is.na(.data$absolute_relative_error_pct) ~ "Poor (>50%)",
        TRUE ~ NA_character_
      ),
      error_direction = dplyr::case_when(
        .data$bias_error > 0  ~ "Over-estimation",
        .data$bias_error < 0  ~ "Under-estimation",
        .data$bias_error == 0 ~ "Exact match",
        TRUE ~ NA_character_
      )
    )

  n_gold     <- nrow(gold)
  n_matched  <- sum(!is.na(comparison$computed_dose))

  # --- overall summary -------------------------------------------------------
  matched <- comparison |> dplyr::filter(!is.na(.data$computed_dose))

  pcor <- if (nrow(matched) >= 3L)
    stats::cor(matched$computed_dose, matched$gold_dose,
               use = "complete.obs", method = "pearson")
  else NA_real_

  scor <- if (nrow(matched) >= 3L)
    stats::cor(matched$computed_dose, matched$gold_dose,
               use = "complete.obs", method = "spearman")
  else NA_real_

  summary_tbl <- tibble::tibble(
    n_gold_periods        = n_gold,
    n_matched_periods     = n_matched,
    coverage_pct          = 100 * n_matched / n_gold,
    MAE                   = mean(matched$absolute_error,              na.rm = TRUE),
    MBE                   = mean(matched$bias_error,                  na.rm = TRUE),
    RMSE                  = sqrt(mean(matched$bias_error^2,           na.rm = TRUE)),
    median_AE             = stats::median(matched$absolute_error,     na.rm = TRUE),
    MAPE                  = mean(matched$absolute_relative_error_pct, na.rm = TRUE),
    mean_relative_bias_pct = mean(matched$relative_error_pct,        na.rm = TRUE),
    pearson_corr          = pcor,
    spearman_corr         = scor
  )

  # --- stratified analysis ---------------------------------------------------
  strat_dose <- comparison |>
    dplyr::filter(!is.na(.data$computed_dose)) |>
    dplyr::mutate(
      dose_range = cut(
        .data$gold_dose,
        breaks = c(0, 10, 20, 40, Inf),
        labels = c("Low (<=10mg)", "Medium (10-20mg)", "High (20-40mg)", "Very High (>40mg)"),
        right  = TRUE
      )
    ) |>
    dplyr::group_by(.data$dose_range) |>
    dplyr::summarise(
      n    = dplyr::n(),
      MAE  = mean(.data$absolute_error,              na.rm = TRUE),
      MBE  = mean(.data$bias_error,                  na.rm = TRUE),
      MAPE = mean(.data$absolute_relative_error_pct, na.rm = TRUE),
      .groups = "drop"
    )

  strat_taper <- tibble::tibble(
    has_taper = logical(0), n = integer(0),
    MAE = numeric(0), MBE = numeric(0), MAPE = numeric(0)
  )
  if ("has_taper" %in% names(computed_df)) {
    strat_taper <- comparison |>
      dplyr::filter(!is.na(.data$computed_dose)) |>
      dplyr::left_join(
        computed_df |>
          dplyr::select("person_id", "episode_start", "has_taper") |>
          dplyr::rename(patient_id = "person_id"),
        by = c("patient_id", "episode_start")
      ) |>
      dplyr::group_by(.data$has_taper) |>
      dplyr::summarise(
        n    = dplyr::n(),
        MAE  = mean(.data$absolute_error,              na.rm = TRUE),
        MBE  = mean(.data$bias_error,                  na.rm = TRUE),
        MAPE = mean(.data$absolute_relative_error_pct, na.rm = TRUE),
        .groups = "drop"
      )
  }

  list(
    comparison = comparison,
    summary    = summary_tbl,
    stratified = list(
      by_dose_range   = strat_dose,
      by_taper_status = strat_taper
    )
  )
}
