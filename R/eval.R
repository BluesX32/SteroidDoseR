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
#'   the columns named by `computed_id_col`, `computed_start_col`,
#'   `computed_end_col`, and `computed_dose_col`.
#'   Typically the output of [build_episodes()] after [convert_pred_equiv()].
#' @param gold_df A data frame of gold-standard episodes. Must contain the
#'   columns named by `gold_id_col`, `gold_start_col`, `gold_end_col`, and
#'   `gold_dose_col`.
#' @param min_overlap_days `integer(1)`. Minimum calendar-day overlap required
#'   to consider a computed episode as matching a gold episode. Default: `1L`.
#' @param computed_id_col `character(1)`. Patient ID column in `computed_df`.
#'   Default: `"person_id"`.
#' @param computed_start_col `character(1)`. Episode start column in
#'   `computed_df`. Default: `"episode_start"`.
#' @param computed_end_col `character(1)`. Episode end column in `computed_df`.
#'   Default: `"episode_end"`.
#' @param computed_dose_col `character(1)`. Dose column in `computed_df`.
#'   Default: `"median_daily_dose"`.
#' @param gold_id_col `character(1)`. Patient ID column in `gold_df`.
#'   Default: `"patient_id"`.
#' @param gold_start_col `character(1)`. Episode start column in `gold_df`.
#'   Default: `"episode_start"`.
#' @param gold_end_col `character(1)`. Episode end column in `gold_df`.
#'   Default: `"episode_end"`.
#' @param gold_dose_col `character(1)`. Dose column in `gold_df`.
#'   Default: `"median_daily_dose"`.
#' @param dose_breaks Numeric vector of cut-point boundaries for dose-range
#'   stratification in `$stratified$by_dose_range`. Default:
#'   `c(0, 10, 20, 40, Inf)` (tuned for typical myositis maintenance doses).
#'   Must have exactly one more element than `dose_labels`.
#' @param dose_labels Character vector of labels for the dose ranges defined
#'   by `dose_breaks`. Default:
#'   `c("Low (<=10mg)", "Medium (10-20mg)", "High (20-40mg)", "Very High (>40mg)")`.
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
                                  min_overlap_days  = 1L,
                                  computed_id_col   = "person_id",
                                  computed_start_col = "episode_start",
                                  computed_end_col  = "episode_end",
                                  computed_dose_col = "median_daily_dose",
                                  gold_id_col       = "patient_id",
                                  gold_start_col    = "episode_start",
                                  gold_end_col      = "episode_end",
                                  gold_dose_col     = "median_daily_dose",
                                  dose_breaks       = c(0, 10, 20, 40, Inf),
                                  dose_labels       = c("Low (<=10mg)",
                                                        "Medium (10-20mg)",
                                                        "High (20-40mg)",
                                                        "Very High (>40mg)")) {

  assert_required_cols(computed_df,
    c(computed_id_col, computed_start_col, computed_end_col, computed_dose_col),
    "computed_df")
  assert_required_cols(gold_df,
    c(gold_id_col, gold_start_col, gold_end_col, gold_dose_col),
    "gold_df")

  if (length(dose_breaks) != length(dose_labels) + 1L) {
    rlang::abort(paste0(
      "dose_breaks must have exactly one more element than dose_labels. ",
      "Got ", length(dose_breaks), " breaks and ", length(dose_labels), " labels."
    ))
  }

  # --- rename user columns to internal names ---------------------------------
  computed_df <- computed_df |>
    dplyr::rename(
      person_id         = dplyr::all_of(computed_id_col),
      episode_start     = dplyr::all_of(computed_start_col),
      episode_end       = dplyr::all_of(computed_end_col),
      median_daily_dose = dplyr::all_of(computed_dose_col)
    )
  gold_df <- gold_df |>
    dplyr::rename(
      patient_id        = dplyr::all_of(gold_id_col),
      episode_start     = dplyr::all_of(gold_start_col),
      episode_end       = dplyr::all_of(gold_end_col),
      median_daily_dose = dplyr::all_of(gold_dose_col)
    )

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
        breaks = dose_breaks,
        labels = dose_labels,
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
