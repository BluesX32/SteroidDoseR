# eval.R
# Gold-standard comparison -- ported from
# DosageCalculation/Comparisons/Comparisons_Sep28.qmd (lines 52-178).

# Suppress R CMD check NOTEs for bare column names used inside dplyr verbs
# inside compute_gold_anchored() (an @noRd internal).
utils::globalVariables(c(
  "person_id", "episode_start", "episode_end", "gold_duration_days",
  "median_daily_dose", "drug_exposure_start_date", "drug_exposure_end_date",
  "pred_equiv_mg", "pt_id", "rec_start", "rec_end", "g_start", "g_end",
  "gold_dur", "gold_dose", "dose", "clip_start", "clip_end", "day",
  "day_dose", "coverage_days", "coverage_pct", "cumulative_dose_mg",
  "avg_daily_dose_mg", "n_records"
))

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
#'   Default: `"mean_daily_dose"` (duration-weighted mean — more reliable than
#'   the unweighted median for tapered regimens). Pass `"median_daily_dose"` to
#'   reproduce legacy behaviour.
#' @param gold_id_col `character(1)`. Patient ID column in `gold_df`.
#'   Default: `"person_id"`.
#' @param gold_start_col `character(1)`. Episode start column in `gold_df`.
#'   Default: `"episode_start"`.
#' @param gold_end_col `character(1)`. Episode end column in `gold_df`.
#'   Default: `"episode_end"`.
#' @param gold_dose_col `character(1)`. Dose column in `gold_df`.
#'   Default: `"median_daily_dose"`. When using [parse_steroid_gold()] output,
#'   pass `gold_dose_col = "dose_daily_mg_equiv"`.
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
#'     `person_id`, `episode_start`, `episode_end`,
#'     `gold_dose`, `computed_dose`,
#'     `overlap_days`, `gold_duration`, `overlap_pct`,
#'     `absolute_error`, `bias_error`, `relative_error_pct`,
#'     `absolute_relative_error_pct`, `agreement_category`, `error_direction`.}
#'   \item{`$summary`}{One-row summary tibble:
#'     `n_gold_periods`, `n_matched_periods`, `coverage_pct`,
#'     `MAE`, `MBE`, `RMSE`, `median_AE`, `MAPE`, `mean_relative_bias_pct`,
#'     `pearson_corr`, `spearman_corr`.}
#'   \item{`$stratified`}{Named list of stratified metric tables:
#'     `by_dose_range` (always), `by_taper_status` (if `has_taper` column is
#'     present in `computed_df`), `by_sig_status` (if `sig_status` column is
#'     present in `computed_df` at episode level — populated when passing
#'     NLP/hierarchical output directly; each stratum shows MAE, MBE, MAPE).}
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
#'   person_id         = 1L,
#'   episode_start     = as.Date("2023-01-01"),
#'   episode_end       = as.Date("2023-06-30"),
#'   median_daily_dose = 10
#' )
#' evaluate_against_gold(computed, gold)
evaluate_against_gold <- function(computed_df,
                                  gold_df,
                                  min_overlap_days  = 1L,
                                  computed_id_col    = "person_id",
                                  computed_start_col = "episode_start",
                                  computed_end_col   = "episode_end",
                                  computed_dose_col  = "mean_daily_dose",
                                  gold_id_col        = "person_id",
                                  gold_start_col     = "episode_start",
                                  gold_end_col       = "episode_end",
                                  gold_dose_col      = "median_daily_dose",
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
  # Drop any column whose target internal name already exists under a *different*
  # source column name. Without this, renaming e.g. mean_daily_dose →
  # median_daily_dose would fail when median_daily_dose is already present.
  .drop_if_collision <- function(df, src, tgt) {
    if (src != tgt && tgt %in% names(df)) dplyr::select(df, -dplyr::all_of(tgt)) else df
  }
  computed_df <- computed_df |>
    .drop_if_collision(computed_id_col,    "person_id") |>
    .drop_if_collision(computed_start_col, "episode_start") |>
    .drop_if_collision(computed_end_col,   "episode_end") |>
    .drop_if_collision(computed_dose_col,  "median_daily_dose") |>
    dplyr::rename(
      person_id         = dplyr::all_of(computed_id_col),
      episode_start     = dplyr::all_of(computed_start_col),
      episode_end       = dplyr::all_of(computed_end_col),
      median_daily_dose = dplyr::all_of(computed_dose_col)
    )
  gold_df <- gold_df |>
    .drop_if_collision(gold_id_col,    "person_id") |>
    .drop_if_collision(gold_start_col, "episode_start") |>
    .drop_if_collision(gold_end_col,   "episode_end") |>
    .drop_if_collision(gold_dose_col,  "median_daily_dose") |>
    dplyr::rename(
      person_id         = dplyr::all_of(gold_id_col),
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
    dplyr::select("person_id", "episode_start", "episode_end", "gold_dose")

  # --- restrict to common patients (apple-to-apple dose accuracy) -----------
  # Patients absent from computed_df are detection failures, not dose errors.
  # Report them separately via detection_coverage_pct; exclude from MAE/MBE.
  n_gold_patients   <- dplyr::n_distinct(gold$person_id)
  common_ids        <- intersect(unique(gold$person_id), unique(comp$person_id))
  n_common_patients <- length(common_ids)
  gold_common       <- gold |> dplyr::filter(.data$person_id %in% common_ids)

  # --- overlap join (no external fuzzyjoin dependency) ----------------------
  # For each gold episode, find all computed episodes for the same patient
  # that overlap by at least min_overlap_days.
  merged <- gold_common |>
    dplyr::rename(g_start = "episode_start", g_end = "episode_end") |>
    dplyr::left_join(
      comp |> dplyr::rename(c_start = "episode_start", c_end = "episode_end"),
      by   = "person_id",
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
    dplyr::group_by(.data$person_id, .data$g_start, .data$g_end) |>
    dplyr::arrange(dplyr::desc(.data$overlap_days)) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()

  # Build comparison table (one row per common-patient gold episode)
  comparison <- gold_common |>
    dplyr::left_join(
      merged |>
        dplyr::transmute(
          person_id              = .data$person_id,
          episode_start          = .data$g_start,
          episode_end            = .data$g_end,
          computed_episode_start = .data$c_start,
          computed_dose          = safe_as_numeric(.data$median_daily_dose),
          overlap_days           = .data$overlap_days
        ),
      by = c("person_id", "episode_start", "episode_end")
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

  n_gold     <- nrow(gold_common)
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
    n_gold_patients        = n_gold_patients,
    n_common_patients      = n_common_patients,
    detection_coverage_pct = 100 * n_common_patients / n_gold_patients,
    n_gold_periods         = n_gold,
    n_matched_periods      = n_matched,
    coverage_pct           = 100 * n_matched / n_gold,
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
          dplyr::select("person_id", "episode_start", "has_taper"),
        by = c("person_id", "episode_start")
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

  # by_sig_status: stratify on parser status ("ok", "prn", "taper", "no_parse")
  # Accepts sig_status (hierarchical output) or parsed_status (NLP advanced
  # output) — whichever is present. Populated when computed_df has at least one
  # of these columns at episode level (set via build_episodes(extra_cols = ...)).
  strat_sig_status <- tibble::tibble(
    sig_status = character(0), n = integer(0),
    MAE = numeric(0), MBE = numeric(0), MAPE = numeric(0)
  )
  status_col <- intersect(c("sig_status", "parsed_status"), names(computed_df))[1L]
  if (!is.na(status_col) && "episode_start" %in% names(computed_df)) {
    strat_sig_status <- comparison |>
      dplyr::filter(!is.na(.data$computed_dose),
                    !is.na(.data$computed_episode_start)) |>
      dplyr::left_join(
        computed_df |>
          dplyr::select("person_id", "episode_start",
                        dplyr::all_of(status_col)) |>
          dplyr::rename(computed_episode_start = "episode_start"),
        by = c("person_id", "computed_episode_start")
      ) |>
      dplyr::rename(sig_status = dplyr::all_of(status_col)) |>
      dplyr::group_by(.data$sig_status) |>
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
      by_taper_status = strat_taper,
      by_sig_status   = strat_sig_status
    )
  )
}


#' Evaluate binary steroid detection against two gold standards
#'
#' Computes TP / FP / TN / FN and Cohen's kappa from two gold-standard
#' cohorts: patients confirmed to be on steroids (`gold_positive_df`) and
#' patients confirmed to have never used steroids (`gold_negative_df`).
#'
#' **Gold positives — episode-level (TP / FN):**
#' A gold-positive episode is **True Positive** when a computed episode for
#' the same patient overlaps it and has `computed_dose >= detection_threshold`.
#' Otherwise it is **False Negative**.
#'
#' **Gold negatives — patient-level (FP / TN):**
#' A gold-negative patient is **False Positive** when the algorithm assigns
#' them a dose `>= detection_threshold` within their observation window.
#' Otherwise it is **True Negative**. The window is set by
#' `obs_window_source`:
#' \describe{
#'   \item{`"computed"` (default)}{Window = all computed episodes for that
#'     patient. Patients absent from `computed_df` are automatically TN.
#'     Preferred when no explicit observation window is available.}
#'   \item{`"explicit"`}{Each row of `gold_negative_df` carries its own
#'     window via `gold_neg_start_col` / `gold_neg_end_col` (e.g. the
#'     patient's enrolment period).}
#'   \item{`"study"`}{A single fixed window (`study_start` to `study_end`)
#'     applies to every gold-negative patient.}
#' }
#'
#' @param computed_df Data frame of computed episode summaries (output of
#'   [build_episodes()] after [convert_pred_equiv()]).
#' @param gold_positive_df Data frame of confirmed steroid-user episodes.
#'   Must contain `gold_pos_id_col`, `gold_pos_start_col`, `gold_pos_end_col`.
#' @param gold_negative_df Data frame of confirmed steroid-naive patients
#'   (one row per patient). Must contain `gold_neg_id_col`. When
#'   `obs_window_source = "explicit"` also requires `gold_neg_start_col` and
#'   `gold_neg_end_col`.
#' @param detection_threshold `numeric(1)`. Minimum computed dose (mg
#'   pred-equiv/day) to count as detected. Default `0` (any positive dose).
#'   Use `5` or `10` to exclude trace / data-artifact doses.
#' @param obs_window_source `character(1)`. How to define the observation
#'   window for gold-negative patients. One of `"computed"` (default),
#'   `"explicit"`, or `"study"`. See Details.
#' @param study_start Date or `character`. Study start date. Required when
#'   `obs_window_source = "study"`.
#' @param study_end Date or `character`. Study end date. Required when
#'   `obs_window_source = "study"`.
#' @param computed_id_col `character(1)`. Patient ID column in `computed_df`.
#'   Default `"person_id"`.
#' @param computed_start_col `character(1)`. Episode start column in
#'   `computed_df`. Default `"episode_start"`.
#' @param computed_end_col `character(1)`. Episode end column in
#'   `computed_df`. Default `"episode_end"`.
#' @param computed_dose_col `character(1)`. Dose column in `computed_df`.
#'   Default `"median_daily_dose"`.
#' @param gold_pos_id_col `character(1)`. Patient ID column in
#'   `gold_positive_df`. Default `"person_id"`.
#' @param gold_pos_start_col `character(1)`. Episode start column in
#'   `gold_positive_df`. Default `"episode_start"`.
#' @param gold_pos_end_col `character(1)`. Episode end column in
#'   `gold_positive_df`. Default `"episode_end"`.
#' @param gold_neg_id_col `character(1)`. Patient ID column in
#'   `gold_negative_df`. Default `"person_id"`.
#' @param gold_neg_start_col `character(1)`. Observation start column in
#'   `gold_negative_df`. Used only when `obs_window_source = "explicit"`.
#'   Default `"obs_start"`.
#' @param gold_neg_end_col `character(1)`. Observation end column in
#'   `gold_negative_df`. Used only when `obs_window_source = "explicit"`.
#'   Default `"obs_end"`.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{`$confusion`}{2×2 matrix. Rows = Gold (Positive / Negative);
#'     columns = Computed (Detected / Not detected). Cells: TP, FN, FP, TN.}
#'   \item{`$metrics`}{One-row tibble: `n_gold_positive`, `n_gold_negative`,
#'     `TP`, `FN`, `FP`, `TN`, `sensitivity`, `specificity`, `PPV`, `NPV`,
#'     `accuracy`, `F1`, `kappa`.}
#'   \item{`$detail_positive`}{Gold-positive rows with added `detected`
#'     (logical) and `classification` (`"TP"` / `"FN"`).}
#'   \item{`$detail_negative`}{Gold-negative rows with added `detected`
#'     (logical) and `classification` (`"FP"` / `"TN"`).}
#' }
#'
#' @export
#'
#' @examples
#' computed <- tibble::tibble(
#'   person_id         = c(1L, 2L),
#'   episode_start     = as.Date(c("2023-01-01", "2023-03-01")),
#'   episode_end       = as.Date(c("2023-06-30", "2023-09-30")),
#'   median_daily_dose = c(20, 5)
#' )
#' gold_pos <- tibble::tibble(
#'   person_id     = 1L,
#'   episode_start = as.Date("2023-01-01"),
#'   episode_end   = as.Date("2023-06-30")
#' )
#' gold_neg <- tibble::tibble(person_id = c(3L, 4L))
#' evaluate_detection(computed, gold_pos, gold_neg)
evaluate_detection <- function(
  computed_df,
  gold_positive_df,
  gold_negative_df,
  detection_threshold = 0,
  obs_window_source   = c("computed", "explicit", "study"),
  study_start         = NULL,
  study_end           = NULL,
  computed_id_col     = "person_id",
  computed_start_col  = "episode_start",
  computed_end_col    = "episode_end",
  computed_dose_col   = "median_daily_dose",
  gold_pos_id_col     = "person_id",
  gold_pos_start_col  = "episode_start",
  gold_pos_end_col    = "episode_end",
  gold_neg_id_col     = "person_id",
  gold_neg_start_col  = "obs_start",
  gold_neg_end_col    = "obs_end"
) {
  obs_window_source <- match.arg(obs_window_source)

  assert_required_cols(computed_df,
    c(computed_id_col, computed_start_col, computed_end_col, computed_dose_col),
    "computed_df")
  assert_required_cols(gold_positive_df,
    c(gold_pos_id_col, gold_pos_start_col, gold_pos_end_col),
    "gold_positive_df")
  assert_required_cols(gold_negative_df, gold_neg_id_col, "gold_negative_df")

  if (obs_window_source == "explicit") {
    assert_required_cols(gold_negative_df,
      c(gold_neg_start_col, gold_neg_end_col),
      "gold_negative_df (obs_window_source = 'explicit')")
  }
  if (obs_window_source == "study") {
    if (is.null(study_start) || is.null(study_end))
      rlang::abort(
        "study_start and study_end must be provided when obs_window_source = 'study'."
      )
    study_start <- safe_as_date(study_start)
    study_end   <- safe_as_date(study_end)
  }

  # --- normalise computed -----------------------------------------------
  comp <- computed_df |>
    dplyr::rename(
      person_id     = dplyr::all_of(computed_id_col),
      episode_start = dplyr::all_of(computed_start_col),
      episode_end   = dplyr::all_of(computed_end_col),
      computed_dose = dplyr::all_of(computed_dose_col)
    ) |>
    dplyr::mutate(
      episode_start = safe_as_date(.data$episode_start),
      episode_end   = safe_as_date(.data$episode_end),
      computed_dose = safe_as_numeric(.data$computed_dose)
    )

  comp_above <- dplyr::filter(comp, .data$computed_dose >= detection_threshold)

  # --- GOLD POSITIVES: TP / FN ------------------------------------------
  gpos <- gold_positive_df |>
    dplyr::rename(
      person_id     = dplyr::all_of(gold_pos_id_col),
      episode_start = dplyr::all_of(gold_pos_start_col),
      episode_end   = dplyr::all_of(gold_pos_end_col)
    ) |>
    dplyr::mutate(
      episode_start = safe_as_date(.data$episode_start),
      episode_end   = safe_as_date(.data$episode_end)
    )

  pos_detected <- gpos |>
    dplyr::rename(g_start = "episode_start", g_end = "episode_end") |>
    dplyr::left_join(
      comp_above |>
        dplyr::rename(c_start = "episode_start", c_end = "episode_end"),
      by = "person_id",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      overlap = as.integer(
        pmin(.data$g_end, .data$c_end, na.rm = FALSE) -
          pmax(.data$g_start, .data$c_start, na.rm = FALSE)
      ) + 1L,
      overlap = dplyr::if_else(
        is.na(.data$overlap) | .data$overlap < 1L, 0L, .data$overlap
      )
    ) |>
    dplyr::group_by(.data$person_id, .data$g_start, .data$g_end) |>
    dplyr::summarise(
      detected = any(.data$overlap >= 1L, na.rm = TRUE),
      .groups  = "drop"
    )

  detail_positive <- gpos |>
    dplyr::left_join(
      pos_detected,
      by = c("person_id",
             "episode_start" = "g_start",
             "episode_end"   = "g_end")
    ) |>
    dplyr::mutate(
      detected       = dplyr::if_else(is.na(.data$detected), FALSE, .data$detected),
      classification = dplyr::if_else(.data$detected, "TP", "FN")
    )

  # --- GOLD NEGATIVES: FP / TN ------------------------------------------
  gneg <- gold_negative_df |>
    dplyr::rename(person_id = dplyr::all_of(gold_neg_id_col))

  if (obs_window_source == "computed") {
    detected_ids <- comp_above |>
      dplyr::distinct(.data$person_id) |>
      dplyr::mutate(detected = TRUE)

    detail_negative <- gneg |>
      dplyr::left_join(detected_ids, by = "person_id") |>
      dplyr::mutate(
        detected       = dplyr::if_else(is.na(.data$detected), FALSE, .data$detected),
        classification = dplyr::if_else(.data$detected, "FP", "TN")
      )

  } else if (obs_window_source == "explicit") {
    gneg <- gneg |>
      dplyr::rename(
        obs_start = dplyr::all_of(gold_neg_start_col),
        obs_end   = dplyr::all_of(gold_neg_end_col)
      ) |>
      dplyr::mutate(
        obs_start = safe_as_date(.data$obs_start),
        obs_end   = safe_as_date(.data$obs_end)
      )

    neg_detected <- gneg |>
      dplyr::left_join(
        comp_above |>
          dplyr::rename(c_start = "episode_start", c_end = "episode_end"),
        by = "person_id",
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        overlap = as.integer(
          pmin(.data$obs_end, .data$c_end, na.rm = FALSE) -
            pmax(.data$obs_start, .data$c_start, na.rm = FALSE)
        ) + 1L,
        overlap = dplyr::if_else(
          is.na(.data$overlap) | .data$overlap < 1L, 0L, .data$overlap
        )
      ) |>
      dplyr::group_by(.data$person_id) |>
      dplyr::summarise(
        detected = any(.data$overlap >= 1L, na.rm = TRUE),
        .groups  = "drop"
      )

    detail_negative <- gneg |>
      dplyr::left_join(neg_detected, by = "person_id") |>
      dplyr::mutate(
        detected       = dplyr::if_else(is.na(.data$detected), FALSE, .data$detected),
        classification = dplyr::if_else(.data$detected, "FP", "TN")
      )

  } else {
    # obs_window_source == "study"
    detected_ids <- comp_above |>
      dplyr::filter(
        .data$episode_start <= study_end,
        .data$episode_end   >= study_start
      ) |>
      dplyr::distinct(.data$person_id) |>
      dplyr::mutate(detected = TRUE)

    detail_negative <- gneg |>
      dplyr::left_join(detected_ids, by = "person_id") |>
      dplyr::mutate(
        detected       = dplyr::if_else(is.na(.data$detected), FALSE, .data$detected),
        classification = dplyr::if_else(.data$detected, "FP", "TN")
      )
  }

  # --- confusion matrix -------------------------------------------------
  n_tp <- sum(detail_positive$classification == "TP")
  n_fn <- sum(detail_positive$classification == "FN")
  n_fp <- sum(detail_negative$classification == "FP")
  n_tn <- sum(detail_negative$classification == "TN")
  n    <- n_tp + n_fn + n_fp + n_tn

  # Standard clinical layout: rows = gold, cols = computed (detected / not)
  confusion <- matrix(
    c(n_tp, n_fp, n_fn, n_tn),
    nrow     = 2L, ncol = 2L,
    dimnames = list(
      Gold     = c("Positive", "Negative"),
      Computed = c("Detected", "Not detected")
    )
  )

  # --- metrics ----------------------------------------------------------
  sensitivity <- if ((n_tp + n_fn) > 0L) n_tp / (n_tp + n_fn) else NA_real_
  specificity <- if ((n_tn + n_fp) > 0L) n_tn / (n_tn + n_fp) else NA_real_
  ppv         <- if ((n_tp + n_fp) > 0L) n_tp / (n_tp + n_fp) else NA_real_
  npv         <- if ((n_tn + n_fn) > 0L) n_tn / (n_tn + n_fn) else NA_real_
  accuracy    <- if (n > 0L) (n_tp + n_tn) / n else NA_real_
  f1          <- if (!is.na(ppv) && !is.na(sensitivity) &&
                     (ppv + sensitivity) > 0)
                   2 * ppv * sensitivity / (ppv + sensitivity) else NA_real_

  # Cohen's kappa = (observed agreement - chance agreement) / (1 - chance)
  p_e   <- ((n_tp + n_fp) / n) * ((n_tp + n_fn) / n) +
            ((n_fn + n_tn) / n) * ((n_fp + n_tn) / n)
  kappa <- if (!is.na(accuracy) && (1 - p_e) > 0)
             (accuracy - p_e) / (1 - p_e) else NA_real_

  metrics <- tibble::tibble(
    n_gold_positive = as.integer(n_tp + n_fn),
    n_gold_negative = as.integer(n_fp + n_tn),
    TP          = as.integer(n_tp),
    FN          = as.integer(n_fn),
    FP          = as.integer(n_fp),
    TN          = as.integer(n_tn),
    sensitivity = sensitivity,
    specificity = specificity,
    PPV         = ppv,
    NPV         = npv,
    accuracy    = accuracy,
    F1          = f1,
    kappa       = kappa
  )

  list(
    confusion       = confusion,
    metrics         = metrics,
    detail_positive = detail_positive,
    detail_negative = detail_negative
  )
}

# ---------------------------------------------------------------------------
# Internal: classify SIG text into interpretable categories
# ---------------------------------------------------------------------------

#' Classify SIG text into clinically meaningful categories
#'
#' Used by error-decomposition and manual-review extras scripts to label each
#' drug-exposure record's SIG string as one of: `"steady"`, `"taper"`,
#' `"escalation"`, `"prn"`, `"conflicting"`, `"ambiguous"`, `"no_sig"`.
#'
#' @param sig_text     Character vector of raw SIG strings.
#' @param sig_status   Character vector of parser status (`"ok"`, `"taper"`, etc.).
#' @param sig_taper_flag Logical vector; `TRUE` when taper language detected.
#' @param bl_dose      Numeric vector of baseline doses (mg/day).
#' @param sig_dose     Numeric vector of NLP doses (mg/day).
#' @param conflict_thr Numeric(1). `|bl_dose - sig_dose|` above this → `"conflicting"`.
#' @return Character vector of SIG type labels.
#' @noRd
classify_sig_type <- function(sig_text, sig_status, sig_taper_flag,
                               bl_dose, sig_dose, conflict_thr = 20) {
  taper_flag <- !is.na(sig_taper_flag) & sig_taper_flag
  both_avail <- !is.na(bl_dose) & !is.na(sig_dose)
  txt_lc     <- tolower(ifelse(is.na(sig_text), "", sig_text))

  dplyr::case_when(
    taper_flag | (!is.na(sig_status) & sig_status %in% c("taper", "taper_ok"))
      ~ "taper",
    both_avail & abs(bl_dose - sig_dose) > conflict_thr
      ~ "conflicting",
    (!is.na(sig_status) & sig_status == "prn") |
      grepl("\\bas needed\\b|\\bprn\\b", txt_lc)
      ~ "prn",
    grepl("\\bincrease|\\bescalate|\\btitrate|\\bump\\s+to|\\bump\\s+dose", txt_lc) &
      !grepl("\\btaper|\\bdecrease|\\breduce|\\bwean", txt_lc)
      ~ "escalation",
    is.na(sig_text) | sig_text == ""
      ~ "no_sig",
    !is.na(sig_status) & sig_status %in% c("no_parse", "free_text", "empty")
      ~ "ambiguous",
    !is.na(sig_status) & sig_status == "ok"
      ~ "steady",
    TRUE ~ "ambiguous"
  )
}

# ---------------------------------------------------------------------------
# Internal: gold-anchored dose comparison
# ---------------------------------------------------------------------------

#' Compare computed record-level doses against gold-standard episodes
#'
#' Alternative to [evaluate_against_gold()]: instead of building gap-bridged
#' episodes, this function clips every computed drug-exposure record to each
#' overlapping gold window, day-expands, sums doses per calendar day (handling
#' concurrent prescriptions correctly), and returns per-gold-episode averages.
#'
#' The denominator is always the full gold duration, so days with no matching
#' record contribute 0 dose — coverage gaps are visible in `avg_daily_dose_mg`
#' rather than being silently ignored.
#'
#' @param records_df Data frame with columns `person_id`,
#'   `drug_exposure_start_date`, `drug_exposure_end_date`, `pred_equiv_mg`.
#'   Typically the output of `convert_pred_equiv()` on record-level doses.
#' @param gold_df Data frame with columns `person_id`, `episode_start`,
#'   `episode_end`, `median_daily_dose`, `gold_duration_days`.
#' @return One row per gold episode with columns: `person_id`, `episode_start`,
#'   `episode_end`, `gold_duration_days`, `gold_dose`, `n_records`,
#'   `coverage_days`, `coverage_pct`, `cumulative_dose_mg`, `avg_daily_dose_mg`.
#' @noRd
compute_gold_anchored <- function(records_df, gold_df) {
  overlapping <- records_df |>
    dplyr::transmute(
      pt_id     = as.integer(person_id),
      rec_start = drug_exposure_start_date,
      rec_end   = drug_exposure_end_date,
      dose      = pred_equiv_mg
    ) |>
    dplyr::inner_join(
      gold_df |>
        dplyr::transmute(
          pt_id     = as.integer(person_id),
          g_start   = episode_start,
          g_end     = episode_end,
          gold_dur  = gold_duration_days,
          gold_dose = median_daily_dose
        ),
      by = "pt_id", relationship = "many-to-many"
    ) |>
    dplyr::filter(rec_start <= g_end, rec_end >= g_start)

  empty_result <- gold_df |>
    dplyr::transmute(
      person_id          = as.integer(person_id),
      episode_start, episode_end,
      gold_duration_days,
      gold_dose          = median_daily_dose,
      n_records          = 0L,
      coverage_days      = 0L,
      coverage_pct       = 0,
      cumulative_dose_mg = NA_real_,
      avg_daily_dose_mg  = NA_real_
    )

  if (nrow(overlapping) == 0L) {
    warning("No records overlap any gold episode.")
    return(empty_result)
  }

  overlapping <- overlapping |>
    dplyr::mutate(
      clip_start = pmax(rec_start, g_start),
      clip_end   = pmin(rec_end,   g_end)
    ) |>
    dplyr::filter(!is.na(clip_start), !is.na(clip_end), clip_start <= clip_end)

  if (nrow(overlapping) == 0L) {
    warning("No valid clipped records remain after date-bound check.")
    return(empty_result)
  }

  daily_totals <- overlapping |>
    dplyr::rowwise() |>
    dplyr::mutate(day = list(seq.Date(clip_start, clip_end, by = "day"))) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = day) |>
    dplyr::group_by(pt_id, g_start, g_end, gold_dur, gold_dose, day) |>
    dplyr::summarise(day_dose = sum(dose, na.rm = TRUE), .groups = "drop")

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

  n_records_per_ep <- overlapping |>
    dplyr::group_by(pt_id, g_start, g_end) |>
    dplyr::summarise(n_records = dplyr::n(), .groups = "drop")

  episode_summary <- episode_summary |>
    dplyr::left_join(n_records_per_ep, by = c("pt_id", "g_start", "g_end"))

  gold_df |>
    dplyr::transmute(
      person_id          = as.integer(person_id),
      episode_start, episode_end,
      gold_duration_days,
      gold_dose          = median_daily_dose
    ) |>
    dplyr::left_join(
      episode_summary |>
        dplyr::transmute(
          person_id          = pt_id,
          episode_start      = g_start,
          episode_end        = g_end,
          n_records, coverage_days, coverage_pct,
          cumulative_dose_mg, avg_daily_dose_mg
        ),
      by = c("person_id", "episode_start", "episode_end")
    ) |>
    dplyr::mutate(
      n_records     = dplyr::coalesce(n_records, 0L),
      coverage_days = dplyr::coalesce(coverage_days, 0L),
      coverage_pct  = dplyr::coalesce(coverage_pct, 0)
    )
}

# ---------------------------------------------------------------------------
# Exported: stratify_errors_by_parse
# ---------------------------------------------------------------------------

#' Decompose evaluation errors by parse or method category
#'
#' Groups a comparison data frame (from [evaluate_against_gold()]`$comparison`)
#' by a categorical column — typically `"parsed_status"`, `"sig_status"`, or
#' `"hierarchical_method"` — and returns per-category error metrics. This
#' reveals whether errors are concentrated in clinically interpretable
#' categories (e.g., taper, PRN, free-text), which is required for reviewers to
#' assess the paper's validity claims.
#'
#' @param comparison_df Data frame. Must contain the columns produced by
#'   [evaluate_against_gold()]`$comparison`: `computed_dose`, `gold_dose`,
#'   `absolute_error`, `bias_error`, `absolute_relative_error_pct`. Must also
#'   contain the column named by `parse_col`.
#' @param parse_col `character(1)`. Name of the categorical parse/method column
#'   to group by. Typical values: `"parsed_status"` (NLP advanced output),
#'   `"sig_status"` (hierarchical output), `"hierarchical_method"`.
#'
#' @return A tibble with one row per category, sorted by descending `n`:
#' \describe{
#'   \item{category}{The parse or method label.}
#'   \item{n}{Total episodes in this category (matched or not).}
#'   \item{n_matched}{Episodes with a non-`NA` computed dose.}
#'   \item{coverage_pct}{`100 * n_matched / n`.}
#'   \item{MAE}{Mean absolute error (mg prednisone-equivalent/day).}
#'   \item{MBE}{Mean bias error (positive = over-estimation).}
#'   \item{MAPE}{Mean absolute percentage error.}
#'   \item{median_AE}{Median absolute error.}
#' }
#'
#' @seealso [evaluate_against_gold()] — source of `comparison_df`.
#'   [build_episodes()] `extra_cols` argument — how to carry parse status into
#'   episode-level output so it can be joined here.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nlp_episodes <- build_episodes(
#'   nlp_df,
#'   end_col    = "drug_exposure_end_date",
#'   dose_col   = "daily_dose_mg",
#'   extra_cols = "parsed_status"
#' )
#' ev <- evaluate_against_gold(nlp_episodes, gold_df,
#'                              gold_dose_col = "dose_daily_mg_equiv")
#' comp_with_status <- ev$comparison |>
#'   dplyr::left_join(
#'     nlp_episodes |>
#'       dplyr::select(person_id, episode_start, parsed_status),
#'     by = c("person_id", "episode_start")
#'   )
#' stratify_errors_by_parse(comp_with_status, "parsed_status")
#' }
stratify_errors_by_parse <- function(comparison_df, parse_col) {
  if (!parse_col %in% names(comparison_df))
    rlang::abort(sprintf(
      "Column '%s' not found in comparison_df. Join parse status from the episode data frame first.",
      parse_col
    ))
  required <- c("computed_dose", "gold_dose",
                "absolute_error", "bias_error", "absolute_relative_error_pct")
  missing_r <- setdiff(required, names(comparison_df))
  if (length(missing_r) > 0L)
    rlang::abort(paste0(
      "comparison_df is missing required columns: ",
      paste(missing_r, collapse = ", "),
      ". Pass the `$comparison` element from evaluate_against_gold()."
    ))

  comparison_df |>
    dplyr::mutate(category = .data[[parse_col]]) |>
    dplyr::group_by(.data$category) |>
    dplyr::summarise(
      n            = dplyr::n(),
      n_matched    = sum(!is.na(.data$computed_dose)),
      coverage_pct = round(100 * mean(!is.na(.data$computed_dose)), 1),
      MAE          = round(mean(.data$absolute_error,              na.rm = TRUE), 2),
      MBE          = round(mean(.data$bias_error,                  na.rm = TRUE), 2),
      MAPE         = round(mean(.data$absolute_relative_error_pct, na.rm = TRUE), 1),
      median_AE    = round(stats::median(.data$absolute_error,     na.rm = TRUE), 2),
      .groups      = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

# ---------------------------------------------------------------------------
# Exported: make_validation_split
# ---------------------------------------------------------------------------

#' Split a gold-standard data frame into train and validate folds by patient
#'
#' Randomly assigns each unique patient to either the training set or the
#' validation set, returning two non-overlapping subsets of `gold_df`. Always
#' splits by patient (not by episode) so the same patient never appears in both
#' folds.
#'
#' **Why this matters for threshold tuning:** If you tune hierarchical
#' thresholds (`match_tol`, `diff_threshold`) using [tune_hierarchical_thresholds()]
#' and then evaluate those same thresholds on the *full* gold set, the
#' performance estimate is optimistic (gold-standard leakage). Use the `$train`
#' split for tuning and the `$validate` split for unbiased evaluation.
#'
#' @param gold_df Data frame of gold-standard episodes. Must contain `id_col`.
#' @param id_col `character(1)`. Patient ID column. Default: `"person_id"`.
#' @param train_frac `numeric(1)`. Fraction of patients to place in the
#'   training fold. Default: `0.7` (70 % train / 30 % validate).
#' @param seed `integer(1)`. Random seed for reproducibility. Default: `42L`.
#'
#' @return A named list:
#' \describe{
#'   \item{`$train`}{Gold episodes for training patients (use for tuning).}
#'   \item{`$validate`}{Gold episodes for validation patients (use for evaluation).}
#'   \item{`$train_ids`}{Integer vector of patient IDs in the training fold.}
#'   \item{`$validate_ids`}{Integer vector of patient IDs in the validation fold.}
#' }
#'
#' @seealso [tune_hierarchical_thresholds()] — pass `gold_df = split$train`.
#'
#' @export
#'
#' @examples
#' gold <- tibble::tibble(
#'   person_id         = c(1L, 1L, 2L, 3L, 4L),
#'   episode_start     = as.Date(c("2023-01-01","2023-06-01",
#'                                 "2023-01-01","2023-03-01","2023-02-01")),
#'   episode_end       = as.Date(c("2023-05-31","2023-12-31",
#'                                 "2023-06-30","2023-09-30","2023-08-31")),
#'   median_daily_dose = c(20, 10, 40, 5, 20)
#' )
#' split <- make_validation_split(gold, train_frac = 0.75, seed = 1L)
#' length(split$train_ids)
#' nrow(split$validate)
make_validation_split <- function(gold_df,
                                   id_col     = "person_id",
                                   train_frac = 0.7,
                                   seed       = 42L) {
  if (!id_col %in% names(gold_df))
    rlang::abort(sprintf("Column '%s' not found in gold_df.", id_col))
  if (!is.numeric(train_frac) || length(train_frac) != 1L ||
      train_frac <= 0 || train_frac >= 1)
    rlang::abort("train_frac must be a single number strictly between 0 and 1.")

  set.seed(seed)
  ids       <- unique(gold_df[[id_col]])
  n_train   <- max(1L, round(length(ids) * train_frac))
  train_ids <- sort(sample(ids, n_train))
  val_ids   <- sort(setdiff(ids, train_ids))

  list(
    train        = gold_df[gold_df[[id_col]] %in% train_ids, ],
    validate     = gold_df[gold_df[[id_col]] %in% val_ids,   ],
    train_ids    = train_ids,
    validate_ids = val_ids
  )
}
