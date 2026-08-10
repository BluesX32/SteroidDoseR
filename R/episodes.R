# episodes.R
# Build continuous corticosteroid episodes by gap-bridging prescriptions.

#' Merge overlapping or adjacent drug-exposure records into continuous episodes
#'
#' Sorts each patient's prescriptions for each drug chronologically, then
#' merges consecutive records whose gap is within `gap_days` into a single
#' continuous episode. This reproduces the standard "era" logic used by OHDSI
#' `DrugUtilisation` (gap-era = 30 days) and the legacy analysis pipeline.
#'
#' The first argument accepts either a **connector** or a plain **data frame**.
#' When a connector is supplied, raw `drug_exposure` records are fetched first;
#' the returned episodes will have `NA` dose statistics unless dose imputation
#' was performed upstream (use [run_pipeline()] to chain fetch + impute +
#' episode in one call).
#'
#' @param connector_or_df A `steroid_connector` (from [create_omop_connector()]
#'   or [create_df_connector()]) **or** a data frame. Must contain columns for
#'   patient ID, drug name, and start date. End dates and dose columns are
#'   optional.
#' @param person_col `character(1)`. Patient identifier column. Default:
#'   `"person_id"`.
#' @param drug_col `character(1)`. Drug-name column (standardised or raw).
#'   Default: `"drug_name_std"`.
#' @param start_col `character(1)`. Exposure-start date column. Default:
#'   `"drug_exposure_start_date"`.
#' @param end_col `character(1)`. Exposure-end date column. When `NA` (default),
#'   uses `start_col` as both start and end (one-day episode per record).
#'   Supply `"drug_exposure_end_date"` when available.
#' @param dose_col `character(1)`. Daily-dose column. Default:
#'   `"daily_dose_mg_imputed"`. Accepts `"daily_dose_mg"` as alternative --
#'   whichever is present in the data.
#' @param gap_days `integer(1)`. Maximum gap (in days) between consecutive
#'   records that are still bridged into the same episode. Default: `30L`.
#' @param extra_cols `character`. Optional vector of column names present in
#'   `connector_or_df` to propagate into the episode summary by statistical
#'   mode (most common value across the records that form each episode).
#'   Typical use: `extra_cols = "parsed_status"` or `extra_cols = "sig_status"`
#'   to carry NLP parse categories into episode-level output for stratified
#'   evaluation via [evaluate_against_gold()]. Default: `character(0)` (no
#'   extra columns). Columns absent from the data are silently ignored.
#' @param drug_concept_ids,person_ids,start_date,end_date
#'   Connector-path filtering arguments. Ignored when `connector_or_df` is a
#'   data frame. See [calc_daily_dose_baseline()] for full descriptions.
#'
#' @return A data frame with one row per patient-drug episode:
#' \describe{
#'   \item{person_id}{Patient identifier (renamed from `person_col`).}
#'   \item{drug_name_std}{Drug name (renamed from `drug_col`).}
#'   \item{episode_id}{Integer episode counter within patient-drug.}
#'   \item{episode_start}{First day of the episode (`Date`).}
#'   \item{episode_end}{Last day of the episode (`Date`).}
#'   \item{n_days}{Number of calendar days (`episode_end - episode_start + 1`).}
#'   \item{n_records}{Number of original records merged into this episode.}
#'   \item{median_daily_dose}{Median of `dose_col` across merged records.}
#'   \item{min_daily_dose}{Minimum daily dose across merged records.}
#'   \item{max_daily_dose}{Maximum daily dose across merged records.}
#'   \item{mean_daily_dose}{Duration-weighted mean daily dose:
#'     `sum(dose_i * days_i) / sum(days_i)` across non-NA records in the
#'     episode. Use this (via `computed_dose_col = "mean_daily_dose"` in
#'     [evaluate_against_gold()]) to weight longer prescriptions more heavily
#'     than short ones.}
#'   \item{dose_implausible}{`logical`. `TRUE` when `mean_daily_dose < 1` mg/day
#'     — the smallest commercially available tablet strength. These episodes are
#'     almost always PRN artefacts, quantity÷days_supply rounding errors, or
#'     non-oral records that slipped through the route filter. Filter them out
#'     of primary analyses but inspect them rather than silently dropping.}
#'   \item{pulse_episode}{`logical`. `TRUE` when `mean_daily_dose > 100` mg/day
#'     pred-equivalent. Likely IV methylprednisolone pulse courses converted to
#'     oral equivalents. These episodes are real but may not be captured in a
#'     gold standard annotated for chronic oral dosing; report separately.}
#' }
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id                = c(1L, 1L, 1L, 2L),
#'   drug_name_std            = "prednisone",
#'   drug_exposure_start_date = as.Date(
#'     c("2023-01-01","2023-02-10","2023-03-01","2023-01-01")),
#'   drug_exposure_end_date   = as.Date(
#'     c("2023-02-01","2023-02-28","2023-04-01","2023-06-01")),
#'   daily_dose_mg_imputed    = c(20, 15, 10, 5)
#' )
#' build_episodes(df, end_col = "drug_exposure_end_date")
build_episodes <- function(connector_or_df,
                           person_col       = "person_id",
                           drug_col         = "drug_name_std",
                           start_col        = "drug_exposure_start_date",
                           end_col          = NA_character_,
                           dose_col         = NULL,
                           gap_days         = 30L,
                           extra_cols       = character(0),
                           drug_concept_ids = NULL,
                           person_ids       = NULL,
                           start_date       = NULL,
                           end_date         = NULL) {

  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date)

  # When fetched via connector, drug_name_std may not exist yet; derive it.
  if (!drug_col %in% names(drug_df)) {
    name_src <- intersect(c("drug_concept_name", "drug_source_value"),
                          names(drug_df))
    if (length(name_src) > 0L) {
      drug_df[["drug_name_std"]] <- standardize_drug_name(
        drug_df[[name_src[[1L]]]]
      )
    }
  }

  # --- resolve columns -------------------------------------------------------
  assert_required_cols(drug_df, c(person_col, drug_col, start_col), "drug_df")

  # Resolve dose column: accept explicit argument, then try common names
  if (is.null(dose_col)) {
    candidates <- c("daily_dose_mg_imputed", "daily_dose_mg",
                     "median_daily_dose", "mean_daily_dose")
    found_dose <- intersect(candidates, names(drug_df))
    dose_col   <- if (length(found_dose) > 0L) found_dose[[1L]] else NA_character_
  }

  use_end <- !is.na(end_col) && end_col %in% names(drug_df)

  # --- same-day deduplication ------------------------------------------------
  # When a patient has two records for the same drug on the same start date
  # (e.g. duplicate dispenses, correction entries), keep only the row with the
  # highest dose. This prevents artificial inflation of median/mean dose and
  # avoids spurious episode splits at the episode-building stage.
  dose_candidate <- if (!is.null(dose_col) && dose_col %in% names(drug_df))
    dose_col else NULL
  if (!is.null(dose_candidate)) {
    drug_df <- drug_df |>
      dplyr::mutate(.sort_dose = safe_as_numeric(.data[[dose_candidate]])) |>
      dplyr::group_by(
        .data[[person_col]], .data[[drug_col]], .data[[start_col]]
      ) |>
      dplyr::slice_max(.data$.sort_dose, n = 1L, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(-".sort_dose")
  }

  # --- extra_cols: save lookup before transmute strips the data frame --------
  extra_present <- intersect(extra_cols, names(drug_df))
  extra_lookup  <- if (length(extra_present) > 0L) {
    drug_df |>
      dplyr::mutate(
        .__person = .data[[person_col]],
        .__drug   = .data[[drug_col]],
        .__start  = safe_as_date(.data[[start_col]])
      ) |>
      dplyr::select(".__person", ".__drug", ".__start",
                    dplyr::all_of(extra_present)) |>
      dplyr::distinct()
  } else NULL

  # --- normalise to working data frame ---------------------------------------
  wd <- drug_df |>
    dplyr::transmute(
      .person = .data[[person_col]],
      .drug   = .data[[drug_col]],
      .start  = safe_as_date(.data[[start_col]]),
      .end    = if (use_end) safe_as_date(.data[[end_col]]) else .data$.start,
      # guard: end must be >= start
      .end    = dplyr::if_else(is.na(.data$.end) | .data$.end < .data$.start,
                               .data$.start, .data$.end),
      .dose     = if (!is.na(dose_col) && dose_col %in% names(drug_df))
                    safe_as_numeric(.data[[dose_col]])
                  else NA_real_,
      # record duration in days -- used for duration-weighted mean dose
      .rec_days = as.integer(.data$.end - .data$.start) + 1L
    ) |>
    dplyr::filter(!is.na(.data$.person), !is.na(.data$.start)) |>
    dplyr::arrange(.data$.person, .data$.drug, .data$.start, .data$.end)

  if (nrow(wd) == 0L) {
    return(.empty_episodes())
  }

  if (!is.null(extra_lookup)) {
    wd <- wd |>
      dplyr::left_join(
        extra_lookup |>
          dplyr::rename(.person = ".__person",
                        .drug   = ".__drug",
                        .start  = ".__start"),
        by = c(".person", ".drug", ".start"),
        relationship = "many-to-many"
      )
  }

  # --- gap-bridging algorithm (vectorised) -----------------------------------
  wd <- wd |>
    dplyr::group_by(.data$.person, .data$.drug) |>
    dplyr::mutate(
      # running maximum end date (to handle overlapping records)
      .run_max_end = cummax(as.integer(.data$.end)),
      .run_max_end = as.Date(.data$.run_max_end, origin = "1970-01-01"),
      # lag: what was the running-max end of the previous record?
      .prev_end    = dplyr::lag(.data$.run_max_end, default = as.Date(NA)),
      # new episode starts when gap from prev_end to this start > gap_days
      .new_ep      = is.na(.data$.prev_end) |
                     as.integer(.data$.start - .data$.prev_end) > gap_days,
      .episode_id  = cumsum(.data$.new_ep)
    ) |>
    dplyr::ungroup()

  # --- summarise per episode -------------------------------------------------
  episodes <- wd |>
    dplyr::group_by(.data$.person, .data$.drug, .data$.episode_id) |>
    dplyr::summarise(
      episode_start     = min(.data$.start),
      episode_end       = max(.data$.end),
      n_records         = dplyr::n(),
      median_daily_dose = stats::median(.data$.dose, na.rm = TRUE),
      min_daily_dose    = suppressWarnings(min(.data$.dose, na.rm = TRUE)),
      max_daily_dose    = suppressWarnings(max(.data$.dose, na.rm = TRUE)),
      # duration-weighted mean: sum(dose_i * days_i) / sum(days_i for non-NA records)
      mean_daily_dose   = dplyr::if_else(
        any(!is.na(.data$.dose)),
        sum(.data$.dose * .data$.rec_days, na.rm = TRUE) /
          sum(dplyr::if_else(!is.na(.data$.dose), .data$.rec_days, 0L)),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_days = as.integer(.data$episode_end - .data$episode_start) + 1L,
      min_daily_dose = dplyr::if_else(is.infinite(.data$min_daily_dose), NA_real_, .data$min_daily_dose),
      max_daily_dose = dplyr::if_else(is.infinite(.data$max_daily_dose), NA_real_, .data$max_daily_dose)
    ) |>
    dplyr::rename(
      person_id     = ".person",
      drug_name_std = ".drug",
      episode_id    = ".episode_id"
    ) |>
    dplyr::select(
      "person_id", "drug_name_std", "episode_id",
      "episode_start", "episode_end", "n_days", "n_records",
      "median_daily_dose", "min_daily_dose", "max_daily_dose", "mean_daily_dose"
    ) |>
    dplyr::arrange(.data$person_id, .data$drug_name_std, .data$episode_start) |>
    dplyr::mutate(
      dose_implausible = !is.na(.data$mean_daily_dose) & .data$mean_daily_dose < 1,
      pulse_episode    = !is.na(.data$mean_daily_dose) & .data$mean_daily_dose > 100
    )

  # --- propagate extra_cols by statistical mode per episode ------------------
  if (!is.null(extra_lookup) && length(extra_present) > 0L) {
    .str_mode <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) return(NA_character_)
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
    extra_ep <- wd |>
      dplyr::group_by(.data$.person, .data$.drug, .data$.episode_id) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(extra_present), .str_mode),
        .groups = "drop"
      ) |>
      dplyr::rename(
        person_id     = ".person",
        drug_name_std = ".drug",
        episode_id    = ".episode_id"
      )
    episodes <- episodes |>
      dplyr::left_join(extra_ep,
                       by = c("person_id", "drug_name_std", "episode_id"))
  }

  episodes
}

# ---------------------------------------------------------------------------
# Exported: gap_sensitivity
# ---------------------------------------------------------------------------

#' Assess sensitivity of episode structure to gap_days assumption
#'
#' Calls [build_episodes()] repeatedly across a grid of `gap_days` values and
#' returns summary statistics so you can choose a clinically defensible
#' bridging window. Typical OHDSI analyses use 30 days; real-world scripts
#' use 7–90 days.
#'
#' @param drug_df Data frame of dose-imputed drug-exposure records (the same
#'   object you would pass to [build_episodes()]).
#' @param gap_grid `integer` vector. Gap values to test. Default:
#'   `c(0L, 7L, 14L, 30L, 60L, 90L)`.
#' @param ... Additional arguments forwarded to [build_episodes()]
#'   (e.g. `dose_col`, `end_col`, `person_col`, `drug_col`).
#'
#' @return A tibble with one row per `gap_days` value and columns:
#' \describe{
#'   \item{gap_days}{The gap parameter tested.}
#'   \item{n_episodes}{Total number of episodes produced.}
#'   \item{median_episode_days}{Median episode duration (days).}
#'   \item{p25_episode_days}{25th percentile episode duration.}
#'   \item{p75_episode_days}{75th percentile episode duration.}
#'   \item{n_patients}{Number of distinct `person_id` values.}
#'   \item{episodes_per_patient}{Mean episodes per patient.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calc_daily_dose_nlp_advanced(drug_df)
#' gap_sensitivity(df, gap_grid = c(0L, 7L, 30L, 90L))
#' }
gap_sensitivity <- function(drug_df,
                             gap_grid = c(0L, 7L, 14L, 30L, 60L, 90L),
                             ...) {
  results <- lapply(as.integer(gap_grid), function(g) {
    ep <- build_episodes(drug_df, gap_days = g, ...)
    if (nrow(ep) == 0L) {
      return(tibble::tibble(
        gap_days              = g,
        n_episodes            = 0L,
        median_episode_days   = NA_real_,
        p25_episode_days      = NA_real_,
        p75_episode_days      = NA_real_,
        n_patients            = 0L,
        episodes_per_patient  = NA_real_
      ))
    }
    q <- stats::quantile(ep$n_days, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    n_pts <- dplyr::n_distinct(ep$person_id)
    tibble::tibble(
      gap_days             = g,
      n_episodes           = nrow(ep),
      median_episode_days  = q[["50%"]],
      p25_episode_days     = q[["25%"]],
      p75_episode_days     = q[["75%"]],
      n_patients           = n_pts,
      episodes_per_patient = nrow(ep) / n_pts
    )
  })
  dplyr::bind_rows(results)
}

#' Look up the active dose at a set of point-in-time visit dates
#'
#' Cross-sectional companion to [evaluate_against_gold()]'s episode-level
#' comparison. For each row in `visits_df`, finds every row in `episodes_df`
#' for the same patient whose `[episode_start_col, episode_end_col]` window
#' contains `visit_date_col`, and sums `dose_col` across all covering episodes
#' -- this matters when a patient is mid cross-titration between two different
#' steroid drugs, since both episodes contribute to total steroid burden on
#' that date. Visits with no covering episode get `no_coverage_value`.
#'
#' @param episodes_df A data frame of episodes -- typically the output of
#'   [build_episodes()], or a gold-standard data frame from
#'   [parse_steroid_gold()]. Must contain `person_col`, `episode_start_col`,
#'   `episode_end_col`, and `dose_col`.
#' @param visits_df A data frame of point-in-time encounters (e.g. office
#'   visits from [fetch_visit_occurrence()]). Must contain `person_col` and
#'   `visit_date_col`.
#' @param person_col `character(1)`. Patient identifier column, shared by
#'   both data frames. Default `"person_id"`.
#' @param visit_date_col `character(1)`. Encounter date column in
#'   `visits_df`. Default `"visit_date"`.
#' @param episode_start_col,episode_end_col `character(1)`. Episode window
#'   columns in `episodes_df`. Defaults `"episode_start"`, `"episode_end"`.
#' @param dose_col `character(1)`. Dose column in `episodes_df`. Default
#'   `"mean_daily_dose"`.
#' @param no_coverage_value `numeric(1)`. Value assigned to `dose_mg` when no
#'   episode covers the visit date. Default `0` -- no active episode means
#'   "not on steroids" per that method, a real and comparable clinical state.
#'   Pass `NA_real_` when `episodes_df` is a gold standard: a coverage gap
#'   there means "not chart-reviewed at this date," not "confirmed off
#'   steroids," and should not be scored as a 0 mg agreement.
#'
#' @return `visits_df` with two columns appended: `dose_mg` (numeric; summed
#'   across covering episodes, `NA` if all covering episodes have unknown
#'   dose, `no_coverage_value` if none cover the date) and `has_coverage`
#'   (logical, `TRUE` when at least one episode covered the visit date).
#'
#' @seealso [build_episodes()], [evaluate_against_gold()],
#'   [dose_agreement_metrics()], [fetch_visit_occurrence()]
#'
#' @export
#'
#' @examples
#' episodes <- tibble::tibble(
#'   person_id       = 1L,
#'   episode_start   = as.Date("2023-01-01"),
#'   episode_end     = as.Date("2023-03-31"),
#'   mean_daily_dose = 10
#' )
#' visits <- tibble::tibble(
#'   person_id  = c(1L, 1L),
#'   visit_date = as.Date(c("2023-02-01", "2023-06-01"))
#' )
#' dose_at_visits(episodes, visits)
dose_at_visits <- function(episodes_df,
                           visits_df,
                           person_col        = "person_id",
                           visit_date_col    = "visit_date",
                           episode_start_col = "episode_start",
                           episode_end_col   = "episode_end",
                           dose_col          = "mean_daily_dose",
                           no_coverage_value = 0) {

  assert_required_cols(
    episodes_df,
    c(person_col, episode_start_col, episode_end_col, dose_col),
    "episodes_df"
  )
  assert_required_cols(visits_df, c(person_col, visit_date_col), "visits_df")

  ep <- episodes_df |>
    dplyr::transmute(
      .person  = .data[[person_col]],
      .e_start = safe_as_date(.data[[episode_start_col]]),
      .e_end   = safe_as_date(.data[[episode_end_col]]),
      .dose    = safe_as_numeric(.data[[dose_col]])
    )

  vs <- visits_df |>
    dplyr::mutate(
      .__row_id = dplyr::row_number(),
      .person   = .data[[person_col]],
      .v_date   = safe_as_date(.data[[visit_date_col]])
    )

  covering <- vs |>
    dplyr::select(".__row_id", ".person", ".v_date") |>
    dplyr::left_join(ep, by = ".person", relationship = "many-to-many") |>
    dplyr::filter(
      !is.na(.data$.e_start), !is.na(.data$.e_end),
      .data$.v_date >= .data$.e_start,
      .data$.v_date <= .data$.e_end
    ) |>
    dplyr::group_by(.data$.__row_id) |>
    dplyr::summarise(
      dose_mg      = dplyr::if_else(
        all(is.na(.data$.dose)), NA_real_, sum(.data$.dose, na.rm = TRUE)
      ),
      has_coverage = TRUE,
      .groups = "drop"
    )

  vs |>
    dplyr::left_join(covering, by = ".__row_id") |>
    dplyr::mutate(
      has_coverage = dplyr::coalesce(.data$has_coverage, FALSE),
      dose_mg      = dplyr::if_else(
        .data$has_coverage, .data$dose_mg, no_coverage_value
      )
    ) |>
    dplyr::select(-".__row_id", -".person", -".v_date")
}

#' Approximate trajectory truth by carrying the last reviewed gold dose forward
#'
#' Implements the "same dose from one visit to the next" assumption: for each
#' patient, sorts visits chronologically and fills unreviewed visits with the
#' most recent *actually reviewed* dose (last-observation-carried-forward,
#' never backward). This turns the sparse, visit-level gold coverage from
#' [dose_at_visits()] into a denser series usable for validating dose
#' *trajectories* -- something the manually reviewed gold standard alone
#' cannot support, since it only covers discrete chart-reviewed intervals.
#'
#' This is an assumption, not a fact: a patient's dose could genuinely change
#' between visits without being caught by chart review, and this function has
#' no way to detect that -- it will systematically understate real dose
#' variability (most acutely during tapers, when the dose is *expected* to
#' change between visits). The output's `dose_source` column exists so
#' callers can always report accuracy separately for `"reviewed"` visits
#' (real chart review) versus `"reviewed"` + `"carried_forward"` visits
#' (includes the assumption) -- never blend the two into one number without
#' saying so.
#'
#' Intended for gold-standard series specifically: run this on the output of
#' `dose_at_visits(gold_std, visits_df, ..., no_coverage_value = NA_real_)`.
#' It should not be run on a computed method's series, where `dose_mg = 0`
#' at an uncovered visit is already a real answer ("not on steroids"), not a
#' gap to fill.
#'
#' @param visits_df Output of [dose_at_visits()] (or any data frame with the
#'   same shape): one row per visit, with `person_col`, `visit_date_col`,
#'   `dose_col` (`NA` where unreviewed), and `coverage_col` (`TRUE` where
#'   `dose_col` reflects an actual chart review).
#' @param person_col `character(1)`. Patient identifier column. Default
#'   `"person_id"`.
#' @param visit_date_col `character(1)`. Encounter date column. Default
#'   `"visit_date"`.
#' @param dose_col `character(1)`. Dose column to fill. Default `"dose_mg"`.
#' @param coverage_col `character(1)`. Logical column marking actually
#'   reviewed visits. Default `"has_coverage"`.
#'
#' @return `visits_df`, row order preserved, with `dose_col` forward-filled
#'   per patient and one column added, `dose_source`:
#' \describe{
#'   \item{`"reviewed"`}{`coverage_col` was `TRUE` -- untouched, real review.}
#'   \item{`"carried_forward"`}{`dose_col` is now non-`NA` only because of
#'     the forward-fill.}
#'   \item{`"unknown"`}{Still `NA` -- no reviewed visit yet exists for this
#'     patient at or before this date.}
#' }
#'
#' @seealso [dose_at_visits()], [dose_agreement_metrics()]
#'
#' @export
#'
#' @examples
#' gold_at_visits <- tibble::tibble(
#'   person_id    = c(1L, 1L, 1L),
#'   visit_date   = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   dose_mg      = c(10, NA, NA),
#'   has_coverage = c(TRUE, FALSE, FALSE)
#' )
#' carry_forward_dose(gold_at_visits)
carry_forward_dose <- function(visits_df,
                               person_col     = "person_id",
                               visit_date_col = "visit_date",
                               dose_col       = "dose_mg",
                               coverage_col   = "has_coverage") {

  assert_required_cols(
    visits_df, c(person_col, visit_date_col, dose_col, coverage_col),
    "visits_df"
  )

  visits_df |>
    dplyr::mutate(.__orig_order = dplyr::row_number()) |>
    dplyr::arrange(.data[[person_col]], safe_as_date(.data[[visit_date_col]])) |>
    dplyr::group_by(.data[[person_col]]) |>
    tidyr::fill(dplyr::all_of(dose_col), .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dose_source = dplyr::case_when(
        .data[[coverage_col]]     ~ "reviewed",
        !is.na(.data[[dose_col]]) ~ "carried_forward",
        TRUE                      ~ "unknown"
      )
    ) |>
    dplyr::arrange(.data$.__orig_order) |>
    dplyr::select(-".__orig_order")
}

.empty_episodes <- function() {
  tibble::tibble(
    person_id         = character(0),
    drug_name_std     = character(0),
    episode_id        = integer(0),
    episode_start     = as.Date(character(0)),
    episode_end       = as.Date(character(0)),
    n_days            = integer(0),
    n_records         = integer(0),
    median_daily_dose = numeric(0),
    min_daily_dose    = numeric(0),
    max_daily_dose    = numeric(0),
    mean_daily_dose   = numeric(0),
    dose_implausible  = logical(0),
    pulse_episode     = logical(0)
  )
}
