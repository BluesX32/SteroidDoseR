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
    dplyr::arrange(.data$person_id, .data$drug_name_std, .data$episode_start)

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
    mean_daily_dose   = numeric(0)
  )
}
