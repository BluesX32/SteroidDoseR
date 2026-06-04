# gold_standard.R
# Parse and validate DMARD gold-standard data from clinical review CSVs.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Empty parse-row template for failed DMARD dose parses
#' @noRd
.empty_dmard_parse_row <- function() {
  tibble::tibble(
    dose_amount         = NA_real_,
    dose_unit           = NA_character_,
    dose_per_kg         = NA,
    dose_frequency      = NA_character_,
    freq_per_day        = NA_real_,
    dose_mg_per_admin   = NA_real_,
    dose_daily_mg_equiv = NA_real_,
    parse_status        = NA_character_
  )
}

#' Extract DMARD frequency label and doses-per-day from a normalised string
#' @noRd
.extract_dmard_freq <- function(s) {
  no_freq <- list(label = NA_character_, per_day = NA_real_)

  if (stringr::str_detect(s, "\\bqid\\b|four\\s+times|4\\s*x\\s*(?:per\\s*)?day"))
    return(list(label = "qid",     per_day = 4))

  if (stringr::str_detect(s, "\\btid\\b|three\\s+times|3\\s*x\\s*(?:per\\s*)?day"))
    return(list(label = "tid",     per_day = 3))

  if (stringr::str_detect(s, "\\bbid\\b|twice\\s+daily|2\\s*x\\s*(?:per\\s*)?day|2\\s*times\\s*(?:a\\s*|per\\s*)?day"))
    return(list(label = "bid",     per_day = 2))

  if (stringr::str_detect(s, "\\bdaily\\b|once\\s+daily|\\bqd\\b|q\\s+day\\b|/\\s*day\\b|per\\s*day|/\\s*d\\b"))
    return(list(label = "daily",   per_day = 1))

  # Every N months -- check before standalone "monthly"
  qn_month <- stringr::str_match(s, "(?:q|every)\\s*(\\d+)\\s*months?")
  if (!is.na(qn_month[1L, 1L])) {
    n     <- as.numeric(qn_month[1L, 2L])
    label <- if (n == 1L) "monthly" else paste0("q", n, "months")
    return(list(label = label, per_day = 1 / (n * 30)))
  }

  # Every N weeks -- check before standalone "weekly"
  qn_week <- stringr::str_match(s, "(?:q|every)\\s*(\\d+)\\s*w(?:ee)?ks?")
  if (!is.na(qn_week[1L, 1L])) {
    n     <- as.numeric(qn_week[1L, 2L])
    label <- if (n == 1L) "weekly" else paste0("q", n, "weeks")
    return(list(label = label, per_day = 1 / (n * 7)))
  }

  # "q week" / "q wk" without a number -> weekly
  if (stringr::str_detect(s, "\\bq\\s*(?:wk|week)\\b|once\\s+(?:a\\s*)?week|once\\s+weekly"))
    return(list(label = "weekly",  per_day = 1 / 7))

  if (stringr::str_detect(s, "\\bweekly\\b"))
    return(list(label = "weekly",  per_day = 1 / 7))

  if (stringr::str_detect(s, "\\bmonthly\\b|once\\s+(?:a\\s*)?month|q\\s*month\\b"))
    return(list(label = "monthly", per_day = 1 / 30))

  no_freq
}

#' Core implementation for a single DMARD dose string (never called directly)
#' @noRd
.parse_dmard_dose_one_impl <- function(x) {
  empty <- .empty_dmard_parse_row()

  if (is.na(x) || !nzchar(stringr::str_trim(as.character(x)))) {
    empty$parse_status <- "empty"
    return(empty)
  }

  s <- stringr::str_squish(stringr::str_to_lower(as.character(x)))

  # Step 1 -- strip route abbreviations
  s <- stringr::str_remove_all(s, "\\bpo\\b|\\boral(ly)?\\b|\\biv\\b|\\bsubq\\b|\\bsc\\b")
  s <- stringr::str_squish(s)

  # Step 2 -- strip leading alphabetic drug-name prefix (e.g. "prednisone 5 mg")
  s <- stringr::str_remove(s, "^(?:[a-z]+\\s+)+(?=\\d)")
  s <- stringr::str_squish(s)

  # Special case -- "X grams over N days" (IVIG infusion course)
  over_m <- stringr::str_match(
    s, "(\\d+(?:\\.\\d+)?)\\s*(?:grams?|g)\\s+over\\s+(\\d+(?:\\.\\d+)?)\\s*days?"
  )
  if (!is.na(over_m[1L, 1L])) {
    amount_g <- as.numeric(over_m[1L, 2L])
    n_days   <- as.numeric(over_m[1L, 3L])
    return(tibble::tibble(
      dose_amount         = amount_g,
      dose_unit           = "g",
      dose_per_kg         = FALSE,
      dose_frequency      = "infusion_course",
      freq_per_day        = 1 / n_days,
      dose_mg_per_admin   = amount_g * 1000,
      dose_daily_mg_equiv = (amount_g * 1000) / n_days,
      parse_status        = "ok"
    ))
  }

  # Step 3 -- detect per-kg flag, then strip "/kg"
  dose_per_kg <- stringr::str_detect(s, "/\\s*kg\\b")
  s           <- stringr::str_remove_all(s, "/\\s*kg\\b")
  s           <- stringr::str_squish(s)

  # Step 4 -- extract amount + unit
  mg_m <- stringr::str_match(s, "(\\d+(?:\\.\\d+)?)\\s*mg\\b")
  g_m  <- stringr::str_match(s, "(\\d+(?:\\.\\d+)?)\\s*(?:grams?|g)\\b")

  if (!is.na(mg_m[1L, 1L])) {
    dose_amount <- as.numeric(mg_m[1L, 2L])
    dose_unit   <- "mg"
  } else if (!is.na(g_m[1L, 1L])) {
    dose_amount <- as.numeric(g_m[1L, 2L])
    dose_unit   <- "g"
  } else {
    # Bare number with no unit
    num_m <- stringr::str_match(s, "^(\\d+(?:\\.\\d+)?)")
    if (!is.na(num_m[1L, 1L])) {
      dose_amount <- as.numeric(num_m[1L, 2L])
      dose_unit   <- NA_character_
    } else {
      empty$parse_status <- "no_parse"
      return(empty)
    }
  }

  # Step 5 -- extract frequency
  freq           <- .extract_dmard_freq(s)
  dose_frequency <- freq$label
  freq_per_day   <- freq$per_day

  # Step 6 -- compute daily equivalent
  if (dose_per_kg) {
    dose_mg_per_admin   <- NA_real_
    dose_daily_mg_equiv <- NA_real_
    parse_status        <- "weight_required"
  } else if (is.na(dose_unit)) {
    dose_mg_per_admin   <- NA_real_
    dose_daily_mg_equiv <- NA_real_
    parse_status        <- "no_unit"
  } else if (is.na(freq_per_day)) {
    dose_mg_per_admin   <- dose_amount * if (dose_unit == "g") 1000 else 1
    dose_daily_mg_equiv <- NA_real_
    parse_status        <- "no_freq"
  } else {
    dose_mg_per_admin   <- dose_amount * if (dose_unit == "g") 1000 else 1
    dose_daily_mg_equiv <- dose_mg_per_admin * freq_per_day
    parse_status        <- "ok"
  }

  tibble::tibble(
    dose_amount         = dose_amount,
    dose_unit           = dose_unit,
    dose_per_kg         = dose_per_kg,
    dose_frequency      = dose_frequency,
    freq_per_day        = freq_per_day,
    dose_mg_per_admin   = dose_mg_per_admin,
    dose_daily_mg_equiv = dose_daily_mg_equiv,
    parse_status        = parse_status
  )
}

#' Parse a single DMARD dose string (tryCatch wrapper -- never throws)
#' @noRd
.parse_dmard_dose_one <- function(x) {
  tryCatch(
    .parse_dmard_dose_one_impl(x),
    error = function(e) {
      r <- .empty_dmard_parse_row()
      r$parse_status <- "error"
      r
    }
  )
}

# ---------------------------------------------------------------------------
# Exported: parse_dmard_dose()
# ---------------------------------------------------------------------------

#' Parse a vector of free-text DMARD dose strings
#'
#' Vectorised wrapper over the internal single-record parser. Each string is
#' normalised, then amount, unit, per-kg flag, and frequency are extracted via
#' regex. A daily mg equivalent is computed where possible.
#'
#' @param x Character vector of free-text dose strings (e.g. `"2g q6 months"`,
#'   `"1000 mg PO BID"`, `"1g/kg/q2 wk"`).
#'
#' @return A tibble with one row per element of `x` and columns:
#' \describe{
#'   \item{`dose_amount`}{Numeric dose amount as written (before unit conversion).}
#'   \item{`dose_unit`}{`"mg"` or `"g"`; `NA` when no unit found.}
#'   \item{`dose_per_kg`}{`TRUE` when `/kg` detected; patient weight required
#'     to compute a daily mg equivalent.}
#'   \item{`dose_frequency`}{Standardised label: `"daily"`, `"bid"`, `"weekly"`,
#'     `"q2weeks"`, `"q4weeks"`, `"monthly"`, `"q3months"`, `"q6months"`,
#'     `"infusion_course"`, etc.; `NA` when not parseable.}
#'   \item{`freq_per_day`}{Numeric doses per calendar day corresponding to
#'     `dose_frequency` (e.g. `1/180` for `"q6months"`); `NA` when unknown.}
#'   \item{`dose_mg_per_admin`}{`dose_amount` converted to mg (g * 1000);
#'     `NA` when `dose_per_kg` is `TRUE`.}
#'   \item{`dose_daily_mg_equiv`}{`dose_mg_per_admin * freq_per_day`; `NA`
#'     when `dose_per_kg` is `TRUE` or frequency is unknown.}
#'   \item{`parse_status`}{`"ok"`, `"weight_required"`, `"no_freq"`,
#'     `"no_unit"`, `"no_parse"`, `"empty"`, or `"error"`.}
#' }
#'
#' @export
#'
#' @examples
#' parse_dmard_dose(c(
#'   "1000 mg PO BID",
#'   "2g q6 months",
#'   "1g/kg/q2 wk",
#'   "3g/d",
#'   "8.75",
#'   "every 6 months",
#'   "120 grams over 6 days"
#' ))
parse_dmard_dose <- function(x) {
  purrr::map_dfr(as.character(x), .parse_dmard_dose_one)
}

# ---------------------------------------------------------------------------
# Exported: parse_dmard_gold()
# ---------------------------------------------------------------------------

#' Parse and validate a DMARD gold-standard data frame
#'
#' Accepts a raw clinical review CSV (typically one row per DMARD record per
#' patient) with messy free-text dose expressions. Validates required columns,
#' derives episode end-dates from status logic, and parses the dose column via
#' [parse_dmard_dose()]. All column name arguments have defaults matching the
#' actual field names used in the myositis clinical review export, but every
#' name can be overridden to accommodate different site exports.
#'
#' @param df A data frame -- the raw gold-standard CSV loaded with
#'   `readr::read_csv()` or equivalent.
#' @param person_id_col `character(1)`. Patient identifier column.
#'   Default: `"myositis_omop_person_id"`.
#' @param drug_name_col `character(1)`. DMARD name column (free text).
#'   Default: `"dmardname"`.
#' @param dose_col `character(1)`. Free-text dose expression column.
#'   Default: `"dmarddose"`.
#' @param start_date_col `character(1)`. Estimated DMARD start date column.
#'   Default: `"dmard_start_date_est"`.
#' @param status_col `character(1)` or `NULL`. Column recording DMARD status.
#'   Set `NULL` to skip status logic. If declared but absent, a warning is
#'   issued. Default: `"dmardstatus"`.
#' @param last_changed_col `character(1)` or `NULL`. Timestamp column
#'   recording when the record was last updated. Used as the primary source
#'   for `episode_end` before falling back to `stop_date_col` or `today`.
#'   Set `NULL` to skip. If declared but absent, a warning is issued.
#'   Default: `"last_changed_datetime"`.
#' @param stop_date_col `character(1)` or `NULL`. Estimated stop-date column.
#'   Used only when status is past AND `last_changed_col` is absent or NA.
#'   Set `NULL` to skip. If declared but absent, a warning is issued.
#'   Default: `"pastdmard_stop_date_est"`.
#' @param end_date_col `character(1)` or `NULL`. Generic end-date column used
#'   as a fallback when `status_col` is absent. Set `NULL` to skip.
#'   Default: `NULL`.
#' @param past_status_val `character(1)`. The value in `status_col` that
#'   indicates a discontinued DMARD. Default: `"past"`.
#' @param today `Date`. Reference date used as `episode_end` when no end date
#'   can be derived. Default: `Sys.Date()`.
#'
#' @details
#' **End-date priority logic:**
#'
#' When `status_col` is present:
#' \itemize{
#'   \item Status = past: `last_changed_col` -> `stop_date_col` -> `NA`
#'   \item Status = current: `last_changed_col` -> `today`
#' }
#' When `status_col` is absent:
#' \itemize{
#'   \item `last_changed_col` -> `end_date_col` -> `today`
#' }
#'
#' @return A tibble with one row per input row:
#' \describe{
#'   \item{`person_id`}{From `person_id_col`.}
#'   \item{`drug_name_std`}{Lowercased and whitespace-trimmed `drug_name_col`.}
#'   \item{`episode_start`}{`start_date_col` coerced to `Date`.}
#'   \item{`episode_end`}{Derived from end-date priority logic (see Details).}
#'   \item{`dose_raw`}{Original dose string from `dose_col`.}
#'   \item{`dose_amount`, `dose_unit`, `dose_per_kg`, `dose_frequency`,
#'     `freq_per_day`, `dose_mg_per_admin`, `dose_daily_mg_equiv`,
#'     `parse_status`}{See [parse_dmard_dose()] for full descriptions.}
#' }
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   myositis_omop_person_id = c(1L, 2L, 3L),
#'   dmardname               = c("methotrexate", "mycophenolate", "rituximab"),
#'   dmarddose               = c("15 mg weekly", "1500 mg daily", "1000 mg q6 months"),
#'   dmard_start_date_est    = c("2020-01-01", "2019-06-01", "2021-03-01"),
#'   dmardstatus             = c("active", "past", "active"),
#'   pastdmard_stop_date_est = c(NA, "2022-12-31", NA)
#' )
#' parse_dmard_gold(df)
parse_dmard_gold <- function(df,
                              person_id_col    = "myositis_omop_person_id",
                              drug_name_col    = "dmardname",
                              dose_col         = "dmarddose",
                              start_date_col   = "dmard_start_date_est",
                              status_col       = "dmardstatus",
                              last_changed_col = "last_changed_datetime",
                              stop_date_col    = "pastdmard_stop_date_est",
                              end_date_col     = NULL,
                              past_status_val  = "past",
                              today            = Sys.Date()) {

  # --- required columns -------------------------------------------------------
  assert_required_cols(
    df,
    c(person_id_col, drug_name_col, dose_col, start_date_col),
    "df"
  )

  # --- optional columns: NULL out if declared but absent ----------------------
  .opt <- function(col, label) {
    if (!is.null(col) && !col %in% names(df)) {
      rlang::warn(paste0(label, " '", col, "' not found in df; ignoring."))
      NULL
    } else {
      col
    }
  }
  status_col       <- .opt(status_col,       "status_col")
  last_changed_col <- .opt(last_changed_col, "last_changed_col")
  stop_date_col    <- .opt(stop_date_col,    "stop_date_col")
  end_date_col     <- .opt(end_date_col,     "end_date_col")

  # --- build base working frame -----------------------------------------------
  base <- df |>
    dplyr::transmute(
      person_id        = .data[[person_id_col]],
      drug_name_std    = stringr::str_squish(
                           stringr::str_to_lower(as.character(.data[[drug_name_col]]))
                         ),
      episode_start    = safe_as_date(.data[[start_date_col]]),
      dose_raw         = as.character(.data[[dose_col]]),
      .status          = if (!is.null(status_col))
                           as.character(.data[[status_col]])
                         else NA_character_,
      .last_changed    = if (!is.null(last_changed_col))
                           safe_as_date(.data[[last_changed_col]])
                         else as.Date(NA_character_),
      .stop_raw        = if (!is.null(stop_date_col))
                           safe_as_date(.data[[stop_date_col]])
                         else as.Date(NA_character_),
      .end_raw         = if (!is.null(end_date_col))
                           safe_as_date(.data[[end_date_col]])
                         else as.Date(NA_character_)
    )

  today_date <- as.Date(today)
  has_status <- !is.na(base$.status)
  is_past    <- has_status & base$.status == past_status_val

  # --- derive episode_end from priority logic ---------------------------------
  #
  # Status exists, past:    last_changed -> stop_date -> NA
  # Status exists, current: last_changed -> today
  # No status:              last_changed -> end_date  -> today
  base <- base |>
    dplyr::mutate(
      episode_end = dplyr::case_when(
        # past: last_changed first
        is_past & !is.na(.data$.last_changed)                          ~ .data$.last_changed,
        # past: fall back to stop_date
        is_past & !is.na(.data$.stop_raw)                              ~ .data$.stop_raw,
        # past: no date available
        is_past                                                         ~ as.Date(NA_character_),
        # current (status known, not past): last_changed -> today
        has_status & !is.na(.data$.last_changed)                       ~ .data$.last_changed,
        has_status                                                      ~ today_date,
        # no status: last_changed -> end_date -> today
        !has_status & !is.na(.data$.last_changed)                      ~ .data$.last_changed,
        !has_status & !is.na(.data$.end_raw)                           ~ .data$.end_raw,
        TRUE                                                            ~ today_date
      )
    )

  n_null <- sum(is_past & is.na(base$.last_changed) & is.na(base$.stop_raw))
  if (n_null > 0L) {
    rlang::warn(paste0(
      n_null, " past row(s) have neither last_changed nor stop_date -- ",
      "episode_end set to NA."
    ))
  }

  # --- parse dose column ------------------------------------------------------
  parsed <- parse_dmard_dose(base$dose_raw)

  # --- assemble output --------------------------------------------------------
  base |>
    dplyr::select("person_id", "drug_name_std", "episode_start", "episode_end",
                  "dose_raw") |>
    dplyr::bind_cols(parsed)
}

# ---------------------------------------------------------------------------
# Exported: compare_dmard_episodes()
# ---------------------------------------------------------------------------

#' Compare DMARD gold-standard records against computed drug episodes
#'
#' For each gold-standard record (patient + drug + date window), finds all
#' computed episodes for the same patient and drug that overlap the gold
#' window, clips each to the intersection, then computes a duration-weighted
#' mean dose. Error metrics are calculated against the gold dose.
#'
#' All column name arguments have defaults matching the output of
#' [parse_dmard_gold()] and [build_episodes()], but every name can be
#' overridden.
#'
#' @param computed_df Data frame of computed drug episodes (output of
#'   [build_episodes()]). Must contain columns named by `computed_id_col`,
#'   `computed_drug_col`, `computed_start_col`, `computed_end_col`, and
#'   `computed_dose_col`.
#' @param gold_df Data frame of parsed gold-standard records (output of
#'   [parse_dmard_gold()]). Must contain columns named by `gold_id_col`,
#'   `gold_drug_col`, `gold_start_col`, `gold_end_col`, and `gold_dose_col`.
#' @param computed_id_col `character(1)`. Patient ID in `computed_df`.
#'   Default: `"person_id"`.
#' @param computed_drug_col `character(1)`. Drug name in `computed_df`.
#'   Default: `"drug_name_std"`.
#' @param computed_start_col `character(1)`. Episode start in `computed_df`.
#'   Default: `"episode_start"`.
#' @param computed_end_col `character(1)`. Episode end in `computed_df`.
#'   Default: `"episode_end"`.
#' @param computed_dose_col `character(1)`. Dose column in `computed_df`.
#'   Default: `"median_daily_dose"`.
#' @param gold_id_col `character(1)`. Patient ID in `gold_df`.
#'   Default: `"person_id"`.
#' @param gold_drug_col `character(1)`. Drug name in `gold_df`.
#'   Default: `"drug_name_std"`.
#' @param gold_start_col `character(1)`. Gold window start in `gold_df`.
#'   Default: `"episode_start"`.
#' @param gold_end_col `character(1)`. Gold window end in `gold_df`.
#'   Default: `"episode_end"`.
#' @param gold_dose_col `character(1)`. Gold dose in `gold_df`.
#'   Default: `"dose_daily_mg_equiv"`.
#' @param min_overlap_days `integer(1)`. Minimum overlap in days for a
#'   computed episode to count. Default: `1L`.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`$comparison`}{One row per gold record. Key columns:
#'     `person_id`, `drug_name_std`, `gold_start`, `gold_end`,
#'     `gold_dose`, `computed_dose` (duration-weighted mean),
#'     `n_computed_episodes`, `total_overlap_days`, `gold_duration`,
#'     `overlap_pct`, `absolute_error`, `bias_error`,
#'     `relative_error_pct`, `absolute_relative_error_pct`,
#'     `agreement_category`, `error_direction`.}
#'   \item{`$summary`}{One-row tibble: `n_gold_records`,
#'     `n_matched_records`, `coverage_pct`, `MAE`, `MBE`, `RMSE`,
#'     `median_AE`, `MAPE`, `mean_relative_bias_pct`,
#'     `pearson_corr`, `spearman_corr`.}
#'   \item{`$stratified`}{List with `by_drug` (metrics per
#'     `drug_name_std`).}
#' }
#'
#' @export
#'
#' @examples
#' computed <- tibble::tibble(
#'   person_id         = 1L,
#'   drug_name_std     = "methotrexate",
#'   episode_start     = as.Date("2020-01-01"),
#'   episode_end       = as.Date("2022-12-31"),
#'   median_daily_dose = 15 / 7   # 15 mg/week -> daily equiv
#' )
#' gold <- tibble::tibble(
#'   person_id           = 1L,
#'   drug_name_std       = "methotrexate",
#'   episode_start       = as.Date("2020-06-01"),
#'   episode_end         = as.Date("2022-06-01"),
#'   dose_daily_mg_equiv = 15 / 7
#' )
#' compare_dmard_episodes(computed, gold)
compare_dmard_episodes <- function(computed_df,
                                    gold_df,
                                    computed_id_col    = "person_id",
                                    computed_drug_col  = "drug_name_std",
                                    computed_start_col = "episode_start",
                                    computed_end_col   = "episode_end",
                                    computed_dose_col  = "median_daily_dose",
                                    gold_id_col        = "person_id",
                                    gold_drug_col      = "drug_name_std",
                                    gold_start_col     = "episode_start",
                                    gold_end_col       = "episode_end",
                                    gold_dose_col      = "dose_daily_mg_equiv",
                                    min_overlap_days   = 1L) {

  assert_required_cols(computed_df,
    c(computed_id_col, computed_drug_col, computed_start_col,
      computed_end_col, computed_dose_col),
    "computed_df")
  assert_required_cols(gold_df,
    c(gold_id_col, gold_drug_col, gold_start_col,
      gold_end_col, gold_dose_col),
    "gold_df")

  # --- rename to internal names -----------------------------------------------
  comp <- computed_df |>
    dplyr::rename(
      person_id     = dplyr::all_of(computed_id_col),
      drug_name_std = dplyr::all_of(computed_drug_col),
      c_start       = dplyr::all_of(computed_start_col),
      c_end         = dplyr::all_of(computed_end_col),
      c_dose        = dplyr::all_of(computed_dose_col)
    ) |>
    dplyr::mutate(
      c_start = safe_as_date(.data$c_start),
      c_end   = safe_as_date(.data$c_end),
      c_dose  = safe_as_numeric(.data$c_dose)
    )

  gold <- gold_df |>
    dplyr::rename(
      person_id     = dplyr::all_of(gold_id_col),
      drug_name_std = dplyr::all_of(gold_drug_col),
      g_start       = dplyr::all_of(gold_start_col),
      g_end         = dplyr::all_of(gold_end_col),
      gold_dose     = dplyr::all_of(gold_dose_col)
    ) |>
    dplyr::mutate(
      g_start   = safe_as_date(.data$g_start),
      g_end     = safe_as_date(.data$g_end),
      gold_dose = safe_as_numeric(.data$gold_dose)
    ) |>
    dplyr::filter(!is.na(.data$g_start), !is.na(.data$g_end))

  # --- many-to-many join: same patient + drug ---------------------------------
  joined <- gold |>
    dplyr::left_join(
      comp,
      by           = c("person_id", "drug_name_std"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      ovlp_start = pmax(.data$g_start, .data$c_start, na.rm = FALSE),
      ovlp_end   = pmin(.data$g_end,   .data$c_end,   na.rm = FALSE),
      ovlp_days  = as.integer(.data$ovlp_end - .data$ovlp_start) + 1L,
      ovlp_days  = dplyr::if_else(
                     is.na(.data$ovlp_days) | .data$ovlp_days < 1L,
                     0L, .data$ovlp_days)
    ) |>
    dplyr::filter(.data$ovlp_days >= min_overlap_days)

  # --- duration-weighted mean dose per gold record ----------------------------
  weighted <- joined |>
    dplyr::group_by(.data$person_id, .data$drug_name_std,
                    .data$g_start, .data$g_end, .data$gold_dose) |>
    dplyr::summarise(
      computed_dose       = sum(.data$c_dose * .data$ovlp_days, na.rm = TRUE) /
                              sum(.data$ovlp_days),
      n_computed_episodes = dplyr::n(),
      total_overlap_days  = sum(.data$ovlp_days),
      .groups = "drop"
    )

  # --- build comparison (one row per gold record, even if unmatched) ----------
  comparison <- gold |>
    dplyr::left_join(
      weighted,
      by = c("person_id", "drug_name_std", "g_start", "g_end", "gold_dose")
    ) |>
    dplyr::rename(gold_start = "g_start", gold_end = "g_end") |>
    dplyr::mutate(
      gold_duration = as.integer(.data$gold_end - .data$gold_start) + 1L,
      overlap_pct   = 100 * .data$total_overlap_days / .data$gold_duration,

      absolute_error              = abs(.data$computed_dose - .data$gold_dose),
      bias_error                  = .data$computed_dose - .data$gold_dose,
      relative_error_pct          = (.data$computed_dose - .data$gold_dose) /
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

  n_gold    <- nrow(gold)
  n_matched <- sum(!is.na(comparison$computed_dose))
  matched   <- comparison |> dplyr::filter(!is.na(.data$computed_dose))

  # --- summary metrics --------------------------------------------------------
  pcor <- if (nrow(matched) >= 3L)
    stats::cor(matched$computed_dose, matched$gold_dose,
               use = "complete.obs", method = "pearson")
  else NA_real_

  scor <- if (nrow(matched) >= 3L)
    stats::cor(matched$computed_dose, matched$gold_dose,
               use = "complete.obs", method = "spearman")
  else NA_real_

  summary_tbl <- tibble::tibble(
    n_gold_records         = n_gold,
    n_matched_records      = n_matched,
    coverage_pct           = 100 * n_matched / n_gold,
    MAE                    = mean(matched$absolute_error,              na.rm = TRUE),
    MBE                    = mean(matched$bias_error,                  na.rm = TRUE),
    RMSE                   = sqrt(mean(matched$bias_error^2,           na.rm = TRUE)),
    median_AE              = stats::median(matched$absolute_error,     na.rm = TRUE),
    MAPE                   = mean(matched$absolute_relative_error_pct, na.rm = TRUE),
    mean_relative_bias_pct = mean(matched$relative_error_pct,         na.rm = TRUE),
    pearson_corr           = pcor,
    spearman_corr          = scor
  )

  # --- stratified by drug -----------------------------------------------------
  strat_drug <- comparison |>
    dplyr::filter(!is.na(.data$computed_dose)) |>
    dplyr::group_by(.data$drug_name_std) |>
    dplyr::summarise(
      n    = dplyr::n(),
      MAE  = mean(.data$absolute_error,              na.rm = TRUE),
      MBE  = mean(.data$bias_error,                  na.rm = TRUE),
      MAPE = mean(.data$absolute_relative_error_pct, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    comparison = comparison,
    summary    = summary_tbl,
    stratified = list(by_drug = strat_drug)
  )
}
