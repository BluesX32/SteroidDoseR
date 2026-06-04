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

  # Every N months — check before standalone "monthly"
  qn_month <- stringr::str_match(s, "(?:q|every)\\s*(\\d+)\\s*months?")
  if (!is.na(qn_month[1L, 1L])) {
    n     <- as.numeric(qn_month[1L, 2L])
    label <- if (n == 1L) "monthly" else paste0("q", n, "months")
    return(list(label = label, per_day = 1 / (n * 30)))
  }

  # Every N weeks — check before standalone "weekly"
  qn_week <- stringr::str_match(s, "(?:q|every)\\s*(\\d+)\\s*w(?:ee)?ks?")
  if (!is.na(qn_week[1L, 1L])) {
    n     <- as.numeric(qn_week[1L, 2L])
    label <- if (n == 1L) "weekly" else paste0("q", n, "weeks")
    return(list(label = label, per_day = 1 / (n * 7)))
  }

  # "q week" / "q wk" without a number → weekly
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

  # Step 1 — strip route abbreviations
  s <- stringr::str_remove_all(s, "\\bpo\\b|\\boral(ly)?\\b|\\biv\\b|\\bsubq\\b|\\bsc\\b")
  s <- stringr::str_squish(s)

  # Step 2 — strip leading alphabetic drug-name prefix (e.g. "prednisone 5 mg")
  s <- stringr::str_remove(s, "^(?:[a-z]+\\s+)+(?=\\d)")
  s <- stringr::str_squish(s)

  # Special case — "X grams over N days" (IVIG infusion course)
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

  # Step 3 — detect per-kg flag, then strip "/kg"
  dose_per_kg <- stringr::str_detect(s, "/\\s*kg\\b")
  s           <- stringr::str_remove_all(s, "/\\s*kg\\b")
  s           <- stringr::str_squish(s)

  # Step 4 — extract amount + unit
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

  # Step 5 — extract frequency
  freq           <- .extract_dmard_freq(s)
  dose_frequency <- freq$label
  freq_per_day   <- freq$per_day

  # Step 6 — compute daily equivalent
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

#' Parse a single DMARD dose string (tryCatch wrapper — never throws)
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
#'   \item{`dose_mg_per_admin`}{`dose_amount` converted to mg (g × 1000);
#'     `NA` when `dose_per_kg` is `TRUE`.}
#'   \item{`dose_daily_mg_equiv`}{`dose_mg_per_admin × freq_per_day`; `NA`
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
#' @param df A data frame — the raw gold-standard CSV loaded with
#'   `readr::read_csv()` or equivalent.
#' @param person_id_col `character(1)`. Patient identifier column.
#'   Default: `"myositis_omop_person_id"`.
#' @param drug_name_col `character(1)`. DMARD name column (free text).
#'   Default: `"dmardname"`.
#' @param dose_col `character(1)`. Free-text dose expression column.
#'   Default: `"dmarddose"`.
#' @param start_date_col `character(1)`. Estimated DMARD start date column.
#'   Default: `"dmard_start_date_est"`.
#' @param status_col `character(1)` or `NULL`. Column recording DMARD status
#'   (e.g. `"past"` vs active). Set `NULL` to skip status logic entirely
#'   (all episode end-dates will be set to `today`). If the column is declared
#'   but absent from `df`, a warning is issued and the argument is ignored.
#'   Default: `"dmardstatus"`.
#' @param stop_date_col `character(1)` or `NULL`. Estimated stop-date column,
#'   used only when `status_col` value equals `past_status_val`. Set `NULL`
#'   to skip. If declared but absent, a warning is issued.
#'   Default: `"pastdmard_stop_date_est"`.
#' @param past_status_val `character(1)`. The value in `status_col` that
#'   indicates a discontinued DMARD. Default: `"past"`.
#' @param today `Date`. Reference date used as `episode_end` for active
#'   (non-past) records. Default: `Sys.Date()`.
#'
#' @return A tibble with one row per input row:
#' \describe{
#'   \item{`person_id`}{From `person_id_col`.}
#'   \item{`drug_name_std`}{Lowercased and whitespace-trimmed `drug_name_col`.}
#'   \item{`episode_start`}{`start_date_col` coerced to `Date`.}
#'   \item{`episode_end`}{`stop_date_col` if status is past and stop date
#'     present; `today` otherwise; `NA` if status is past but stop date missing.}
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
                              person_id_col   = "myositis_omop_person_id",
                              drug_name_col   = "dmardname",
                              dose_col        = "dmarddose",
                              start_date_col  = "dmard_start_date_est",
                              status_col      = "dmardstatus",
                              stop_date_col   = "pastdmard_stop_date_est",
                              past_status_val = "past",
                              today           = Sys.Date()) {

  # --- required columns -------------------------------------------------------
  assert_required_cols(
    df,
    c(person_id_col, drug_name_col, dose_col, start_date_col),
    "df"
  )

  # --- optional columns — warn if declared but absent ------------------------
  if (!is.null(status_col) && !status_col %in% names(df)) {
    rlang::warn(paste0(
      "status_col '", status_col,
      "' not found in df; treating all rows as active (episode_end = today)."
    ))
    status_col <- NULL
  }
  if (!is.null(stop_date_col) && !stop_date_col %in% names(df)) {
    rlang::warn(paste0(
      "stop_date_col '", stop_date_col,
      "' not found in df; episode_end will be today for all rows."
    ))
    stop_date_col <- NULL
  }

  # --- build base working frame -----------------------------------------------
  base <- df |>
    dplyr::transmute(
      person_id     = .data[[person_id_col]],
      drug_name_std = stringr::str_squish(
                        stringr::str_to_lower(as.character(.data[[drug_name_col]]))
                      ),
      episode_start = safe_as_date(.data[[start_date_col]]),
      dose_raw      = as.character(.data[[dose_col]]),
      .status       = if (!is.null(status_col))
                        as.character(.data[[status_col]])
                      else NA_character_,
      .stop_raw     = if (!is.null(stop_date_col))
                        .data[[stop_date_col]]
                      else NA_character_
    )

  # --- derive episode_end from status logic -----------------------------------
  base <- base |>
    dplyr::mutate(
      episode_end = dplyr::case_when(
        !is.na(.data$.status) &
          .data$.status == past_status_val &
          !is.na(.data$.stop_raw) ~
            safe_as_date(.data$.stop_raw),
        !is.na(.data$.status) &
          .data$.status == past_status_val &
          is.na(.data$.stop_raw) ~
            as.Date(NA_character_),
        TRUE ~ as.Date(today)
      )
    )

  n_missing_stop <- sum(
    !is.na(base$.status) &
      base$.status == past_status_val &
      is.na(base$.stop_raw),
    na.rm = TRUE
  )
  if (n_missing_stop > 0L) {
    rlang::warn(paste0(
      n_missing_stop, " row(s) have status '", past_status_val,
      "' but no stop date — episode_end set to NA for those rows."
    ))
  }

  # --- parse dose column -------------------------------------------------------
  parsed <- parse_dmard_dose(base$dose_raw)

  # --- assemble output --------------------------------------------------------
  base |>
    dplyr::select("person_id", "drug_name_std", "episode_start", "episode_end",
                  "dose_raw") |>
    dplyr::bind_cols(parsed)
}
