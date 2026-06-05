# gold_standard.R
# Parse and validate DMARD gold-standard data from clinical review CSVs.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Replace common null-sentinel strings with NA in all character columns
#' @noRd
.null_to_na_cols <- function(df) {
  df |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~ {
        x <- stringr::str_trim(.x)
        dplyr::if_else(x %in% c("NULL", "null", "NA", "N/A", "n/a", ""),
                       NA_character_, x)
      }
    ))
}

#' Build a Date from separate year/month/day columns, with sensible defaults
#' when month or day is missing (mirrors gold_standard.qmd make_date_from_parts)
#' @noRd
.make_date_from_parts <- function(y, m, d,
                                   default_month = 6L,
                                   default_day   = 15L) {
  y <- suppressWarnings(as.integer(y))
  m <- suppressWarnings(as.integer(m))
  d <- suppressWarnings(as.integer(d))
  m[is.na(m) | m < 1L | m > 12L] <- default_month
  d[is.na(d) | d < 1L | d > 28L] <- default_day
  out <- dplyr::if_else(!is.na(y),
                         sprintf("%04d-%02d-%02d", y, m, d),
                         NA_character_)
  suppressWarnings(as.Date(out))
}

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
#' Accepts a raw clinical review CSV (one row per DMARD record per patient)
#' with messy free-text dose expressions. Aligns with the original
#' `gold_standard.qmd` analysis pipeline: null-sentinel cleaning, year/month/day
#' date fallbacks, negative-duration correction, non-daily regimen exclusion,
#' and plausibility capping. All column name arguments are parameterised.
#'
#' @param df A data frame -- the raw gold-standard CSV (pre-loaded).
#' @param person_id_col `character(1)`. Patient identifier column.
#'   Default: `"myositis_omop_person_id"`.
#' @param drug_name_col `character(1)`. DMARD name column. Default: `"dmardname"`.
#' @param dose_col `character(1)`. Free-text dose column. Default: `"dmarddose"`.
#' @param start_date_col `character(1)`. ISO start-date column.
#'   Default: `"dmard_start_date_est"`.
#' @param start_year_col,start_month_col,start_day_col `character(1)` or `NULL`.
#'   Year/month/day part columns used as fallback when `start_date_col` is NA.
#'   Defaults: `"dmardstartyear"`, `"dmardstartmonth"`, `"dmardstartday"`.
#'   Set any to `NULL` to disable the fallback.
#' @param status_col `character(1)` or `NULL`. DMARD status column.
#'   Default: `"dmardstatus"`.
#' @param last_changed_col `character(1)` or `NULL`. Last-updated timestamp
#'   column; primary source for `episode_end`. Default: `"last_changed_datetime"`.
#' @param stop_date_col `character(1)` or `NULL`. ISO stop-date column (past
#'   records). Default: `"pastdmard_stop_date_est"`.
#' @param stop_year_col,stop_month_col,stop_day_col `character(1)` or `NULL`.
#'   Year/month/day fallback for the stop date. Defaults: `"pastdmardstopyear"`,
#'   `"pastdmardstopmonth"`, `"pastdmardstopday"`.
#' @param end_date_col `character(1)` or `NULL`. Generic end-date fallback when
#'   `status_col` is absent. Default: `NULL`.
#' @param past_status_val `character(1)`. Value in `status_col` indicating
#'   a discontinued DMARD. Default: `"past"`.
#' @param drug_filter `character` vector or `NULL`. If supplied, only rows
#'   whose standardised drug name matches one of these values (case-insensitive)
#'   are retained. `NULL` keeps all drugs. Default: `NULL`.
#' @param exclude_non_daily `logical(1)`. If `TRUE` (default), doses flagged as
#'   non-daily regimens (weekly, monthly, infusion, weight-based, etc.) have
#'   `dose_daily_mg_equiv` set to `NA` and `parse_status` set to `"non_daily"`,
#'   matching the original analysis pipeline.
#' @param dose_min,dose_max `numeric(1)`. Plausibility bounds (exclusive).
#'   `dose_daily_mg_equiv` values outside `(dose_min, dose_max)` are set to
#'   `NA` with `parse_status = "implausible"`. Defaults: `0`, `300`.
#' @param today `Date`. Fallback reference date for active records.
#'   Default: `Sys.Date()`.
#'
#' @details
#' **End-date priority:**
#' \itemize{
#'   \item Status = past: `last_changed` -> `stop_date` (ISO then parts) -> `NA`
#'   \item Status = current/other: `last_changed` -> `today`
#'   \item No status: `last_changed` -> `end_date_col` -> `today`
#' }
#' Negative durations (`episode_end < episode_start`) are corrected by swapping
#' the two dates, mirroring the original pipeline's imputation step.
#'
#' @return A tibble with one row per retained input row and columns:
#' `person_id`, `drug_name_std`, `episode_start`, `episode_end`, `dose_raw`,
#' `dose_amount`, `dose_unit`, `dose_per_kg`, `dose_frequency`, `freq_per_day`,
#' `dose_mg_per_admin`, `dose_daily_mg_equiv`, `non_daily_regimen`,
#' `parse_status`.
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   myositis_omop_person_id = c(1L, 2L, 3L),
#'   dmardname               = c("Corticosteroids", "Corticosteroids", "Methotrexate"),
#'   dmarddose               = c("10 mg daily", "1000 mg infusion", "15 mg weekly"),
#'   dmard_start_date_est    = c("2020-01-01", "2019-06-01", "2021-03-01"),
#'   dmardstatus             = c("current", "past", "current"),
#'   pastdmard_stop_date_est = c(NA, "2022-12-31", NA),
#'   last_changed_datetime   = c("2024-01-01", NA, "2024-06-01")
#' )
#' parse_dmard_gold(df, drug_filter = "corticosteroids")
parse_dmard_gold <- function(df,
                              person_id_col    = "myositis_omop_person_id",
                              drug_name_col    = "dmardname",
                              dose_col         = "dmarddose",
                              start_date_col   = "dmard_start_date_est",
                              start_year_col   = "dmardstartyear",
                              start_month_col  = "dmardstartmonth",
                              start_day_col    = "dmardstartday",
                              status_col       = "dmardstatus",
                              last_changed_col = "last_changed_datetime",
                              stop_date_col    = "pastdmard_stop_date_est",
                              stop_year_col    = "pastdmardstopyear",
                              stop_month_col   = "pastdmardstopmonth",
                              stop_day_col     = "pastdmardstopday",
                              end_date_col     = NULL,
                              past_status_val  = "past",
                              drug_filter      = NULL,
                              exclude_non_daily = TRUE,
                              dose_min         = 0,
                              dose_max         = 300,
                              today            = Sys.Date()) {

  # --- 1. Null-sentinel cleaning ----------------------------------------------
  df <- .null_to_na_cols(df)

  # --- 2. Required columns ----------------------------------------------------
  assert_required_cols(
    df,
    c(person_id_col, drug_name_col, dose_col, start_date_col),
    "df"
  )

  # --- 3. Silence absent optional columns -------------------------------------
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
  start_year_col   <- .opt(start_year_col,   "start_year_col")
  start_month_col  <- .opt(start_month_col,  "start_month_col")
  start_day_col    <- .opt(start_day_col,    "start_day_col")
  stop_year_col    <- .opt(stop_year_col,    "stop_year_col")
  stop_month_col   <- .opt(stop_month_col,   "stop_month_col")
  stop_day_col     <- .opt(stop_day_col,     "stop_day_col")

  # --- 4. Build working frame -------------------------------------------------
  has_start_parts <- !is.null(start_year_col) && !is.null(start_month_col) &&
                       !is.null(start_day_col)
  has_stop_parts  <- !is.null(stop_year_col)  && !is.null(stop_month_col)  &&
                       !is.null(stop_day_col)

  base <- df |>
    dplyr::transmute(
      person_id     = .data[[person_id_col]],
      drug_name_std = stringr::str_squish(
                        stringr::str_to_lower(as.character(.data[[drug_name_col]]))
                      ),
      dose_raw      = as.character(.data[[dose_col]]),
      .status       = if (!is.null(status_col))
                        as.character(.data[[status_col]])
                      else NA_character_,
      .last_changed = if (!is.null(last_changed_col))
                        safe_as_date(.data[[last_changed_col]])
                      else as.Date(NA_character_),
      # start date: ISO primary, parts fallback
      .start_iso    = safe_as_date(.data[[start_date_col]]),
      .start_parts  = if (has_start_parts)
                        .make_date_from_parts(
                          .data[[start_year_col]],
                          .data[[start_month_col]],
                          .data[[start_day_col]]
                        )
                      else as.Date(NA_character_),
      # stop date: ISO primary, parts fallback
      .stop_iso     = if (!is.null(stop_date_col))
                        safe_as_date(.data[[stop_date_col]])
                      else as.Date(NA_character_),
      .stop_parts   = if (has_stop_parts)
                        .make_date_from_parts(
                          .data[[stop_year_col]],
                          .data[[stop_month_col]],
                          .data[[stop_day_col]]
                        )
                      else as.Date(NA_character_),
      .end_raw      = if (!is.null(end_date_col))
                        safe_as_date(.data[[end_date_col]])
                      else as.Date(NA_character_)
    ) |>
    dplyr::mutate(
      episode_start = dplyr::coalesce(.data$.start_iso, .data$.start_parts),
      .stop_raw     = dplyr::coalesce(.data$.stop_iso,  .data$.stop_parts)
    )

  # --- 5. Drug filter ---------------------------------------------------------
  if (!is.null(drug_filter)) {
    filter_std <- stringr::str_squish(stringr::str_to_lower(as.character(drug_filter)))
    base <- base |> dplyr::filter(.data$drug_name_std %in% filter_std)
    if (nrow(base) == 0L) {
      rlang::warn(paste0(
        "drug_filter matched no rows. Check that drug_filter values match ",
        "the lowercased drug_name_col entries."
      ))
      return(dplyr::bind_cols(
        base |> dplyr::select("person_id", "drug_name_std",
                              "episode_start", "dose_raw"),
        tibble::tibble(episode_end = as.Date(character(0))),
        .empty_dmard_parse_row()[0L, ]
      ))
    }
  }

  # --- 6. Derive episode_end --------------------------------------------------
  today_date <- as.Date(today)
  has_status <- !is.na(base$.status)
  is_past    <- has_status & base$.status == past_status_val

  base <- base |>
    dplyr::mutate(
      episode_end = dplyr::case_when(
        is_past & !is.na(.data$.last_changed)  ~ .data$.last_changed,
        is_past & !is.na(.data$.stop_raw)      ~ .data$.stop_raw,
        is_past                                 ~ as.Date(NA_character_),
        has_status & !is.na(.data$.last_changed) ~ .data$.last_changed,
        has_status                              ~ today_date,
        !has_status & !is.na(.data$.last_changed) ~ .data$.last_changed,
        !has_status & !is.na(.data$.end_raw)   ~ .data$.end_raw,
        TRUE                                   ~ today_date
      )
    )

  n_null <- sum(is_past & is.na(base$.last_changed) & is.na(base$.stop_raw),
                na.rm = TRUE)
  if (n_null > 0L) {
    rlang::warn(paste0(
      n_null, " past row(s) have neither last_changed nor stop_date -- ",
      "episode_end set to NA."
    ))
  }

  # --- 7. Negative-duration correction ----------------------------------------
  # Swap start/end when end < start (mirrors original imputation step)
  base <- base |>
    dplyr::mutate(
      .swap        = !is.na(.data$episode_start) &
                       !is.na(.data$episode_end) &
                       .data$episode_end < .data$episode_start,
      episode_start = dplyr::if_else(.data$.swap, .data$episode_end,   .data$episode_start),
      episode_end   = dplyr::if_else(.data$.swap, .data$episode_start, .data$episode_end)
    )

  n_swap <- sum(base$.swap, na.rm = TRUE)
  if (n_swap > 0L) {
    rlang::warn(paste0(
      n_swap, " row(s) had episode_end < episode_start; start/end dates swapped."
    ))
  }

  # --- 8. Parse dose ----------------------------------------------------------
  parsed <- parse_dmard_dose(base$dose_raw)

  # --- 9. Non-daily regimen exclusion -----------------------------------------
  # Patterns that indicate a non-daily regimen (mirrors original pipeline).
  # dose_daily_mg_equiv is set to NA; parse_status updated to "non_daily".
  non_daily_pat <- paste0(
    "g/kg|grams?/kg|\\binfusion\\b|\\bmonthly\\b|\\bweekly\\b|",
    "\\bevery\\b|\\bover\\b|\\bgram\\b"
  )
  non_daily_flag <- stringr::str_detect(
    stringr::str_to_lower(base$dose_raw),
    non_daily_pat
  ) & !is.na(base$dose_raw)

  # Also flag weight-based and infusion-course records from the parser
  non_daily_flag <- non_daily_flag |
    (parsed$parse_status %in% c("weight_required") & !is.na(parsed$parse_status)) |
    (!is.na(parsed$dose_frequency) &
       parsed$dose_frequency %in% c("weekly", "q2weeks", "q4weeks",
                                     "monthly", "q2months", "q3months",
                                     "q6months", "infusion_course"))

  parsed <- parsed |>
    dplyr::mutate(
      non_daily_regimen = non_daily_flag,
      parse_status = dplyr::if_else(
        non_daily_flag & .data$parse_status == "ok",
        "non_daily", .data$parse_status
      ),
      dose_daily_mg_equiv = dplyr::if_else(
        non_daily_flag & isTRUE(exclude_non_daily),
        NA_real_, .data$dose_daily_mg_equiv
      )
    )

  # --- 10. Plausibility cap ---------------------------------------------------
  # 0 < dose_daily_mg_equiv < dose_max (mirrors original pred_equiv_flagged logic)
  implausible <- !is.na(parsed$dose_daily_mg_equiv) &
    (parsed$dose_daily_mg_equiv <= dose_min |
       parsed$dose_daily_mg_equiv >= dose_max)

  parsed <- parsed |>
    dplyr::mutate(
      parse_status = dplyr::if_else(
        implausible, "implausible", .data$parse_status
      ),
      dose_daily_mg_equiv = dplyr::if_else(
        implausible, NA_real_, .data$dose_daily_mg_equiv
      )
    )

  # --- 11. Assemble output ----------------------------------------------------
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

  # --- select only needed columns to avoid name conflicts --------------------
  # Mirror eval.R: join by patient ID only, then filter by date overlap.
  # Drug name is carried from the gold record for stratification, not used
  # as a join key (computed episodes may use different name formats).
  comp <- computed_df |>
    dplyr::select(
      person_id = dplyr::all_of(computed_id_col),
      c_start   = dplyr::all_of(computed_start_col),
      c_end     = dplyr::all_of(computed_end_col),
      c_dose    = dplyr::all_of(computed_dose_col)
    ) |>
    dplyr::mutate(
      c_start = safe_as_date(.data$c_start),
      c_end   = safe_as_date(.data$c_end),
      c_dose  = safe_as_numeric(.data$c_dose)
    )

  gold <- gold_df |>
    dplyr::select(
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

  # --- overlap join by patient ID only (same approach as eval.R) -------------
  # For each gold record, find ALL computed episodes that overlap the gold
  # window. No drug-name constraint: the gold drug label is an attribute of
  # the gold record, not a filter on computed episodes.
  merged <- gold |>
    dplyr::left_join(
      comp,
      by           = "person_id",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      ovlp_start = pmax(.data$g_start, .data$c_start, na.rm = FALSE),
      ovlp_end   = pmin(.data$g_end,   .data$c_end,   na.rm = FALSE),
      ovlp_days  = as.integer(.data$ovlp_end - .data$ovlp_start) + 1L,
      ovlp_days  = dplyr::if_else(.data$ovlp_days < 1L, 0L, .data$ovlp_days)
    ) |>
    dplyr::filter(.data$ovlp_days >= min_overlap_days)

  # --- duration-weighted mean dose per gold record ----------------------------
  # Refinement over eval.R's slice(1L): instead of taking the single best-
  # overlapping episode, weight ALL overlapping computed episodes by their
  # overlap duration within the gold window.
  weighted <- merged |>
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
