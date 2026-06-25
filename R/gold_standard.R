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

#' @noRd
parse_dmard_dose <- function(x) {
  purrr::map_dfr(as.character(x), .parse_dmard_dose_one)
}

# ---------------------------------------------------------------------------
# Exported: parse_steroid_gold()
# ---------------------------------------------------------------------------

#' Parse and validate a corticosteroid gold-standard data frame
#'
#' Accepts a raw clinical review CSV (one row per corticosteroid record per
#' patient) with messy free-text dose expressions. Applies null-sentinel
#' cleaning, year/month/day date fallbacks, negative-duration correction,
#' non-daily regimen exclusion, and plausibility capping. All column name
#' arguments are parameterised.
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
#' @param drug_filter `character` vector or `NULL`. Only rows whose standardised
#'   drug name matches one of these values (case-insensitive) are retained.
#'   Default: `"corticosteroids"`. Set to `NULL` to keep all drugs.
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
#' parse_steroid_gold(df)
parse_steroid_gold <- function(df,
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
                                drug_filter      = "corticosteroids",
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
