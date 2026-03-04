# nlp.R
# Rule-based NLP SIG parser and daily dose calculator.
#
# Logic ported from DosageCalculation/NLP_Parser/NLP_parser.Rmd (lines
# 318-503), with improved frequency dictionary, taper handling, and
# never-error guarantee.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.norm_sig <- function(x) stringr::str_squish(stringr::str_to_lower(as.character(x)))

# ---------------------------------------------------------------------------
# Single-record SIG parser
# ---------------------------------------------------------------------------

#' Parse one SIG (prescription instruction) string into dose components
#'
#' Applies a hierarchy of regex rules to extract tablets per dose, frequency
#' per day, mg amounts, duration, and flags. The function **never throws an
#' error** — malformed or empty inputs return a row of `NA`s with
#' `parsed_status = "empty"` or `"no_parse"`.
#'
#' @param sig_text `character(1)`. The raw SIG string to parse.
#'
#' @return A one-row `tibble` with columns:
#' \describe{
#'   \item{sig_raw}{Original input string.}
#'   \item{tablets}{Number of tablets per dose (numeric).}
#'   \item{freq_per_day}{Doses per day as a decimal (e.g. 0.5, 1, 2, 3, 4).}
#'   \item{mg_per_admin}{Milligrams per single administration.}
#'   \item{mg_total_flag}{`TRUE` if an explicit daily total was given.}
#'   \item{duration_days}{Prescribed duration converted to days.}
#'   \item{taper_flag}{`TRUE` if taper language detected.}
#'   \item{prn_flag}{`TRUE` if "as needed" / PRN language detected.}
#'   \item{free_text_flag}{`TRUE` if SIG is unstructured ("as directed", etc.)}
#'   \item{daily_dose_mg}{Computed daily dose in mg.}
#'   \item{parsed_status}{One of `"ok"`, `"taper"`, `"prn"`, `"free_text"`,
#'     `"no_parse"`, or `"empty"`.}
#' }
#'
#' @export
#'
#' @examples
#' parse_sig_one("Take 4 tablets (20 mg per dose) by mouth daily.")
#' parse_sig_one("Take 2 tabs BID for 14 days")
#' parse_sig_one("Taper by 1 mg every 4 weeks")
#' parse_sig_one("as directed")
#' parse_sig_one(NA_character_)
parse_sig_one <- function(sig_text) {
  tryCatch(
    .parse_sig_one_impl(sig_text),
    error = function(e) {
      .empty_parse_row(sig_text, "error")
    }
  )
}

.empty_parse_row <- function(sig_text, status = "empty") {
  tibble::tibble(
    sig_raw        = as.character(sig_text),
    tablets        = NA_real_,
    freq_per_day   = NA_real_,
    mg_per_admin   = NA_real_,
    mg_total_flag  = FALSE,
    duration_days  = NA_real_,
    taper_flag     = FALSE,
    prn_flag       = FALSE,
    free_text_flag = FALSE,
    daily_dose_mg  = NA_real_,
    parsed_status  = status
  )
}

.parse_sig_one_impl <- function(sig_text) {
  if (is.null(sig_text) || (length(sig_text) == 1L && is.na(sig_text)) ||
      nchar(trimws(as.character(sig_text))) == 0L) {
    return(.empty_parse_row(sig_text, "empty"))
  }

  s <- .norm_sig(sig_text)

  # ---- Flags ----------------------------------------------------------------
  prn_flag       <- stringr::str_detect(s, "\\bprn\\b|as needed|when needed|if needed")
  free_text_flag <- stringr::str_detect(s,
    "as directed|use as directed|see attach|per md|per ng|per physician|per doctor|per provider")
  taper_flag     <- stringr::str_detect(s,
    "taper|decreas|reducing|reduce by|drop by|\\bthen\\b.*\\bmg\\b|alternate day|\\bqod\\b|every other day")

  # ---- Tablets per dose -----------------------------------------------------
  tablets <- stringr::str_match(s,
    "(\\d+(?:\\.\\d+)?)\\s*(?:tablets?|tabs?|pills?|capsules?|caps?)")[, 2L] |>
    safe_as_numeric()

  # ---- Frequency per day ----------------------------------------------------
  freq <- dplyr::case_when(
    stringr::str_detect(s, "every other day|\\bqod\\b") ~ 0.5,
    stringr::str_detect(s, "four\\s*(?:times|x)\\s*(?:a\\s*)?(?:daily|day)|\\bqid\\b|\\bq6h\\b|every\\s*6\\s*hours?") ~ 4,
    stringr::str_detect(s, "three\\s*(?:times|x)\\s*(?:a\\s*)?(?:daily|day)|\\btid\\b|\\bq8h\\b|every\\s*8\\s*hours?") ~ 3,
    stringr::str_detect(s, "twice\\s*(?:daily|a\\s*day)|two\\s*(?:times|x)\\s*(?:a\\s*)?(?:daily|day)|\\bbid\\b|\\bq12h\\b|every\\s*12\\s*hours?") ~ 2,
    stringr::str_detect(s, "once\\s*(?:daily|a\\s*day)|\\bqd\\b|\\bdaily\\b|every\\s*day|every\\s*morning|with\\s*breakfast|q\\s*am|qam|every\\s*24\\s*hours?") ~ 1,
    TRUE ~ NA_real_
  )

  # ---- Duration -------------------------------------------------------------
  dur_match <- stringr::str_match(
    s, "(?:for|x)\\s*(\\d+(?:\\.\\d+)?)\\s*(day|days|wk|wks|week|weeks|mo|mos|month|months)"
  )
  dur_num  <- safe_as_numeric(dur_match[, 2L])
  dur_unit <- dur_match[, 3L]

  duration_days <- dplyr::case_when(
    is.na(dur_num) ~ NA_real_,
    stringr::str_detect(dur_unit, "^day") ~ dur_num,
    stringr::str_detect(dur_unit, "^wk|^week") ~ dur_num * 7,
    stringr::str_detect(dur_unit, "^mo") ~ dur_num * 30,
    TRUE ~ NA_real_
  )

  # ---- mg extraction (priority order) --------------------------------------
  # 1. Explicit daily total in parens: "(X mg total)"
  mg_total <- stringr::str_match(s,
    "\\((\\d+(?:\\.\\d+)?)\\s*mg\\s*total\\)")[, 2L] |> safe_as_numeric()

  # 2. Explicit per-dose amount in parens: "(X mg per dose)"
  mg_per_dose <- stringr::str_match(s,
    "\\((\\d+(?:\\.\\d+)?)\\s*mg\\s*per\\s*dose\\)")[, 2L] |> safe_as_numeric()

  # 3. Plain mg in parens: "(X mg)" — only when neither of the above
  mg_paren_plain <- if (is.na(mg_per_dose) && is.na(mg_total)) {
    stringr::str_match(s, "\\((\\d+(?:\\.\\d+)?)\\s*mg\\)")[, 2L] |> safe_as_numeric()
  } else NA_real_

  # 4. Bare mg not in parens: "X mg" — only when none of the above
  mg_bare <- if (is.na(mg_per_dose) && is.na(mg_total) && is.na(mg_paren_plain)) {
    stringr::str_match(s, "(?<!\\()\\b(\\d+(?:\\.\\d+)?)\\s*mg\\b")[, 2L] |> safe_as_numeric()
  } else NA_real_

  # ---- per-administration mg ------------------------------------------------
  # "(X mg per dose)" is the total per-administration amount — do NOT multiply
  # by tablet count (tablets just tell you how many pills, not a dose multiplier).
  # Plain parenthetical "(X mg)" and bare "X mg" are treated as per-tablet
  # strengths, so they ARE multiplied by tablet count when tablets is known.
  mg_per_admin <- dplyr::case_when(
    !is.na(mg_total) ~ NA_real_,       # daily total provided; use directly below
    !is.na(mg_per_dose) ~ mg_per_dose, # explicit per-dose total; tablets already accounted for
    !is.na(mg_paren_plain) & !is.na(tablets) ~ mg_paren_plain * tablets,
    !is.na(mg_paren_plain) ~ mg_paren_plain,
    !is.na(mg_bare) & !is.na(tablets) ~ mg_bare * tablets,
    !is.na(mg_bare) ~ mg_bare,
    TRUE ~ NA_real_
  )

  # ---- daily dose mg --------------------------------------------------------
  daily_mg <- dplyr::case_when(
    !is.na(mg_total) ~ mg_total,
    !is.na(mg_per_admin) & !is.na(freq) ~ mg_per_admin * freq,
    !is.na(mg_per_admin) & is.na(freq) &
      stringr::str_detect(s, "daily|every day|once daily|\\bqd\\b|with breakfast|every morning|qam|every 24 hours?") ~ mg_per_admin,
    TRUE ~ NA_real_
  )

  # ---- parsed_status --------------------------------------------------------
  status <- dplyr::case_when(
    free_text_flag ~ "free_text",
    taper_flag     ~ "taper",
    prn_flag       ~ "prn",
    !is.na(daily_mg) ~ "ok",
    TRUE ~ "no_parse"
  )

  tibble::tibble(
    sig_raw        = sig_text,
    tablets        = tablets,
    freq_per_day   = freq,
    mg_per_admin   = mg_per_admin,
    mg_total_flag  = !is.na(mg_total),
    duration_days  = duration_days,
    taper_flag     = taper_flag,
    prn_flag       = prn_flag,
    free_text_flag = free_text_flag,
    daily_dose_mg  = daily_mg,
    parsed_status  = status
  )
}

# ---------------------------------------------------------------------------
# Vectorised SIG parser
# ---------------------------------------------------------------------------

#' Apply `parse_sig_one()` to every row of a data frame
#'
#' @param drug_df A data frame containing a SIG text column.
#' @param sig_col `character(1)`. Name of the SIG column. Default: `"sig"`.
#'
#' @return `drug_df` with the parsed columns from [parse_sig_one()] appended
#'   (excluding the `sig_raw` duplicate).
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id = 1:2,
#'   sig = c("Take 1 tab (5 mg) daily", "Take 2 tabs BID")
#' )
#' parse_sig(df, sig_col = "sig")
parse_sig <- function(drug_df, sig_col = "sig") {
  assert_required_cols(drug_df, sig_col, "drug_df")

  parsed <- purrr::map_dfr(drug_df[[sig_col]], parse_sig_one) |>
    dplyr::select(-"sig_raw")  # avoid duplicating the original column

  dplyr::bind_cols(drug_df, parsed)
}

# ---------------------------------------------------------------------------
# Full NLP pipeline
# ---------------------------------------------------------------------------

#' Compute daily steroid doses using rule-based SIG parsing (NLP method)
#'
#' Filters an OMOP drug-exposure data frame to oral systemic corticosteroids,
#' standardises drug names, parses each SIG string, and returns a data frame
#' with a computed `daily_dose_mg` column.
#'
#' @param drug_df A data frame. Must contain `sig_col`, date columns, and
#'   either `route_concept_name` or `route_source_value` (for oral filtering).
#' @param drug_name_col `character(1)`. Column with the drug name.
#'   Default: `"drug_concept_name"`.
#' @param sig_col `character(1)`. Column with the SIG text. Default: `"sig"`.
#' @param filter_oral `logical(1)`. If `TRUE` (default), only oral-route
#'   records are kept. Set to `FALSE` if route filtering was done upstream.
#' @param baseline_fallback `logical(1)`. If `TRUE`, the function tries to
#'   carry through an existing `daily_dose_mg` column (e.g. from the Baseline
#'   method) for records where NLP parsing fails. Default: `FALSE`.
#'
#' @return A data frame with the same rows as `drug_df` (after optional oral
#'   filter) plus columns from [parse_sig_one()], `drug_name_std`, and the
#'   final `daily_dose_mg`.
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id              = 1L,
#'   drug_concept_name      = "prednisone 5 MG oral tablet",
#'   route_concept_name     = "oral",
#'   sig                    = "Take 2 tablets (10 mg total) daily",
#'   drug_exposure_start_date = as.Date("2023-01-01"),
#'   drug_exposure_end_date   = as.Date("2023-03-01")
#' )
#' calc_daily_dose_nlp(df)
calc_daily_dose_nlp <- function(drug_df,
                                drug_name_col    = "drug_concept_name",
                                sig_col          = "sig",
                                filter_oral      = TRUE,
                                baseline_fallback = FALSE) {

  assert_required_cols(drug_df, c(drug_name_col, sig_col), "drug_df")

  # --- standardise drug names -----------------------------------------------
  drug_df <- drug_df |>
    dplyr::mutate(drug_name_std = standardize_drug_name(.data[[drug_name_col]]))

  # --- filter to oral corticosteroids ----------------------------------------
  if (filter_oral) {
    rc <- if ("route_concept_name" %in% names(drug_df)) drug_df$route_concept_name else NULL
    rs <- if ("route_source_value" %in% names(drug_df)) drug_df$route_source_value else NULL

    if (is.null(rc) && is.null(rs)) {
      rlang::warn("No route column found; skipping oral-route filter.")
    } else {
      route_class <- classify_route(rc, rs)
      drug_df <- drug_df[route_class == "oral" | is.na(route_class), ]
    }

    # Keep only recognised oral systemic steroids
    known_steroids <- .pred_equiv_table$drug_name_std[!is.na(.pred_equiv_table$drug_name_std)]
    drug_df <- drug_df[drug_df$drug_name_std %in% known_steroids, ]
  }

  if (nrow(drug_df) == 0L) {
    rlang::warn("No oral corticosteroid records found after filtering.")
    return(drug_df)
  }

  # --- parse SIG strings --------------------------------------------------------
  result <- parse_sig(drug_df, sig_col = sig_col)

  # --- optional baseline fallback -----------------------------------------------
  if (baseline_fallback && "daily_dose_mg_orig" %in% names(drug_df)) {
    result <- result |>
      dplyr::mutate(
        daily_dose_mg = dplyr::coalesce(.data$daily_dose_mg,
                                        safe_as_numeric(.data$daily_dose_mg_orig))
      )
  }

  result
}
