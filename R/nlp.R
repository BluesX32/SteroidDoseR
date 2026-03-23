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

# Pre-processing applied to a normalised SIG string before pattern matching.
# 1. Translates common Spanish number words and tablet terms to English.
# 2. Strips parenthetical word-only clarifications such as "(twelve)" or
#    "(oral)" that would otherwise block frequency regex matches, while
#    preserving parenthetical dose info like "(10 mg per dose)" or "(1 mg)".
.preprocess_sig <- function(s) {
  # Spanish number words → digits; tablet/frequency synonyms → English
  s <- stringr::str_replace_all(s, c(
    # English number words (for tab-count SIGs like "one a day")
    "\\bone\\b"       = "1",
    "\\btwo\\b"       = "2",
    "\\bthree\\b"     = "3",
    "\\bfour\\b"      = "4",
    "\\bfive\\b"      = "5",
    # Spanish number words
    "\\buno\\b"       = "1",
    "\\bdos\\b"       = "2",
    "\\btres\\b"      = "3",
    "\\bcuatro\\b"    = "4",
    "\\bcinco\\b"     = "5",
    "\\bseis\\b"      = "6",
    "\\bsiete\\b"     = "7",
    "\\bdiez\\b"      = "10",
    "\\btabletas?\\b" = "tablet",
    "\\bdiario\\b"    = "daily",
    "\\bpor\\s+via\\b" = "by route"
  ))
  # Strip pure-word parentheticals: "(twelve)", "(oral)" etc.
  # Negative lookahead preserves anything containing a digit or "mg"/"mcg"
  s <- stringr::str_remove_all(s, "\\s*\\((?![^)]*(?:mg|mcg|\\d))[^)]+\\)")
  stringr::str_squish(s)
}

# ---------------------------------------------------------------------------
# Single-record SIG parser
# ---------------------------------------------------------------------------

#' Parse one SIG (prescription instruction) string into dose components
#'
#' Applies a hierarchy of regex rules to extract tablets per dose, frequency
#' per day, mg amounts, duration, and flags. The function **never throws an
#' error** -- malformed or empty inputs return a row of `NA`s with
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
  s <- .preprocess_sig(s)

  # ---- Flags ----------------------------------------------------------------
  prn_flag       <- stringr::str_detect(s, "\\bprn\\b|as needed|when needed|if needed")
  free_text_flag <- stringr::str_detect(s,
    "as directed|use as directed|see attach|per md|per ng|per physician|per doctor|per provider")
  taper_flag     <- stringr::str_detect(s,
    "taper|decreas|reducing|reduce by|\\bdrop\\b|\\bthen\\b.*\\bmg\\b|\\bthen\\b.*\\btabs?\\b|alternate day|\\bqod\\b|every other day")

  # ---- Tablets per dose -----------------------------------------------------
  tablets <- stringr::str_match(s,
    "(\\d+(?:\\.\\d+)?)\\s*(?:tablets?|tabs?|pills?|capsules?|caps?)")[, 2L] |>
    safe_as_numeric()

  # ---- Frequency per day ----------------------------------------------------
  freq <- dplyr::case_when(
    stringr::str_detect(s, "every other day|\\bqod\\b") ~ 0.5,
    stringr::str_detect(s, "(?:four|4)\\s*(?:times|x)\\s*(?:a\\s*)?(?:daily|day)|\\bqid\\b|\\bq6h\\b|every\\s*6\\s*hours?") ~ 4,
    stringr::str_detect(s, "(?:three|3)\\s*(?:times|x)\\s*(?:a\\s*)?(?:daily|day)|\\btid\\b|\\bq8h\\b|every\\s*8\\s*hours?") ~ 3,
    stringr::str_detect(s, "twice\\s*(?:daily|a\\s*day)|(?:two|2)\\s*(?:times|x)\\s*(?:a\\s*)?(?:daily|day)|\\bbid\\b|\\bq12h\\b|every\\s*12\\s*hours?") ~ 2,
    stringr::str_detect(s, "once\\s*(?:daily|a\\s*day)|\\bqd\\b|\\bdaily\\b|every\\s*day|every\\s*morning|with\\s*breakfast|q\\s*am|qam|every\\s*24\\s*hours?") ~ 1,
    stringr::str_detect(s, "\\bonce\\b.*\\boral\\b|\\bnightly\\b|every\\s*evening") ~ 1,
    # "in am" / "in the morning" / "every morning"
    stringr::str_detect(s,
      "\\bin\\s+(?:the\\s+)?(?:am\\b|morning)|every\\s+(?:am\\b|morning)|each\\s+morning") ~ 1,
    # "once for X dose(s)" — single-administration instruction
    stringr::str_detect(s, "\\bonce\\b.*\\bfor\\s+\\d+\\s+doses?") ~ 1,
    # Bare "X mg." with nothing else — treat as once-daily dose
    stringr::str_detect(s, "^\\d+(?:\\.\\d+)?\\s*mg\\.?$") ~ 1,
    # "by mouth" / "po" / "orally" without any time qualifier — implicit QD
    stringr::str_detect(s, "(?:by\\s+mouth|\\bpo\\b|\\borally\\b)") &
      !stringr::str_detect(s, "\\bhours?\\b|\\bhrs?\\b|\\bbefore\\b|\\bafter\\b|procedure|surgery") ~ 1,
    # "a day" / "per day" — common shorthand for once-daily
    stringr::str_detect(s, "\\ba\\s+day\\b|\\bper\\s+day\\b|\\bper\\s+d\\b") ~ 1,
    TRUE ~ NA_real_
  )

  # Default tablets to 1 when SIG contains no explicit tablet count.
  # "Once Oral", "Nightly", "Every evening" prescriptions typically mean 1 tablet.
  tablets <- dplyr::if_else(is.na(tablets), 1, tablets)

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

  # 3. Plain mg in parens: "(X mg)" -- only when neither of the above
  mg_paren_plain <- if (is.na(mg_per_dose) && is.na(mg_total)) {
    stringr::str_match(s, "\\((\\d+(?:\\.\\d+)?)\\s*mg\\)")[, 2L] |> safe_as_numeric()
  } else NA_real_

  # 4. Bare mg not in parens: "X mg" -- only when none of the above
  mg_bare <- if (is.na(mg_per_dose) && is.na(mg_total) && is.na(mg_paren_plain)) {
    stringr::str_match(s, "(?<!\\()\\b(\\d+(?:\\.\\d+)?)\\s*mg\\b")[, 2L] |> safe_as_numeric()
  } else NA_real_

  # ---- per-administration mg ------------------------------------------------
  # "(X mg per dose)" is the total per-administration amount -- do NOT multiply
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
#' The first argument accepts either a **connector** (created by
#' [create_omop_connector()] or [create_df_connector()]) or a plain
#' **data frame** for backward compatibility.
#'
#' @param connector_or_df A `steroid_connector` or a data frame. See
#'   [calc_daily_dose_baseline()] for details on the connector path.
#' @param drug_name_col `character(1)`. Column with the drug name.
#'   Default: `"drug_concept_name"`.
#' @param sig_col `character(1)`. Column with the SIG text. Default: `"sig"`.
#'   When `sig` is absent and `sig_source = "drug_source_value"`, the
#'   `drug_source_value` column is aliased to `sig` automatically.
#' @param filter_oral `logical(1)`. If `TRUE` (default), only oral-route
#'   records are kept. Set to `FALSE` if route filtering was done upstream.
#' @param baseline_fallback `logical(1)`. If `TRUE`, the function tries to
#'   carry through an existing `daily_dose_mg` column (e.g. from the Baseline
#'   method) for records where NLP parsing fails. Default: `FALSE`.
#' @param drug_concept_ids,person_ids,start_date,end_date,sig_source
#'   Connector-path filtering arguments. Ignored when `connector_or_df` is a
#'   data frame. See [calc_daily_dose_baseline()] for full descriptions.
#'
#' @return A data frame with the same rows as the input (after optional oral
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
calc_daily_dose_nlp <- function(connector_or_df,
                                drug_name_col     = "drug_concept_name",
                                sig_col           = "sig",
                                filter_oral       = TRUE,
                                baseline_fallback  = FALSE,
                                max_daily_dose_mg = 2000,
                                equiv_table       = NULL,
                                drug_name_map     = NULL,
                                drug_concept_ids  = NULL,
                                person_ids        = NULL,
                                start_date        = NULL,
                                end_date          = NULL,
                                sig_source        = "sig") {

  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date, sig_source)

  assert_required_cols(drug_df, drug_name_col, "drug_df")

  # If sig column is absent (site didn't populate it), insert NA column so
  # parse_sig() can still run and return parsed_status = "empty" for all rows.
  if (!sig_col %in% names(drug_df)) {
    drug_df[[sig_col]] <- NA_character_
  }

  # --- standardise drug names -----------------------------------------------
  drug_df <- drug_df |>
    dplyr::mutate(drug_name_std = standardize_drug_name(.data[[drug_name_col]],
                                                         drug_name_map = drug_name_map))

  # --- filter to oral corticosteroids ----------------------------------------
  if (filter_oral) {
    rc <- if ("route_concept_name" %in% names(drug_df)) drug_df$route_concept_name else NULL
    rs <- if ("route_source_value" %in% names(drug_df)) drug_df$route_source_value else NULL
    ds <- if ("drug_source_value"  %in% names(drug_df)) drug_df$drug_source_value  else NULL

    if (is.null(rc) && is.null(rs) && is.null(ds)) {
      rlang::warn("No route column found; skipping oral-route filter.")
    } else {
      route_class <- classify_route(rc, rs, ds)
      drug_df <- drug_df[route_class == "oral" | is.na(route_class), ]
    }

    # Keep only recognised oral systemic steroids (built-in or user-supplied table)
    .etbl          <- if (is.null(equiv_table)) .pred_equiv_table else equiv_table
    known_steroids <- .etbl$drug_name_std[!is.na(.etbl$drug_name_std)]
    drug_df <- drug_df[drug_df$drug_name_std %in% known_steroids, ]
  }

  if (nrow(drug_df) == 0L) {
    rlang::warn("No oral corticosteroid records found after filtering.")
    return(drug_df)
  }

  # --- parse SIG strings --------------------------------------------------------
  result <- parse_sig(drug_df, sig_col = sig_col)

  # --- strength fallback: use amount_value / drug concept name when the SIG
  #     string has no mg (e.g. "take 1 tablet daily" without a dose amount).
  #     This is the common case in live OMOP data where the drug strength is
  #     stored in drug_strength.amount_value and drug_concept_name, not in sig.
  has_no_mg <- result$parsed_status == "no_parse" & is.na(result$mg_per_admin)
  if (any(has_no_mg, na.rm = TRUE)) {
    av <- if ("amount_value" %in% names(result))
      safe_as_numeric(result$amount_value)
    else
      rep(NA_real_, nrow(result))

    name_col <- intersect(c("drug_concept_name", "drug_source_value"), names(result))
    sn <- if (length(name_col) > 0L) {
      stringr::str_match(
        stringr::str_to_lower(as.character(result[[name_col[[1L]]]])),
        "(\\d+(?:\\.\\d+)?)\\s*mg"
      )[, 2L] |> safe_as_numeric()
    } else {
      rep(NA_real_, nrow(result))
    }

    strength_fb <- dplyr::coalesce(av, sn)

    result <- result |>
      dplyr::mutate(
        needs_mg_fb   = has_no_mg & !is.na(strength_fb),
        mg_per_admin  = dplyr::if_else(
          .data$needs_mg_fb,
          strength_fb * dplyr::coalesce(.data$tablets, 1),
          .data$mg_per_admin
        ),
        daily_dose_mg = dplyr::if_else(
          .data$needs_mg_fb & !is.na(.data$freq_per_day),
          .data$mg_per_admin * .data$freq_per_day,
          .data$daily_dose_mg
        ),
        parsed_status = dplyr::if_else(
          .data$needs_mg_fb & !is.na(.data$daily_dose_mg),
          "ok",
          .data$parsed_status
        )
      ) |>
      dplyr::select(-"needs_mg_fb")
  }

  # --- optional baseline fallback (legacy: use pre-existing column) -------------
  if (baseline_fallback && "daily_dose_mg_orig" %in% names(drug_df)) {
    result <- result |>
      dplyr::mutate(
        daily_dose_mg = dplyr::coalesce(.data$daily_dose_mg,
                                        safe_as_numeric(.data$daily_dose_mg_orig))
      )
  }

  # --- structural fallback: baseline M1/M3/M4 for records still NA -----------
  # Guarantees NLP coverage >= baseline: anything computable from structured
  # OMOP fields (original daily_dose, Burkard formula, quantity/days_supply)
  # is carried through even when SIG parsing fails entirely.
  still_na <- is.na(result$daily_dose_mg) &
              result$parsed_status %in% c("no_parse", "empty")
  if (any(still_na, na.rm = TRUE)) {
    bl <- calc_daily_dose_baseline(
      result[still_na, ],
      filter_oral       = FALSE,   # already filtered above
      m2_sig_parse      = "none",  # M2 = SIG-based; already attempted
      max_daily_dose_mg = max_daily_dose_mg,
      equiv_table       = equiv_table,
      drug_name_map     = drug_name_map,
      methods           = c("original", "actual_duration", "supply_based")
    )
    result$daily_dose_mg[still_na] <- bl$daily_dose_mg_imputed
    # Label with which baseline method was used; keep "no_parse" for missing
    fb_label <- paste0("fallback_", bl$imputation_method)
    fb_label[bl$imputation_method == "missing"] <- "no_parse"
    result$parsed_status[still_na] <- fb_label
  }

  # --- dose plausibility cap ---------------------------------------------------
  if (!is.null(max_daily_dose_mg) && is.finite(max_daily_dose_mg)) {
    implausible <- !is.na(result$daily_dose_mg) &
                   result$daily_dose_mg > max_daily_dose_mg
    if (any(implausible, na.rm = TRUE)) {
      rlang::warn(sprintf(
        paste0(
          "%d record(s) have daily_dose_mg > %.0f mg/day and were set to NA.\n",
          "Possible causes: SIG with total-course dose, unit mismatch, or ",
          "data-entry error.\nPass max_daily_dose_mg = NULL to disable this cap."
        ),
        sum(implausible), max_daily_dose_mg
      ))
      result$daily_dose_mg[implausible] <- NA_real_
      result$parsed_status[implausible] <- "implausible"
    }
  }

  result
}
