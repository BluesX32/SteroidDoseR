# utils-validate.R
# Internal validation helpers shared across all SteroidDoseR modules.
# None of these are exported; they are prefixed with a dot or kept lowercase.

# ---------------------------------------------------------------------------
# 1. Column presence check
# ---------------------------------------------------------------------------

#' Assert required columns exist in a data frame
#'
#' Raises an informative error if any expected column names are absent.
#'
#' @param df A data frame.
#' @param cols Character vector of required column names.
#' @param df_name Character(1). Label used in the error message (default
#'   `"drug_df"`).
#' @return `df` invisibly on success.
#' @noRd
assert_required_cols <- function(df, cols, df_name = "drug_df") {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        df_name, " is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        ".\nAvailable columns: ", paste(names(df), collapse = ", ")
      )
    )
  }
  invisible(df)
}

# ---------------------------------------------------------------------------
# 2. Safe date coercion
# ---------------------------------------------------------------------------

#' Coerce a column to Date safely
#'
#' Accepts Date objects or character strings in common formats
#' (`"YYYY-MM-DD"`, `"MM/DD/YYYY"`, `"YYYYMMDD"`). Returns NA for values
#' that cannot be parsed, with a warning.
#'
#' @param x A Date or character vector.
#' @return A Date vector of the same length.
#' @noRd
safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  result <- suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c("Ymd", "mdY", "Y-m-d", "m/d/Y", "Y/m/d"),
      quiet  = TRUE
    )
  )
  out <- as.Date(result)
  n_fail <- sum(is.na(out) & !is.na(x))
  if (n_fail > 0L) {
    rlang::warn(
      paste0(n_fail, " date value(s) could not be parsed and were set to NA.")
    )
  }
  out
}

# ---------------------------------------------------------------------------
# 3. Drug name standardisation
# ---------------------------------------------------------------------------

#' Standardise a corticosteroid drug name to a canonical form
#'
#' Lowercases, trims whitespace, then maps the most common synonyms and
#' brand names to a short canonical string. Returns the input lowercased
#' and trimmed for any name that does not match a known steroid.
#'
#' @param x Character vector of drug names.
#' @return Character vector with standardised names.
#' @noRd
standardize_drug_name <- function(x) {
  s <- stringr::str_squish(stringr::str_to_lower(as.character(x)))

  dplyr::case_when(
    stringr::str_detect(s, "\\bprednisolone\\b") &
      !stringr::str_detect(s, "methyl") ~ "prednisolone",
    stringr::str_detect(s, "methylpred|methyl prednisolone|medrol|solu-medrol|solumedrol") ~ "methylprednisolone",
    stringr::str_detect(s, "\\bprednisone\\b|rayos|sterapred") ~ "prednisone",
    stringr::str_detect(s, "dexamethasone|decadron|dexasone") ~ "dexamethasone",
    stringr::str_detect(s, "hydrocortisone|cortef|solu-cortef") ~ "hydrocortisone",
    stringr::str_detect(s, "triamcinolone|kenalog|aristospan") ~ "triamcinolone",
    stringr::str_detect(s, "budesonide|entocort|uceris|pulmicort") ~ "budesonide",
    TRUE ~ s
  )
}

# ---------------------------------------------------------------------------
# 4. Route classification
# ---------------------------------------------------------------------------

#' Classify a drug route as oral, inhaled, topical, injection, or other
#'
#' Used by the NLP pipeline to keep only oral systemic steroids.
#'
#' @param route_concept Character vector. OMOP `route_concept_name` field.
#' @param route_source  Character vector. `route_source_value` field
#'   (free text). Used as fallback when `route_concept` is NA.
#' @return Character vector: one of `"oral"`, `"inhaled"`, `"topical"`,
#'   `"injection"`, `"ophthalmic"`, or `"other"`.
#' @noRd
classify_route <- function(route_concept = NULL, route_source = NULL) {
  # Build a combined string; prefer concept name, fall back to source value
  n <- max(
    if (!is.null(route_concept)) length(route_concept) else 0L,
    if (!is.null(route_source))  length(route_source)  else 0L
  )

  rc <- if (!is.null(route_concept)) stringr::str_to_lower(as.character(route_concept)) else rep(NA_character_, n)
  rs <- if (!is.null(route_source))  stringr::str_to_lower(as.character(route_source))  else rep(NA_character_, n)

  combined <- dplyr::coalesce(rc, rs)
  combined[is.na(combined)] <- ""

  dplyr::case_when(
    stringr::str_detect(combined, "inhal|metered|aerosol|dry powder|actuat|nebul|pulmicort") ~ "inhaled",
    stringr::str_detect(combined, "ophthalm|eye|ocular|otic|ear") ~ "ophthalmic",
    stringr::str_detect(combined, "topical|cream|ointment|lotion|gel|patch|transdermal|rectal|nasal") ~ "topical",
    stringr::str_detect(combined, "inject|intramus|\\bim\\b|intravein|\\biv\\b|subcutane|\\bsc\\b|intraartic|intradermal|epidural") ~ "injection",
    stringr::str_detect(combined, "oral|mouth|sublingual|buccal|swallow|tablet|capsule|solution|liquid") ~ "oral",
    TRUE ~ "other"
  )
}

# ---------------------------------------------------------------------------
# 5. Numeric coercion helper
# ---------------------------------------------------------------------------

#' Suppress-and-coerce a vector to numeric, returning NA for failures
#'
#' @param x A vector.
#' @return Numeric vector of the same length.
#' @noRd
safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}
