# baseline.R
# Cascading baseline dose imputation — faithful replica of the logic in
# DosageCalculation/Baseline/Baseline.qmd (lines 23-86).

#' Compute daily steroid doses using structured OMOP fields (Baseline method)
#'
#' Applies a four-step cascading imputation strategy to estimate the daily
#' dose in mg for each drug-exposure record. The cascade order and
#' tie-breaking rules match the `Baseline.qmd` analysis used in the
#' Phase 1 paper.
#'
#' The first argument accepts either a **connector** (created by
#' [create_omop_connector()] or [create_df_connector()]) or a plain
#' **data frame** for backward compatibility.
#'
#' ## Cascade order
#' | Method | Label | Logic |
#' |--------|-------|-------|
#' | M1 | `"original"` | Use `daily_dose` or `daily_dose_mg` column if numeric and > 0. |
#' | M2 | `"tablets_freq"` | `tablets × freq_per_day × strength_mg`. Requires `sig` column to be parsed first, or pre-parsed `tablets` and `freq_per_day` columns. |
#' | M3 | `"supply_based"` | `(quantity × strength_mg) / days_supply`. |
#' | M4 | `"actual_duration"` | `(quantity × strength_mg) / actual_duration_days`, where `actual_duration_days` is derived from the date columns. |
#'
#' @param connector_or_df A `steroid_connector` (from [create_omop_connector()]
#'   or [create_df_connector()]) **or** a data frame. When a connector is
#'   supplied the function fetches data from the source; when a data frame is
#'   supplied the legacy in-memory path is used unchanged.
#' @param methods `character` vector. Ordered list of methods to attempt.
#'   Defaults to `c("original", "tablets_freq", "supply_based", "actual_duration")`.
#'   Each method is only tried if the required columns are present; missing
#'   columns cause a graceful skip (not an error).
#' @param drug_concept_ids Integer vector of OMOP `drug_concept_id` values to
#'   restrict the extraction. Ignored when `connector_or_df` is a data frame.
#' @param person_ids Vector of `person_id` values to restrict the extraction.
#'   Ignored when `connector_or_df` is a data frame.
#' @param start_date Character or Date. Lower bound on
#'   `drug_exposure_start_date`. Ignored when `connector_or_df` is a data frame.
#' @param end_date Character or Date. Upper bound on
#'   `drug_exposure_start_date`. Ignored when `connector_or_df` is a data frame.
#' @param sig_source `character(1)`. Which column to use as SIG text when the
#'   native `sig` field is absent. One of `"sig"` (default) or
#'   `"drug_source_value"`. Ignored when `connector_or_df` is a data frame.
#' @param m2_sig_parse `character(1)`. Controls behaviour when `tablets` and
#'   `freq_per_day` columns are absent but a `sig` column is present:
#'   \describe{
#'     \item{`"warn"` (default)}{Emit a warning and skip M2.}
#'     \item{`"auto"`}{Call [parse_sig()] internally to populate `tablets` and
#'       `freq_per_day` before running M2.}
#'     \item{`"none"`}{Silently skip M2 without any message.}
#'   }
#'   Ignored when `tablets` and `freq_per_day` are already present, or when
#'   `"tablets_freq"` is not in `methods`.
#'
#' @return The input data frame (or the fetched data frame) with three
#' additional columns:
#' \describe{
#'   \item{strength_mg}{Extracted tablet/capsule strength in mg.}
#'   \item{daily_dose_mg_imputed}{Best-estimate daily dose in mg.}
#'   \item{imputation_method}{One of `"original"`, `"tablets_freq"`,
#'     `"supply_based"`, `"actual_duration"`, or `"missing"`.}
#' }
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id                = 1L,
#'   drug_source_value        = "PREDNISONE 5 MG TABLET",
#'   amount_value             = 5,
#'   quantity                 = 28,
#'   days_supply              = 28,
#'   drug_exposure_start_date = as.Date("2023-01-01"),
#'   drug_exposure_end_date   = as.Date("2023-01-28"),
#'   daily_dose               = NA_real_
#' )
#' calc_daily_dose_baseline(df)
#'
#' # Connector path (synthetic example via df_connector):
#' con <- create_df_connector(df)
#' calc_daily_dose_baseline(con)
calc_daily_dose_baseline <- function(connector_or_df,
                                     methods       = c("original", "tablets_freq",
                                                       "supply_based", "actual_duration"),
                                     drug_concept_ids = NULL,
                                     person_ids       = NULL,
                                     start_date       = NULL,
                                     end_date         = NULL,
                                     sig_source       = "sig",
                                     m2_sig_parse     = c("warn", "auto", "none")) {

  m2_sig_parse <- match.arg(m2_sig_parse)

  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date, sig_source)

  assert_required_cols(drug_df, "drug_exposure_start_date", "drug_df")

  # --- 1. actual duration (always compute; needed for M4) -------------------
  # drug_exposure_end_date is optional. When absent, substitute today so that
  # M4 can still produce a finite (though rough) duration estimate.
  if (!"drug_exposure_end_date" %in% names(drug_df)) {
    rlang::warn(paste0(
      "`drug_exposure_end_date` is absent; substituting today's date (",
      Sys.Date(), ") for M4 duration calculation."
    ))
    drug_df[["drug_exposure_end_date"]] <- Sys.Date()
  }

  drug_df <- drug_df |>
    dplyr::mutate(
      .start = safe_as_date(.data$drug_exposure_start_date),
      .end   = safe_as_date(.data$drug_exposure_end_date),
      .actual_dur = as.numeric(.data$.end - .data$.start) + 1L,
      .actual_dur = dplyr::if_else(.data$.actual_dur > 0, .data$.actual_dur, NA_real_)
    )

  # --- 2. strength_mg -------------------------------------------------------
  # Prefer amount_value; fallback to extracting from drug_source_value string.
  av <- if ("amount_value" %in% names(drug_df)) safe_as_numeric(drug_df$amount_value) else rep(NA_real_, nrow(drug_df))
  sv <- if ("drug_source_value" %in% names(drug_df)) drug_df$drug_source_value else rep(NA_character_, nrow(drug_df))

  str_from_source <- stringr::str_extract(
    stringr::str_to_lower(as.character(sv)),
    "(\\d+(?:\\.\\d+)?)\\s*(?:mg|MG)"
  ) |>
    stringr::str_extract("\\d+(?:\\.\\d+)?") |>
    safe_as_numeric()

  drug_df <- drug_df |>
    dplyr::mutate(strength_mg = dplyr::coalesce(av, str_from_source))

  # --- 3. numeric coercions (check column existence OUTSIDE mutate) ----------
  has_qty  <- "quantity"     %in% names(drug_df)
  has_sup  <- "days_supply"  %in% names(drug_df)
  has_tab  <- "tablets"      %in% names(drug_df)
  has_freq <- "freq_per_day" %in% names(drug_df)

  # --- M2 SIG-parse guard -------------------------------------------------------
  # When tablets/freq_per_day are absent but a sig column is present, apply
  # the strategy chosen by m2_sig_parse before the imputation block runs.
  if ("tablets_freq" %in% methods && !has_tab && !has_freq) {
    sig_present <- "sig" %in% names(drug_df) &&
      any(!is.na(drug_df[["sig"]]) & nzchar(trimws(drug_df[["sig"]])))

    if (sig_present) {
      if (m2_sig_parse == "auto") {
        message("M2: `tablets`/`freq_per_day` absent — parsing `sig` column automatically.")
        drug_df  <- parse_sig(drug_df, sig_col = "sig")
        has_tab  <- "tablets"      %in% names(drug_df)
        has_freq <- "freq_per_day" %in% names(drug_df)
      } else if (m2_sig_parse == "warn") {
        rlang::warn(paste0(
          "M2 (tablets_freq) skipped: `tablets` and `freq_per_day` are absent ",
          "but a `sig` column is present.\n",
          "  Use m2_sig_parse = 'auto' to parse it automatically, or\n",
          "  use m2_sig_parse = 'nlp_first' in run_pipeline() to run NLP before baseline."
        ))
      }
      # "none" → silent skip; no action needed
    }
  }
  # Accept both daily_dose (package convention) and daily_dose_mg (Version2).
  dd_col <- intersect(c("daily_dose", "daily_dose_mg"), names(drug_df))
  dd_col <- if (length(dd_col) > 0L) dd_col[[1L]] else NA_character_
  has_dd <- !is.na(dd_col)

  drug_df <- drug_df |>
    dplyr::mutate(
      .quantity    = if (has_qty)  safe_as_numeric(.data$quantity)      else NA_real_,
      .days_supply = if (has_sup)  safe_as_numeric(.data$days_supply)   else NA_real_,
      .tablets     = if (has_tab)  safe_as_numeric(.data$tablets)       else NA_real_,
      .freq        = if (has_freq) safe_as_numeric(.data$freq_per_day)  else NA_real_,
      # Evaluate outside mutate to avoid reference to absent column.
      .dd          = if (has_dd)   safe_as_numeric(.data[[dd_col]])     else NA_real_
    )

  # --- 4. candidate estimates -----------------------------------------------
  drug_df <- drug_df |>
    dplyr::mutate(
      # M1: original
      .m1 = dplyr::case_when(
        "original" %in% methods & !is.na(.data$.dd) & .data$.dd > 0 ~ .data$.dd,
        TRUE ~ NA_real_
      ),

      # M2: tablets × freq × strength
      .m2 = dplyr::case_when(
        "tablets_freq" %in% methods &
          !is.na(.data$.tablets) & !is.na(.data$.freq) & !is.na(.data$strength_mg) ~
          .data$.tablets * .data$.freq * .data$strength_mg,
        TRUE ~ NA_real_
      ),

      # M3: (quantity × strength) / days_supply
      .m3 = dplyr::case_when(
        "supply_based" %in% methods &
          !is.na(.data$.quantity) & !is.na(.data$.days_supply) &
          .data$.days_supply > 0 & !is.na(.data$strength_mg) ~
          (.data$.quantity * .data$strength_mg) / .data$.days_supply,
        TRUE ~ NA_real_
      ),

      # M4: (quantity × strength) / actual_duration
      .m4 = dplyr::case_when(
        "actual_duration" %in% methods &
          !is.na(.data$.quantity) & !is.na(.data$.actual_dur) &
          .data$.actual_dur > 0 & !is.na(.data$strength_mg) ~
          (.data$.quantity * .data$strength_mg) / .data$.actual_dur,
        TRUE ~ NA_real_
      )
    )

  # --- 5. cascade: first non-NA following the user-supplied methods order ----
  method_map    <- c(
    "original"        = ".m1",
    "tablets_freq"    = ".m2",
    "supply_based"    = ".m3",
    "actual_duration" = ".m4"
  )
  valid_methods <- methods[methods %in% names(method_map)]
  ordered_cols  <- method_map[valid_methods]

  # daily_dose_mg_imputed: coalesce in the requested order
  dose_vals <- lapply(ordered_cols, function(col) drug_df[[col]])
  drug_df$daily_dose_mg_imputed <- if (length(dose_vals) > 0L)
    do.call(dplyr::coalesce, dose_vals)
  else
    rep(NA_real_, nrow(drug_df))

  # imputation_method: assign in reverse order so the first method wins
  drug_df$imputation_method <- "missing"
  for (m in rev(valid_methods)) {
    col  <- method_map[[m]]
    mask <- !is.na(drug_df[[col]])
    drug_df$imputation_method[mask] <- m
  }

  # --- 6. remove internal scratch columns -----------------------------------
  drug_df |>
    dplyr::select(-dplyr::any_of(c(".start", ".end", ".actual_dur",
                                    ".quantity", ".days_supply",
                                    ".tablets", ".freq", ".dd",
                                    ".m1", ".m2", ".m3", ".m4")))
}
