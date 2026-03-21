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
#' @param max_daily_dose_mg `numeric(1)` or `NULL`. Upper plausibility bound in
#'   mg/day. Records whose imputed dose exceeds this value are set to `NA` with
#'   a warning, because doses that large almost always indicate data-quality
#'   problems (non-mg `amount_value`, quantity in total-mg, near-zero
#'   `days_supply`, etc.). Default: `2000`. Set to `NULL` to disable the cap.
#' @param filter_oral `logical(1)`. If `TRUE`, restrict to oral-route records
#'   and to drugs present in the prednisone-equivalency table before imputing,
#'   matching the default behaviour of [calc_daily_dose_nlp()]. Default:
#'   `FALSE` (no filtering) to preserve backward compatibility.
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
#' @return The input data frame (or the fetched data frame) with seven
#' additional columns:
#' \describe{
#'   \item{strength_mg}{Extracted tablet/capsule strength in mg.}
#'   \item{dose_from_original}{M1 value: the original `daily_dose` or
#'     `daily_dose_mg` column cast to numeric (NA if absent or non-positive).}
#'   \item{dose_from_tablets_freq}{M2 value: `tablets × freq_per_day ×
#'     strength_mg` (NA if any component is missing).}
#'   \item{dose_from_supply}{M3 value: `(quantity × strength_mg) / days_supply`
#'     (NA if any component is missing or `days_supply` is 0).}
#'   \item{dose_from_actual_duration}{M4 value: `(quantity × strength_mg) /
#'     actual_duration_days` (NA if any component is missing or duration ≤ 0).}
#'   \item{daily_dose_mg_imputed}{Best-estimate daily dose in mg: the first
#'     non-NA value across M1–M4 in cascade order.}
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
                                     drug_concept_ids  = NULL,
                                     person_ids        = NULL,
                                     start_date        = NULL,
                                     end_date          = NULL,
                                     sig_source        = "sig",
                                     m2_sig_parse      = c("warn", "auto", "none"),
                                     max_daily_dose_mg = 2000,
                                     filter_oral       = FALSE,
                                     equiv_table       = NULL,
                                     drug_name_map     = NULL) {

  m2_sig_parse <- match.arg(m2_sig_parse)

  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date, sig_source)

  assert_required_cols(drug_df, "drug_exposure_start_date", "drug_df")

  # --- 0. optional oral-route / drug-class filter ---------------------------
  # Mirrors calc_daily_dose_nlp(filter_oral = TRUE): keeps only oral-route
  # records and drugs present in the prednisone-equivalency table.
  # Default FALSE preserves backward-compatible behaviour (no filtering).
  if (filter_oral) {
    # Standardise drug names if not already present so the steroid list can
    # be applied.  Prefer drug_concept_name, fall back to drug_source_value.
    if (!"drug_name_std" %in% names(drug_df)) {
      name_col <- intersect(c("drug_concept_name", "drug_source_value"), names(drug_df))
      if (length(name_col) > 0L) {
        drug_df <- drug_df |>
          dplyr::mutate(
            drug_name_std = standardize_drug_name(
              .data[[name_col[[1L]]]],
              drug_name_map = drug_name_map
            )
          )
      }
    }

    rc <- if ("route_concept_name" %in% names(drug_df))
      drug_df$route_concept_name else NULL
    rs <- if ("route_source_value" %in% names(drug_df))
      drug_df$route_source_value else NULL

    if (is.null(rc) && is.null(rs)) {
      rlang::warn("No route column found; skipping oral-route filter.")
    } else {
      route_class <- classify_route(rc, rs)
      drug_df <- drug_df[route_class == "oral" | is.na(route_class), ]
    }

    if ("drug_name_std" %in% names(drug_df)) {
      .etbl <- if (is.null(equiv_table)) .pred_equiv_table else equiv_table
      known_steroids <- .etbl$drug_name_std[!is.na(.etbl$drug_name_std)]
      drug_df <- drug_df[drug_df$drug_name_std %in% known_steroids, ]
    }

    if (nrow(drug_df) == 0L) {
      rlang::warn("No oral corticosteroid records found after filtering.")
      return(drug_df)
    }
  }

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
  # Prefer amount_value when its unit is mg (OMOP concept_id 8576).
  # Non-mg units (mcg = 9655, g = 8504, etc.) would be misinterpreted as mg
  # and produce dose explosions in M3/M4; discard them and fall through to the
  # string-extraction fallback.  When amount_unit_concept_id is absent or NA
  # we cannot verify the unit, so we accept the value but also cross-check it
  # against the string extraction and warn when they differ by > 100×.
  av      <- if ("amount_value" %in% names(drug_df)) safe_as_numeric(drug_df$amount_value) else rep(NA_real_, nrow(drug_df))
  av_unit <- if ("amount_unit_concept_id" %in% names(drug_df)) drug_df$amount_unit_concept_id else rep(NA_integer_, nrow(drug_df))
  # Accept when unit is mg (8576), unknown/absent (NA), or unmapped concept 0.
  # Many production CDMs store 0 rather than NULL when the unit concept is not
  # mapped — treating 0 identically to NA avoids silently discarding all
  # amount_value rows at sites that use the OMOP "no matching concept" sentinel.
  av_mg   <- dplyr::if_else(
    is.na(av_unit) | as.integer(av_unit) == 0L | as.integer(av_unit) == 8576L,
    av, NA_real_
  )

  sv <- if ("drug_source_value" %in% names(drug_df)) drug_df$drug_source_value else rep(NA_character_, nrow(drug_df))

  str_from_source <- stringr::str_extract(
    stringr::str_to_lower(as.character(sv)),
    "(\\d+(?:\\.\\d+)?)\\s*(?:mg|MG)"
  ) |>
    stringr::str_extract("\\d+(?:\\.\\d+)?") |>
    safe_as_numeric()

  drug_df <- drug_df |>
    dplyr::mutate(strength_mg = dplyr::coalesce(av_mg, str_from_source))

  if (all(is.na(drug_df$strength_mg))) {
    n_av_na   <- sum(is.na(av))
    n_unit_ok <- sum(!is.na(av_unit) & as.integer(av_unit) %in% c(0L, 8576L),
                     na.rm = TRUE)
    rlang::warn(sprintf(
      paste0(
        "strength_mg is NA for all %d records — no dose can be imputed.\n",
        "  amount_value: %d NA, %d non-NA (unit accepted as mg/unknown)\n",
        "  drug_source_value string fallback: %d non-NA mg values found\n",
        "  Check: is amount_unit_concept_id mostly a non-mg concept (not 8576/0/NA)?\n",
        "  Check: does drug_source_value contain 'X mg' patterns?"
      ),
      nrow(drug_df),
      n_av_na, sum(!is.na(av_mg)),
      sum(!is.na(str_from_source))
    ))
  }

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

  # --- 6. plausibility cap ---------------------------------------------------
  # Doses above max_daily_dose_mg almost certainly reflect data-quality issues:
  # unit mismatches in amount_value, quantity coded in total-mg rather than
  # tablet count, or days_supply / actual_duration of 0 slipping through.
  # Records that exceed the cap are set to NA (imputation_method = "missing")
  # rather than silently inflating episode-level summaries.
  if (!is.null(max_daily_dose_mg) && is.finite(max_daily_dose_mg)) {
    implausible <- !is.na(drug_df$daily_dose_mg_imputed) &
                   drug_df$daily_dose_mg_imputed > max_daily_dose_mg
    if (any(implausible, na.rm = TRUE)) {
      rlang::warn(sprintf(
        paste0(
          "%d record(s) had daily_dose_mg_imputed > %.0f mg/day and were set to NA.\n",
          "  Likely causes: non-mg amount_value units, quantity in total-mg, or bad days_supply.\n",
          "  Adjust max_daily_dose_mg if higher doses are expected."
        ),
        sum(implausible, na.rm = TRUE), max_daily_dose_mg
      ))
      drug_df$daily_dose_mg_imputed[implausible] <- NA_real_
      drug_df$imputation_method[implausible]     <- "missing"
    }
  }

  # --- 7. rename intermediates to Version2-compatible public names ----------
  drug_df <- drug_df |>
    dplyr::rename(
      dose_from_original        = .m1,
      dose_from_tablets_freq    = .m2,
      dose_from_supply          = .m3,
      dose_from_actual_duration = .m4
    )

  # Drop only the arithmetic scratch columns (not the renamed dose columns)
  drug_df |>
    dplyr::select(-dplyr::any_of(c(".start", ".end", ".actual_dur",
                                    ".quantity", ".days_supply",
                                    ".tablets", ".freq", ".dd")))
}
