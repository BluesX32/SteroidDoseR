# conversion.R
# Prednisone-equivalent dose conversion.

# ---------------------------------------------------------------------------
# Internal equivalency table
# ---------------------------------------------------------------------------

# Standard clinical potency ratios relative to prednisone (anti-inflammatory).
# Sources: Buttgereit et al. (2002); ACR / UpToDate glucocorticoid comparison.
# budesonide: oral (not inhaled) roughly 9x; flagged NA because inhaled is
#   excluded upstream and oral budesonide dosing is context-dependent.
.pred_equiv_table <- tibble::tribble(
  ~drug_name_std,       ~equiv_factor,
  "prednisone",          1.00,
  "prednisolone",        1.00,
  "methylprednisolone",  1.25,   # 5 mg pred ~ 4 mg methylpred  => factor 5/4
  "dexamethasone",       7.50,   # 5 mg pred ~ 0.667 mg dexa    => factor 7.5
  "hydrocortisone",      0.25,   # 5 mg pred ~ 20 mg hydrocort  => factor 0.25
  "triamcinolone",       1.25,   # similar potency to methylpred
  "budesonide",          NA_real_ # flagged; route-dependent; requires manual review
)

# ---------------------------------------------------------------------------
# Exported function
# ---------------------------------------------------------------------------

#' Convert raw daily doses to prednisone-equivalent mg/day
#'
#' Joins a built-in equivalency table (or a user-supplied one) to multiply
#' each drug's raw daily dose by its potency factor relative to prednisone.
#'
#' @param drug_df A data frame containing at least a drug-name column and a
#'   numeric daily-dose column.
#' @param drug_col `character(1)`. Name of the column holding drug names.
#'   Values are standardised internally (lowercase, trimmed, synonym-mapped).
#'   Default: `"drug_name_std"`. The function also accepts `"dmard_name"` or
#'   `"drug_concept_name"` -- whichever is present if `drug_col` is not found,
#'   a fallback search is attempted.
#' @param dose_col `character(1)`. Name of the numeric daily-dose column.
#'   Default: `"daily_dose_mg"`.
#' @param out_col `character(1)`. Name of the output column for
#'   prednisone-equivalent dose. Default: `"pred_equiv_mg"`.
#' @param equiv_table A data frame with columns `drug_name_std` and
#'   `equiv_factor`. If `NULL` (default), the built-in clinical table is used.
#' @param drug_name_map Optional data frame with columns `pattern` and
#'   `canonical_name` passed to [standardize_drug_name()]. Use to add
#'   site-specific brand names or non-English synonyms. Default: `NULL`.
#'
#' @return `drug_df` with three additional columns:
#'   - **`<out_col>`** (`numeric`): prednisone-equivalent daily dose (mg/day).
#'     `NA` when the equivalency factor is unknown or missing.
#'   - **`equiv_factor`** (`numeric`): the multiplicative factor applied.
#'   - **`pred_equiv_status`** (`character`): one of `"ok"`,
#'     `"missing_factor"` (drug known but factor is `NA`, e.g. budesonide), or
#'     `"unknown_drug"` (drug not in the equivalency table).
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   drug_name_std = c("prednisone", "methylprednisolone", "dexamethasone"),
#'   daily_dose_mg = c(10, 8, 4)
#' )
#' convert_pred_equiv(df)
convert_pred_equiv <- function(drug_df,
                               drug_col      = "drug_name_std",
                               dose_col      = "daily_dose_mg",
                               out_col       = "pred_equiv_mg",
                               equiv_table   = NULL,
                               drug_name_map = NULL) {

  # --- resolve drug column with fallback ---
  if (!drug_col %in% names(drug_df)) {
    fallbacks <- c("dmard_name", "drug_concept_name", "ingredient_concept_name")
    found <- intersect(fallbacks, names(drug_df))
    if (length(found) == 0L) {
      rlang::abort(
        paste0(
          "Column '", drug_col, "' not found in drug_df. ",
          "Tried fallbacks: ", paste(fallbacks, collapse = ", "), ". ",
          "Available columns: ", paste(names(drug_df), collapse = ", ")
        )
      )
    }
    drug_col <- found[[1L]]
    rlang::inform(paste0("convert_pred_equiv: using column '", drug_col, "' for drug names."))
  }
  assert_required_cols(drug_df, dose_col, "drug_df")

  etable <- if (is.null(equiv_table)) .pred_equiv_table else equiv_table
  assert_required_cols(etable, c("drug_name_std", "equiv_factor"), "equiv_table")

  # --- standardise drug names ---
  drug_df <- drug_df |>
    dplyr::mutate(.drug_std_tmp = standardize_drug_name(.data[[drug_col]],
                                                         drug_name_map = drug_name_map))

  # --- join ---
  result <- drug_df |>
    dplyr::left_join(etable, by = c(".drug_std_tmp" = "drug_name_std")) |>
    dplyr::mutate(
      !!out_col := safe_as_numeric(.data[[dose_col]]) * .data$equiv_factor,
      pred_equiv_status = dplyr::case_when(
        is.na(.data$equiv_factor) & .data$.drug_std_tmp %in% etable$drug_name_std ~ "missing_factor",
        is.na(.data$equiv_factor) ~ "unknown_drug",
        TRUE ~ "ok"
      )
    ) |>
    dplyr::select(-".drug_std_tmp")

  result
}
