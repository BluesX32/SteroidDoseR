# conversion.R
# Prednisone-equivalent dose conversion.

# ---------------------------------------------------------------------------
# Internal equivalency table
# ---------------------------------------------------------------------------

# Standard clinical potency ratios relative to prednisone (anti-inflammatory).
#
# Citation sources per drug:
#   prednisone / prednisolone: Reference drug; ratio 1:1 because prednisone is
#     hepatically converted to prednisolone (the active form). These are
#     interchangeable in oral dose calculations.
#     Source: Liu D et al. "A practical guide to the monitoring and management
#     of the complications of systemic corticosteroid therapy." Allergy Asthma
#     Clin Immunol. 2013;9(1):30. doi:10.1186/1710-1492-9-30
#
#   methylprednisolone: 4 mg methylprednisolone ≡ 5 mg prednisone → ratio 5/4=1.25
#     Source: Buttgereit F et al. "Standardised nomenclature for
#     glucocorticoid dosages and glucocorticoid treatment regimens."
#     Ann Rheum Dis. 2002;61(8):718-722. doi:10.1136/ard.61.8.718
#
#   dexamethasone: 0.75 mg ≡ 5 mg prednisone → ratio 5/0.667≈7.5
#     Source: Same Buttgereit et al. 2002; UpToDate "Pharmacologic use of
#     glucocorticoids" (Accessed 2024).
#     CONSERVATIVE estimate: 6.67 (5 mg pred / 0.75 mg dexa)
#     AGGRESSIVE estimate:   8.00 (based on 0.625 mg dexa ≡ 5 mg pred in
#       some pharmacology texts — Spoorenberg et al. 2014)
#
#   hydrocortisone: 20 mg ≡ 5 mg prednisone → ratio 5/20=0.25
#     Source: Buttgereit et al. 2002.
#
#   triamcinolone: 4 mg ≡ 5 mg prednisone → ratio 5/4=1.25 (same as methylpred)
#     Source: Same Buttgereit et al. 2002.
#
#   budesonide: oral (not inhaled); local potency ~9-15x, systemic ~9x.
#     Flagged NA because inhaled budesonide is excluded upstream; oral
#     budesonide equivalence is highly route- and formulation-dependent.
#     Source: Löfberg R et al. "Oral budesonide in Crohn's disease."
#     Inflamm Bowel Dis. 2006;12(5):345-348.
.pred_equiv_table <- tibble::tribble(
  ~drug_name_std,       ~equiv_factor,
  "prednisone",          1.00,
  "prednisolone",        1.00,
  "methylprednisolone",  1.25,
  "dexamethasone",       7.50,
  "hydrocortisone",      0.25,
  "triamcinolone",       1.25,
  "budesonide",          NA_real_
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

# ---------------------------------------------------------------------------
# Exported equivalency table variants
# ---------------------------------------------------------------------------

#' Conservative prednisone equivalency factors
#'
#' Use when minimising over-attribution of steroid exposure is the priority
#' (e.g., safety analyses where false-positive high-dose exposure is harmful).
#' Uses the lowest published equivalency ratios within the plausible clinical
#' range. The default table ([convert_pred_equiv()] with `equiv_table = NULL`)
#' uses the consensus mid-point values from Buttgereit et al. (2002).
#'
#' @format A tibble with 7 rows and 2 columns:
#' \describe{
#'   \item{drug_name_std}{Standardised drug name.}
#'   \item{equiv_factor}{Multiplication factor relative to prednisone 1 mg.}
#' }
#'
#' @references Buttgereit F et al. (2002) Ann Rheum Dis 61:718-722.
#'   Liu D et al. (2013) Allergy Asthma Clin Immunol 9:30.
#'
#' @export
pred_equiv_table_conservative <- tibble::tribble(
  ~drug_name_std,       ~equiv_factor,
  "prednisone",          1.00,
  "prednisolone",        1.00,
  "methylprednisolone",  1.25,    # consensus; no lower-bound variant
  "dexamethasone",       6.67,    # 0.75 mg dexa ≡ 5 mg pred (Buttgereit 2002)
  "hydrocortisone",      0.20,    # 25 mg HC ≡ 5 mg pred (lower-potency estimate)
  "triamcinolone",       1.00,    # 5 mg tria ≡ 5 mg pred (lower-potency estimate)
  "budesonide",          NA_real_
)

#' Aggressive prednisone equivalency factors
#'
#' Use when the goal is to capture the maximum plausible systemic steroid load
#' (e.g., damage-index studies where under-counting exposure is the concern).
#' Uses the highest published equivalency ratios within the plausible clinical
#' range.
#'
#' @format A tibble with 7 rows and 2 columns:
#' \describe{
#'   \item{drug_name_std}{Standardised drug name.}
#'   \item{equiv_factor}{Multiplication factor relative to prednisone 1 mg.}
#' }
#'
#' @references Spoorenberg SMC et al. (2014) Int J Infect Dis 28:18-23.
#'   UpToDate: Pharmacologic use of glucocorticoids (2024).
#'
#' @export
pred_equiv_table_aggressive <- tibble::tribble(
  ~drug_name_std,       ~equiv_factor,
  "prednisone",          1.00,
  "prednisolone",        1.00,
  "methylprednisolone",  1.50,    # some texts cite 0.8 mg pred per mg methylpred => 1/0.8=1.25 to 1.5
  "dexamethasone",       8.00,    # 0.625 mg dexa ≡ 5 mg pred (Spoorenberg 2014)
  "hydrocortisone",      0.25,    # consensus (no higher estimate)
  "triamcinolone",       1.25,    # Buttgereit 2002 upper estimate
  "budesonide",          NA_real_
)
