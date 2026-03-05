# pipeline.R
# Convenience wrapper that chains fetch → impute → convert → episode.

#' Document the standardized drug_df column contract
#'
#' All SteroidDoseR algorithms consume a data frame that follows this column
#' contract. The internal data-fetching layer guarantees these column names
#' when extracting from an OMOP CDM database. When supplying your own data
#' frame (legacy path), ensure the relevant columns are present.
#'
#' ## Required columns
#' | Column | Type | Notes |
#' |--------|------|-------|
#' | `person_id` | int/chr | Patient identifier |
#' | `drug_exposure_start_date` | Date | Exposure start |
#'
#' ## Recommended columns (used by one or more methods)
#' | Column | Type | Used by |
#' |--------|------|---------|
#' | `drug_exposure_id` | int | Traceability |
#' | `drug_concept_id` | int | Concept-set filtering |
#' | `drug_source_concept_id` | int | Unmapped-concept fallback |
#' | `drug_concept_name` | chr | Drug name standardisation |
#' | `drug_source_value` | chr | Strength extraction (Baseline M3/M4) |
#' | `drug_exposure_end_date` | Date | Duration (Baseline M4, episodes) |
#' | `quantity` | num | Baseline M3/M4 |
#' | `days_supply` | num | Baseline M3 |
#' | `sig` | chr | NLP method |
#' | `route_concept_id` | int | Oral-route filter |
#' | `dose_unit_source_value` | chr | Unit context |
#' | `amount_value` | num | Strength (Baseline M2/M3/M4) |
#'
#' ## dmard_* fields
#' Legacy `dmard_*` columns (e.g. `dmard_name`, `dmard_dose`) belong to the
#' **analysis layer** and should be derived after [build_episodes()], not used
#' as Connector inputs.
#'
#' @name drug_df_contract
#' @aliases drug_df_contract
NULL

# ---------------------------------------------------------------------------
# run_pipeline
# ---------------------------------------------------------------------------

#' Run the full SteroidDoseR pipeline in a single call
#'
#' Convenience wrapper that chains data extraction, daily-dose imputation,
#' prednisone-equivalency conversion, and optional episode building into a
#' single function call. Each step can also be run independently using the
#' individual exported functions.
#'
#' @param connector_or_df A `steroid_connector` or data frame. Passed to the
#'   selected dose method.
#' @param method `character(1)`. Imputation method: `"baseline"` (default) or
#'   `"nlp"`.
#' @param drug_concept_ids,person_ids,start_date,end_date,sig_source
#'   Passed to the dose function. Ignored when `connector_or_df` is a data
#'   frame.
#' @param gap_days `integer(1)`. Gap threshold for [build_episodes()]. Only
#'   used when `return_level = "episode"`. Default: `30L`.
#' @param equiv_table Optional custom equivalency table. Passed to
#'   [convert_pred_equiv()]. Default `NULL` uses the built-in table.
#' @param return_level `character(1)`. `"exposure"` returns one row per
#'   drug-exposure record with dose columns appended. `"episode"` (default)
#'   additionally runs [build_episodes()] and returns one row per
#'   patient–drug episode.
#'
#' @return
#' - When `return_level = "exposure"`: the dose data frame from the chosen
#'   method, with `pred_equiv_mg` and `equiv_factor` columns appended.
#' - When `return_level = "episode"`: episode summary from [build_episodes()].
#'
#' @export
#'
#' @examples
#' extdata <- system.file("extdata", package = "SteroidDoseR")
#' drug_exp <- readr::read_csv(
#'   file.path(extdata, "synthetic_drug_exposure.csv"),
#'   show_col_types = FALSE
#' )
#' con <- create_df_connector(drug_exp)
#' episodes <- run_pipeline(con, method = "baseline")
#' episodes[, c("person_id", "drug_name_std", "episode_start",
#'              "episode_end", "median_daily_dose")]
run_pipeline <- function(connector_or_df,
                         method           = c("baseline", "nlp"),
                         drug_concept_ids = NULL,
                         person_ids       = NULL,
                         start_date       = NULL,
                         end_date         = NULL,
                         sig_source       = "sig",
                         gap_days         = 30L,
                         equiv_table      = NULL,
                         return_level     = c("episode", "exposure")) {

  method       <- match.arg(method)
  return_level <- match.arg(return_level)

  # ------------------------------------------------------------------
  # Step 1: Fetch / validate drug_df
  # ------------------------------------------------------------------
  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date, sig_source)

  # ------------------------------------------------------------------
  # Step 2: Daily dose imputation
  # ------------------------------------------------------------------
  if (method == "baseline") {
    drug_df  <- calc_daily_dose_baseline(drug_df)
    dose_col <- "daily_dose_mg_imputed"
  } else {
    drug_df  <- calc_daily_dose_nlp(drug_df, sig_source = sig_source)
    dose_col <- "daily_dose_mg"
  }

  # ------------------------------------------------------------------
  # Step 3: Drug name standardisation (ensure drug_name_std exists)
  # ------------------------------------------------------------------
  if (!"drug_name_std" %in% names(drug_df)) {
    name_src <- intersect(
      c("drug_concept_name", "drug_source_value"), names(drug_df)
    )
    if (length(name_src) > 0L) {
      drug_df[["drug_name_std"]] <- standardize_drug_name(
        drug_df[[name_src[[1L]]]]
      )
    } else {
      rlang::warn(
        "No drug name column found; drug_name_std will be NA."
      )
      drug_df[["drug_name_std"]] <- NA_character_
    }
  }

  # ------------------------------------------------------------------
  # Step 4: Prednisone-equivalency conversion
  # ------------------------------------------------------------------
  if (dose_col %in% names(drug_df)) {
    drug_df <- convert_pred_equiv(
      drug_df,
      drug_col    = "drug_name_std",
      dose_col    = dose_col,
      equiv_table = equiv_table
    )
  }

  if (return_level == "exposure") {
    return(drug_df)
  }

  # ------------------------------------------------------------------
  # Step 5: Episode building
  # ------------------------------------------------------------------
  end_col_arg  <- if ("drug_exposure_end_date" %in% names(drug_df))
    "drug_exposure_end_date" else NA_character_

  final_dose   <- if ("pred_equiv_mg" %in% names(drug_df))
    "pred_equiv_mg" else dose_col

  build_episodes(
    drug_df,
    end_col  = end_col_arg,
    dose_col = final_dose,
    gap_days = gap_days
  )
}