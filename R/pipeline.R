# pipeline.R
# Convenience wrapper that chains fetch -> impute -> convert -> episode.

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
#' | `drug_source_value` | chr | Strength extraction (Baseline supply_based/actual_duration) |
#' | `drug_exposure_end_date` | Date | Duration (Baseline actual_duration M3, episodes) |
#' | `quantity` | num | Baseline actual_duration (M3), supply_based (M4) |
#' | `days_supply` | num | Baseline supply_based (M4, fallback) |
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
#' @param method `character(1)`. Imputation method: `"baseline"` (default),
#'   `"nlp"`, or `"nlp_notes"`. The `"nlp_notes"` method runs the regex SIG
#'   parser first, then falls back to medspaCy for unresolved records (requires
#'   Python + medspaCy — see [calc_daily_dose_nlp_notes()]).
#' @param m2_sig_parse `character(1)`. Only used when `method = "baseline"`.
#'   Controls how M2 (`tablets_freq`) is handled when `tablets` and
#'   `freq_per_day` are absent:
#'   \describe{
#'     \item{`"auto"` (default)}{Parse the `sig` column inside
#'       [calc_daily_dose_baseline()] to populate `tablets` and `freq_per_day`.
#'       Recommended for real-world OMOP data where these columns are absent.}
#'     \item{`"warn"`}{Warn and skip M2.}
#'     \item{`"nlp_first"`}{Run [parse_sig()] on the full data frame *before*
#'       calling [calc_daily_dose_baseline()], so NLP-derived `tablets` and
#'       `freq_per_day` are available for M2.}
#'     \item{`"none"`}{Silently skip M2.}
#'   }
#' @param note_col `character(1)`. Name of the clinical note text column.
#'   Passed to [calc_daily_dose_nlp_notes()] when `method = "nlp_notes"`.
#'   Default: `"clinical_note"`.
#' @param drug_concept_ids,person_ids,start_date,end_date,sig_source
#'   Passed to the dose function. Ignored when `connector_or_df` is a data
#'   frame.
#' @param gap_days `integer(1)`. Gap threshold for [build_episodes()]. Only
#'   used when `return_level = "episode"`. Default: `30L`.
#' @param return_level `character(1)`. `"exposure"` returns one row per
#'   drug-exposure record with dose columns appended. `"episode"` (default)
#'   additionally runs [build_episodes()] and returns one row per
#'   patient-drug episode.
#'
#' @return
#' - When `return_level = "exposure"`: the dose data frame from the chosen
#'   method.
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
                         method           = c("baseline", "nlp", "nlp_notes", "hierarchical"),
                         m2_sig_parse     = c("auto", "warn", "nlp_first", "none"),
                         note_col         = "clinical_note",
                         drug_concept_ids = NULL,
                         person_ids       = NULL,
                         start_date       = NULL,
                         end_date         = NULL,
                         sig_source       = "sig",
                         gap_days         = 30L,
                         return_level     = c("episode", "exposure")) {

  method       <- match.arg(method)
  m2_sig_parse <- match.arg(m2_sig_parse)
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
    if (m2_sig_parse == "nlp_first") {
      # Option C: run NLP SIG parser first so tablets/freq_per_day are
      # available for M2, then hand the enriched data frame to baseline.
      sig_col <- if ("sig" %in% names(drug_df)) "sig" else sig_source
      if (sig_col %in% names(drug_df)) {
        message("m2_sig_parse = 'nlp_first': running parse_sig() before baseline.")
        drug_df <- parse_sig(drug_df, sig_col = sig_col)
      }
      drug_df  <- calc_daily_dose_baseline(drug_df, m2_sig_parse = "none")
    } else {
      drug_df  <- calc_daily_dose_baseline(drug_df, m2_sig_parse = m2_sig_parse)
    }
    dose_col <- "daily_dose_mg_imputed"
  } else if (method == "nlp") {
    drug_df  <- calc_daily_dose_nlp(drug_df, sig_source = sig_source)
    dose_col <- "daily_dose_mg"
  } else if (method == "nlp_notes") {
    drug_df  <- calc_daily_dose_nlp_notes(drug_df, note_col = note_col,
                                          sig_source = sig_source)
    dose_col <- "daily_dose_mg"
  } else {
    drug_df  <- calc_daily_dose_hierarchical(drug_df, sig_source = sig_source)
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

  if (return_level == "exposure") {
    return(drug_df)
  }

  # ------------------------------------------------------------------
  # Step 4: Episode building
  # ------------------------------------------------------------------
  end_col_arg <- if ("drug_exposure_end_date" %in% names(drug_df))
    "drug_exposure_end_date" else NA_character_

  build_episodes(
    drug_df,
    end_col  = end_col_arg,
    dose_col = dose_col,
    gap_days = gap_days
  )
}