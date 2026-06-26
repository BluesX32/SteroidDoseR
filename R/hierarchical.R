# hierarchical.R
# Hierarchical daily-dose imputation: Baseline cross-checked against NLP Advanced.
#
# Decision tree per row:
#
#  baseline complete  sig complete
#  ──────────────────────────────────────────────────────────────────────────
#  TRUE               TRUE     → cross-check
#                                  |diff| ≤ match_tol            → cross_checked    (NLP value)
#                                  |diff| > diff_threshold        → nlp_override     (NLP value)
#                                  match_tol < |diff| ≤ threshold → nlp_preferred /  (controlled by
#                                                                    baseline_preferred  moderate_disagreement_action)
#  TRUE               FALSE    → baseline_only
#  FALSE              TRUE & !taper → nlp_fills_baseline (NLP fills missing fields)
#  FALSE              TRUE & taper  → nlp_taper          (NLP handles taper directly)
#  FALSE              FALSE    → missing (NA)
#
# "Baseline" here uses M1/M3/M4 ONLY (original column, actual_duration, supply_based).
# M2 (tablets_freq) is intentionally excluded so the two sides remain independent:
#   Baseline = quantity / supply side of OMOP data
#   NLP      = SIG text side of OMOP data
# This makes the cross-check comparison meaningful.
#
# NLP strength fallback reuses `strength_mg` already computed by baseline (unit-safe)
# to avoid duplicating the amount_value / drug_concept_name extraction logic.

# ---------------------------------------------------------------------------
# Exported: hierarchical daily-dose imputation
# ---------------------------------------------------------------------------

#' Compute daily steroid doses using the hierarchical Baseline + NLP method
#'
#' Runs Baseline (M1/M3/M4) and NLP Advanced (SIG parser) independently, then
#' applies a per-row decision tree that cross-checks the two estimates and
#' selects the most appropriate result.
#'
#' ## Decision logic
#'
#' | Baseline | SIG | Outcome |
#' |---|---|---|
#' | complete | complete | Cross-check; blend or override based on `diff_threshold` |
#' | complete | absent / unparseable | Baseline only |
#' | incomplete | complete, steady | NLP fills missing baseline fields |
#' | incomplete | complete, taper | NLP calculates dose from taper SIG |
#' | incomplete | absent / unparseable | `NA` |
#'
#' **Cross-check resolution:**
#' - `|diff| ≤ match_tol` → `"cross_checked"` (use NLP value; both agree)
#' - `match_tol < |diff| ≤ diff_threshold` → controlled by `moderate_disagreement_action`:
#'   `"nlp_preferred"` (default), `"baseline_preferred"`, or `"blended"` (legacy average)
#' - `|diff| > diff_threshold` → `"nlp_override"` (use NLP; large discrepancy)
#'
#' ## Independence of the two sides
#' Baseline uses M1 (original daily_dose column), M3 (quantity / actual duration),
#' and M4 (quantity / days_supply) only. M2 (tablets × freq × strength) is
#' excluded so the baseline and NLP estimates are derived from independent data
#' sources, making the cross-check meaningful.
#'
#' @param connector_or_df A `steroid_connector` or a data frame.
#' @param sig_col `character(1)`. Column with SIG text. Default: `"sig"`.
#' @param drug_name_col `character(1)`. Column with drug name.
#'   Default: `"drug_concept_name"`.
#' @param filter_oral `logical(1)`. Keep only oral-route records.
#'   Default: `TRUE`.
#' @param diff_threshold `numeric(1)`. Absolute difference (mg/day) above which
#'   the NLP result overrides baseline in a cross-check. Default: `5`.
#' @param match_tol `numeric(1)`. Absolute difference (mg/day) at or below which
#'   the two estimates are considered to "match". Default: `1`. A tolerance of
#'   1 mg/day is appropriate because corticosteroids are dispensed in whole-mg
#'   tablet strengths; two estimates within 1 mg are clinically concordant.
#' @param moderate_disagreement_action `character(1)`. What to do when both
#'   Baseline and NLP have a dose and `match_tol < |diff| <= diff_threshold`
#'   (moderate disagreement). `"nlp"` (default): use the NLP value — the SIG
#'   text is the prescriber's explicit intent. `"baseline"`: use the structured
#'   field value — more resistant to SIG mis-parse. `"blend"`: arithmetic mean
#'   of both (legacy behaviour; statistically unjustified but available for
#'   backward compatibility). The method label in the output reflects the choice:
#'   `"nlp_preferred"`, `"baseline_preferred"`, or `"blended"`.
#' @param prn_action `character(1)`. How to handle PRN records. `"na"` (default)
#'   sets `daily_dose_mg = NA` for PRN records — the hierarchical decision
#'   naturally yields `baseline_only` or `missing` for PRN, but PRN records can
#'   still receive a baseline dose via the structured fields. `"na"` prevents
#'   this by also excluding PRN from the final result. `"keep"` passes PRN
#'   through to `baseline_only` / `missing` without nulling out.
#' @param expand_tapers `logical(1)`. When `TRUE`, taper SIGs handled by the
#'   NLP branch are expanded into per-step rows via [parse_taper_schedule()].
#'   Default: `FALSE`.
#' @param max_daily_dose_mg `numeric(1)` or `NULL`. Plausibility cap. Records
#'   above this value are set to `NA` with a warning. Default: `2000`.
#' @param equiv_table,drug_name_map,drug_concept_ids,person_ids,start_date,end_date,sig_source
#'   Passed to the underlying imputation functions.
#'
#' @return A data frame with the original columns plus:
#' \describe{
#'   \item{`drug_name_std`}{Standardised drug name.}
#'   \item{`bl_dose`}{Baseline daily dose (mg/day) from M1/M3/M4.}
#'   \item{`bl_method`}{Baseline imputation method used.}
#'   \item{`sig_dose`}{NLP daily dose (mg/day) from SIG text + strength fallback.}
#'   \item{`sig_status`}{NLP parsed status.}
#'   \item{`sig_tablets`}{Tablets per dose from SIG.}
#'   \item{`sig_freq_per_day`}{Doses per day from SIG.}
#'   \item{`sig_mg_per_admin`}{mg per administration from SIG.}
#'   \item{`sig_taper_flag`}{`TRUE` if taper language detected in SIG.}
#'   \item{`sig_duration_days`}{Prescribed duration from SIG.}
#'   \item{`daily_dose_mg`}{Final hierarchical daily dose (mg/day).}
#'   \item{`hierarchical_method`}{One of `"cross_checked"`, `"nlp_override"`,
#'     `"nlp_preferred"`, `"baseline_preferred"`, `"blended"` (legacy),
#'     `"baseline_only"`, `"nlp_fills_baseline"`, `"nlp_taper"`,
#'     `"missing"`, `"implausible"`.}
#' }
#'
#' @seealso [calc_daily_dose_baseline()], [calc_daily_dose_nlp_advanced()]
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   person_id                = 1:3,
#'   drug_concept_name        = rep("prednisone 5 MG oral tablet", 3),
#'   route_concept_name       = rep("oral", 3),
#'   sig = c(
#'     "Take 2 tablets (10 mg) daily",   # both complete, should match
#'     "Take 1 tablet daily",            # NLP needs strength fallback
#'     NA_character_                     # baseline only
#'   ),
#'   quantity                 = c(60, 90, 90),
#'   days_supply              = c(30, 90, 90),
#'   amount_value             = c(5, 5, 5),
#'   amount_unit_concept_id   = c(8576L, 8576L, 8576L),
#'   drug_exposure_start_date = rep(as.Date("2023-01-01"), 3),
#'   drug_exposure_end_date   = rep(as.Date("2023-02-01"), 3)
#' )
#' calc_daily_dose_hierarchical(df)
calc_daily_dose_hierarchical <- function(connector_or_df,
                                         sig_col           = "sig",
                                         drug_name_col     = "drug_concept_name",
                                         filter_oral       = TRUE,
                                         diff_threshold    = 5,
                                         match_tol                      = 1,
                                         moderate_disagreement_action   = c("nlp", "baseline", "blend"),
                                         prn_action                     = c("na", "keep"),
                                         expand_tapers                  = FALSE,
                                         max_daily_dose_mg = 2000,
                                         equiv_table       = NULL,
                                         drug_name_map     = NULL,
                                         drug_concept_ids  = NULL,
                                         person_ids        = NULL,
                                         start_date        = NULL,
                                         end_date          = NULL,
                                         sig_source        = "sig") {
  moderate_disagreement_action <- match.arg(moderate_disagreement_action)
  prn_action                   <- match.arg(prn_action)

  # --- 1. Resolve, standardise, filter ----------------------------------------
  drug_df <- .resolve_drug_df(connector_or_df, drug_concept_ids, person_ids,
                               start_date, end_date, sig_source)

  assert_required_cols(drug_df, drug_name_col, "drug_df")

  if (!sig_col %in% names(drug_df)) {
    drug_df[[sig_col]] <- NA_character_
  }

  drug_df <- drug_df |>
    dplyr::mutate(drug_name_std = standardize_drug_name(.data[[drug_name_col]],
                                                         drug_name_map = drug_name_map))

  .etbl          <- if (is.null(equiv_table)) .pred_equiv_table else equiv_table
  known_steroids <- .etbl$drug_name_std[!is.na(.etbl$drug_name_std) & !is.na(.etbl$equiv_factor)]
  drug_df        <- drug_df[drug_df$drug_name_std %in% known_steroids, ]

  if (filter_oral) {
    rc <- if ("route_concept_name" %in% names(drug_df)) drug_df$route_concept_name else NULL
    rs <- if ("route_source_value" %in% names(drug_df)) drug_df$route_source_value else NULL
    dc <- if ("drug_concept_name"  %in% names(drug_df)) drug_df$drug_concept_name  else NULL
    ds <- if ("drug_source_value"  %in% names(drug_df)) drug_df$drug_source_value  else NULL

    if (is.null(rc) && is.null(rs) && is.null(dc) && is.null(ds)) {
      rlang::warn("No route column found; skipping oral-route filter.")
    } else {
      route_class <- classify_route(rc, rs, dc, ds)
      drug_df     <- drug_df[route_class == "oral" | is.na(route_class), ]
    }
  }

  if (nrow(drug_df) == 0L) {
    rlang::warn("No corticosteroid records found after filtering.")
    return(drug_df)
  }

  # --- 2. Baseline: M1/M3/M4 only (quantity / supply side) --------------------
  # M2 (tablets_freq) is excluded so baseline and NLP are independent sources.
  bl_result <- calc_daily_dose_baseline(
    drug_df,
    filter_oral       = FALSE,
    m2_sig_parse      = "none",
    max_daily_dose_mg = NULL,        # cap applied at the end
    equiv_table       = equiv_table,
    drug_name_map     = drug_name_map,
    methods           = c("original", "actual_duration", "supply_based")
  )

  bl_dose    <- bl_result$daily_dose_mg_imputed
  bl_method  <- bl_result$imputation_method
  # strength_mg is also in bl_result; re-used for NLP strength fallback below
  strength_mg <- if ("strength_mg" %in% names(bl_result)) bl_result$strength_mg
                 else rep(NA_real_, nrow(drug_df))

  # Drop daily_dose_mg from drug_df before parse_sig_advanced: baseline has
  # already captured what it needs, and leaving it in causes a bind_cols column
  # name collision (drug_df$daily_dose_mg vs. parse_sig_advanced's output column
  # of the same name), making nlp_parsed$daily_dose_mg NULL afterward.
  if ("daily_dose_mg" %in% names(drug_df)) {
    drug_df[["daily_dose_mg"]] <- NULL
  }

  # --- 3. NLP: SIG text side (parse_sig_advanced, no baseline fallback) -------
  nlp_parsed <- parse_sig_advanced(drug_df, sig_col = sig_col)

  # --- 4. NLP strength fallback ------------------------------------------------
  # When the SIG has tablets + freq but no mg (e.g. "Take 1 tablet daily"),
  # use strength_mg already computed by baseline to fill in the mg component.
  # This is the "nlp fills missing baseline columns" mechanism:
  #   sig_dose = strength_mg_baseline × tablets_nlp × freq_nlp
  has_no_mg <- nlp_parsed$parsed_status == "no_parse" & is.na(nlp_parsed$mg_per_admin)

  if (any(has_no_mg, na.rm = TRUE)) {
    fb_strength <- strength_mg[has_no_mg]
    fb_tablets  <- dplyr::coalesce(nlp_parsed$tablets[has_no_mg], 1)
    fb_freq     <- nlp_parsed$freq_per_day[has_no_mg]

    fb_mg_admin <- dplyr::if_else(!is.na(fb_strength), fb_strength * fb_tablets, NA_real_)
    fb_dose     <- dplyr::if_else(
      !is.na(fb_mg_admin) & !is.na(fb_freq),
      fb_mg_admin * fb_freq,
      NA_real_
    )

    nlp_parsed$mg_per_admin[has_no_mg]  <- fb_mg_admin
    nlp_parsed$daily_dose_mg[has_no_mg] <- fb_dose
    nlp_parsed$parsed_status[has_no_mg] <- dplyr::if_else(
      !is.na(fb_dose),
      "ok",
      nlp_parsed$parsed_status[has_no_mg]
    )
  }

  sig_dose   <- nlp_parsed$daily_dose_mg
  sig_status <- nlp_parsed$parsed_status

  # --- 5. Taper expansion (optional) ------------------------------------------
  if (expand_tapers) {
    taper_mask <- sig_status %in% c("taper")
    if (any(taper_mask, na.rm = TRUE)) {
      taper_rows    <- drug_df[taper_mask, ]
      taper_rows[[".sig_text"]] <- taper_rows[[sig_col]]

      expanded <- purrr::map_dfr(seq_len(nrow(taper_rows)), function(i) {
        steps <- tryCatch(
          parse_taper_schedule(taper_rows[[".sig_text"]][[i]]),
          error = function(e) NULL
        )
        if (is.null(steps) || nrow(steps) == 0L) return(taper_rows[i, ])
        n <- nrow(steps)
        rep_row <- taper_rows[rep(i, n), ]
        rep_row$sig_dose_taper_step     <- steps$step
        rep_row$sig_dose_taper_start    <- steps$step_start_day
        rep_row$sig_dose_taper_end      <- steps$step_end_day
        rep_row$.expanded_dose          <- steps$daily_dose_mg
        rep_row$.expanded_freq          <- steps$freq_per_day
        rep_row$.expanded_dur           <- steps$duration_days
        rep_row
      })

      sig_dose[taper_mask]   <- expanded$.expanded_dose
      sig_status[taper_mask] <- "taper_ok"
    }
  }

  # --- 6. Hierarchical decision logic -----------------------------------------
  bl_complete  <- !is.na(bl_dose)
  sig_exists   <- !is.na(drug_df[[sig_col]]) &
                  nchar(trimws(as.character(drug_df[[sig_col]]))) > 0L
  sig_complete <- sig_status == "ok"
  is_taper     <- sig_status %in% c("taper", "taper_ok")
  dose_diff    <- abs(bl_dose - sig_dose)   # NA when either is NA

  moderate_label <- switch(moderate_disagreement_action,
    nlp      = "nlp_preferred",
    baseline = "baseline_preferred",
    blend    = "blended"
  )

  hierarchical_method <- dplyr::case_when(
    # Branch 1: both sides have a computable dose → cross-check
    bl_complete & sig_complete & !is.na(dose_diff) &
      dose_diff <= match_tol                  ~ "cross_checked",
    bl_complete & sig_complete & !is.na(dose_diff) &
      dose_diff > diff_threshold              ~ "nlp_override",
    bl_complete & sig_complete & !is.na(dose_diff) ~ moderate_label,

    # Branch 2: baseline has a dose; SIG is absent or unresolvable
    bl_complete                               ~ "baseline_only",

    # Branch 3a: baseline could not compute; SIG is clean steady dosing
    !bl_complete & sig_exists &
      sig_complete & !is_taper               ~ "nlp_fills_baseline",

    # Branch 3b: baseline could not compute; SIG is a taper/escalation
    !bl_complete & sig_exists & is_taper     ~ "nlp_taper",

    # Branch 4: baseline could not compute and no usable SIG
    TRUE                                     ~ "missing"
  )

  daily_dose_mg <- dplyr::case_when(
    hierarchical_method == "cross_checked"       ~ sig_dose,
    hierarchical_method == "nlp_override"        ~ sig_dose,
    hierarchical_method == "nlp_preferred"       ~ sig_dose,
    hierarchical_method == "baseline_preferred"  ~ bl_dose,
    hierarchical_method == "blended"             ~ (bl_dose + sig_dose) / 2,
    hierarchical_method == "baseline_only"       ~ bl_dose,
    hierarchical_method == "nlp_fills_baseline"  ~ sig_dose,
    hierarchical_method == "nlp_taper"           ~ sig_dose,
    TRUE                                         ~ NA_real_
  )

  # --- 7. Assemble result -----------------------------------------------------
  result <- drug_df |>
    dplyr::mutate(
      bl_dose          = bl_dose,
      bl_method        = bl_method,
      sig_dose         = sig_dose,
      sig_status       = sig_status,
      sig_tablets      = nlp_parsed$tablets,
      sig_freq_per_day = nlp_parsed$freq_per_day,
      sig_mg_per_admin = nlp_parsed$mg_per_admin,
      sig_taper_flag   = nlp_parsed$taper_flag,
      sig_duration_days = nlp_parsed$duration_days,
      hierarchical_method = hierarchical_method,
      daily_dose_mg    = daily_dose_mg
    )

  # --- 8. Plausibility cap ----------------------------------------------------
  if (!is.null(max_daily_dose_mg) && is.finite(max_daily_dose_mg)) {
    implausible <- !is.na(result$daily_dose_mg) &
                   result$daily_dose_mg > max_daily_dose_mg
    if (any(implausible, na.rm = TRUE)) {
      rlang::warn(sprintf(
        paste0(
          "%d record(s) have daily_dose_mg > %.0f mg/day and were set to NA.\n",
          "Pass max_daily_dose_mg = NULL to disable this cap."
        ),
        sum(implausible), max_daily_dose_mg
      ))
      result$daily_dose_mg[implausible]        <- NA_real_
      result$hierarchical_method[implausible]  <- "implausible"
    }
  }

  # --- 9. PRN action ----------------------------------------------------------
  if (prn_action == "na") {
    prn_mask <- result$sig_status == "prn"
    result$daily_dose_mg[prn_mask]           <- NA_real_
    result$hierarchical_method[prn_mask]     <- "prn_excluded"
  }

  result
}

# ---------------------------------------------------------------------------
# Internal: re-apply adaptive decision to pre-computed hierarchical intermediates
# ---------------------------------------------------------------------------

#' Re-apply the adaptive decision rule to pre-computed hierarchical intermediates
#'
#' Used for parameter grid search: run `calc_daily_dose_hierarchical()` once to
#' obtain `bl_dose`, `sig_dose`, `sig_status`, and `sig_taper_flag`, then call
#' this function repeatedly with different parameter combinations to find the
#' combination that minimises MAE against a held-out gold standard — without
#' re-running the baseline and NLP parsers each time.
#'
#' @param df Data frame with columns `bl_dose`, `sig_dose`, `sig_status`,
#'   `sig_taper_flag` (as produced by `calc_daily_dose_hierarchical()`).
#' @param match_tol `numeric(1)`. `|diff| <= match_tol` → `"cross_checked"`.
#' @param diff_thr `numeric(1)`. `|diff| > diff_thr` → `"adaptive_override"`;
#'   between `match_tol` and `diff_thr` → `"blended"`.
#' @param override `character(1)`. Which source wins when `|diff| > diff_thr`:
#'   `"nlp"` (default) or `"baseline"`.
#' @return `df` with `daily_dose_mg` and `hierarchical_method` columns updated.
#' @noRd
apply_adaptive_decision <- function(df, match_tol, diff_thr, override = "nlp") {
  bl    <- df$bl_dose
  sig   <- df$sig_dose
  stat  <- df$sig_status
  taper <- !is.na(df$sig_taper_flag) & df$sig_taper_flag

  bl_ok         <- !is.na(bl)
  sig_ok_steady <- !is.na(sig) & !is.na(stat) & stat == "ok" & !taper
  sig_ok_taper  <- taper & !is.na(sig)

  dose_diff <- abs(bl - sig)

  daily_dose <- dplyr::case_when(
    bl_ok & sig_ok_steady & dose_diff <= match_tol  ~ sig,
    bl_ok & sig_ok_steady & dose_diff <= diff_thr   ~ (bl + sig) / 2,
    bl_ok & sig_ok_steady & dose_diff >  diff_thr   ~
      if (override == "nlp") sig else bl,
    bl_ok & !sig_ok_steady & !sig_ok_taper          ~ bl,
    !bl_ok & sig_ok_steady                          ~ sig,
    !bl_ok & sig_ok_taper                           ~ sig,
    TRUE                                            ~ NA_real_
  )

  method <- dplyr::case_when(
    bl_ok & sig_ok_steady & dose_diff <= match_tol  ~ "cross_checked",
    bl_ok & sig_ok_steady & dose_diff <= diff_thr   ~ "blended",
    bl_ok & sig_ok_steady & dose_diff >  diff_thr   ~ "adaptive_override",
    bl_ok & !sig_ok_steady & !sig_ok_taper          ~ "baseline_only",
    !bl_ok & sig_ok_steady                          ~ "nlp_fills_baseline",
    !bl_ok & sig_ok_taper                           ~ "nlp_taper",
    TRUE                                            ~ "missing"
  )

  df$daily_dose_mg       <- daily_dose
  df$hierarchical_method <- method
  df
}

# ---------------------------------------------------------------------------
# Exported: grid-search threshold tuner
# ---------------------------------------------------------------------------

#' Grid-search match_tol and diff_threshold against a gold standard
#'
#' Runs `calc_daily_dose_hierarchical()` once to obtain the intermediate
#' `bl_dose` and `sig_dose` columns, then evaluates every combination of
#' `match_tol` × `diff_threshold` × `moderate_disagreement_action` using
#' `apply_adaptive_decision()` — without re-running the baseline and NLP
#' parsers. Use the returned MAE grid to select parameters for the primary
#' analysis.
#'
#' @param drug_df Data frame of drug-exposure records (same input as
#'   [calc_daily_dose_hierarchical()]).
#' @param gold_df Data frame of manually reviewed gold-standard episodes.
#'   Must contain `person_id`, `episode_start`, `episode_end`, and a dose
#'   column (see `gold_dose_col`).
#' @param match_tol_grid `numeric` vector. Values of `match_tol` to test.
#'   Default: `c(0.5, 1, 2, 5)`.
#' @param diff_thr_grid `numeric` vector. Values of `diff_threshold` to test.
#'   Default: `c(3, 5, 10, 20)`.
#' @param actions `character` vector. Which `moderate_disagreement_action`
#'   values to test. Default: `c("nlp", "baseline")`.
#' @param gold_dose_col `character(1)`. Dose column in `gold_df`.
#'   Default: `"dose_daily_mg_equiv"`.
#' @param ... Additional arguments forwarded to [calc_daily_dose_hierarchical()]
#'   for the initial parse (e.g. `sig_col`, `filter_oral`, `equiv_table`).
#'
#' @return A tibble with one row per parameter combination and columns:
#'   `match_tol`, `diff_threshold`, `moderate_disagreement_action`,
#'   `n_matched`, `MAE`, `MBE`, `RMSE`, `coverage_pct`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' grid <- tune_hierarchical_thresholds(drug_df, gold_df)
#' grid[which.min(grid$MAE), ]
#' }
tune_hierarchical_thresholds <- function(drug_df,
                                          gold_df,
                                          match_tol_grid = c(0.5, 1, 2, 5),
                                          diff_thr_grid  = c(3, 5, 10, 20),
                                          actions        = c("nlp", "baseline"),
                                          gold_dose_col  = "dose_daily_mg_equiv",
                                          ...) {

  # Run the parser once to get intermediates
  parsed <- calc_daily_dose_hierarchical(
    drug_df,
    moderate_disagreement_action = "nlp",  # doesn't matter for intermediates
    ...
  )

  assert_required_cols(gold_df,
    c("person_id", "episode_start", "episode_end", gold_dose_col), "gold_df")

  grid <- expand.grid(
    match_tol                    = match_tol_grid,
    diff_threshold               = diff_thr_grid,
    moderate_disagreement_action = actions,
    stringsAsFactors             = FALSE
  )
  # Only keep rows where match_tol < diff_threshold
  grid <- grid[grid$match_tol < grid$diff_threshold, ]

  results <- lapply(seq_len(nrow(grid)), function(i) {
    row     <- grid[i, ]
    action  <- row$moderate_disagreement_action

    candidate <- apply_adaptive_decision(
      parsed,
      match_tol = row$match_tol,
      diff_thr  = row$diff_threshold,
      override  = action
    )

    # Convert to episodes and evaluate
    ep <- tryCatch(
      build_episodes(candidate,
        dose_col  = "daily_dose_mg",
        end_col   = if ("drug_exposure_end_date" %in% names(candidate))
                      "drug_exposure_end_date" else NA_character_
      ),
      error = function(e) NULL
    )
    if (is.null(ep) || nrow(ep) == 0L) {
      return(tibble::tibble(
        match_tol = row$match_tol, diff_threshold = row$diff_threshold,
        moderate_disagreement_action = action,
        n_matched = 0L, MAE = NA_real_, MBE = NA_real_,
        RMSE = NA_real_, coverage_pct = 0
      ))
    }

    ev <- tryCatch(
      evaluate_against_gold(
        convert_pred_equiv(ep, dose_col = "median_daily_dose",
                           out_col = "pred_equiv_mg"),
        gold_df,
        computed_dose_col = "pred_equiv_mg",
        gold_dose_col     = gold_dose_col
      ),
      error = function(e) NULL
    )
    if (is.null(ev)) {
      return(tibble::tibble(
        match_tol = row$match_tol, diff_threshold = row$diff_threshold,
        moderate_disagreement_action = action,
        n_matched = 0L, MAE = NA_real_, MBE = NA_real_,
        RMSE = NA_real_, coverage_pct = 0
      ))
    }

    tibble::tibble(
      match_tol                    = row$match_tol,
      diff_threshold               = row$diff_threshold,
      moderate_disagreement_action = action,
      n_matched                    = ev$summary$n_matched_periods,
      MAE                          = ev$summary$MAE,
      MBE                          = ev$summary$MBE,
      RMSE                         = ev$summary$RMSE,
      coverage_pct                 = ev$summary$coverage_pct
    )
  })

  dplyr::bind_rows(results) |>
    dplyr::arrange(.data$MAE)
}
