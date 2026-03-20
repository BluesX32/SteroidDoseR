# SteroidDoseR 0.1.1

## Bug fixes

* **Baseline — unit-safe `amount_value`** (`calc_daily_dose_baseline()`):
  `drug_strength.amount_value` is now only accepted as mg when
  `amount_unit_concept_id` is the OMOP mg concept (8576) or absent/unknown.
  Records whose unit concept indicates mcg, g, or any other non-mg unit are
  discarded and the string-extraction fallback (`drug_source_value`) is used
  instead. Previously these records were treated as mg, producing doses in the
  billions of mg/day. (#BUG-6)

* **Baseline — dose plausibility cap** (`calc_daily_dose_baseline()`):
  New `max_daily_dose_mg` parameter (default 2000 mg/day). Records whose
  imputed dose exceeds this value are set to NA and labeled `"missing"`, with
  a warning that counts the affected rows and names the likely data-quality
  causes. Pass `NULL` to disable. (#BUG-6)

* **NLP — strength fallback from `amount_value` / drug name**
  (`calc_daily_dose_nlp()`):
  When a SIG string contains administration instructions but no mg amount (the
  common OMOP pattern, e.g. "take 1 tablet daily"), the parser now uses
  `amount_value` (from the `drug_strength` JOIN) as the per-tablet strength,
  falling back to mg extracted from `drug_concept_name` or
  `drug_source_value`. Records where `freq_per_day` was successfully parsed
  are promoted from `"no_parse"` to `"ok"`. Previously all such records
  returned `daily_dose_mg = NA`. (#BUG-5)

---

# SteroidDoseR 0.1.0

## New features

* `calc_daily_dose_baseline()`: Four-step cascading imputation (M1 original
  → M2 tablets×freq×strength → M3 quantity/days_supply → M4
  quantity/actual_duration) faithful to the Baseline.qmd analysis from the
  OHDSI 2025 AgentDose study.

* `calc_daily_dose_nlp()`: Rule-based SIG text parsing via `parse_sig_one()`.
  Handles QD/BID/TID/QID/QOD/q6h/q8h/q12h frequencies, taper language, PRN
  flags, and explicit mg totals. Never throws an error — malformed input
  returns `parsed_status = "empty"` or `"no_parse"`.

* `convert_pred_equiv()`: Multiplies raw daily doses by built-in prednisone-
  equivalency factors (prednisone/prednisolone 1.0, methylprednisolone 1.25,
  dexamethasone 7.5, hydrocortisone 0.25, triamcinolone 1.25; budesonide
  flagged as NA). Supports custom equivalency tables.

* `build_episodes()`: Gap-bridging algorithm (default 30 days) that merges
  overlapping or adjacent prescriptions into continuous episodes, returning
  episode_start, episode_end, n_days, n_records, and dose statistics.

* `evaluate_against_gold()`: Date-overlap join against a manually-reviewed
  gold standard. Returns `$summary` (coverage%, MAE, MBE, RMSE, MAPE,
  Pearson/Spearman), `$comparison` (per-episode detail), and `$stratified`
  (by dose range and taper status) — matching the OHDSI 2025 poster metrics.

## Testing

* 112 unit tests across 5 test files using testthat edition 3.
* Synthetic fixtures only (`inst/extdata/`) — no real patient data.

## Notes

* Phase 2 (LLM agent integration) is not yet included.
* `fuzzyjoin` is listed in Suggests; the core overlap join uses only dplyr.
