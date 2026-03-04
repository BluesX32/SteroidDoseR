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
