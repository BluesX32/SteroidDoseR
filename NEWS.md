# SteroidDoseR 0.1.6

## Enhancements — generalizability for other OMOP sites

* **`standardize_drug_name()`** gains a `drug_name_map` parameter: a data frame
  with columns `pattern` (regex) and `canonical_name`. User-supplied rules are
  applied after the built-in mapping and take priority, allowing sites to add
  non-English synonyms or local brand names without modifying the package.

* **`calc_daily_dose_baseline()`, `calc_daily_dose_nlp()`,
  `calc_daily_dose_nlp_advanced()`, `convert_pred_equiv()`** all accept
  `drug_name_map = NULL` and thread it through to `standardize_drug_name()`.

* **`calc_daily_dose_baseline()`, `calc_daily_dose_nlp()`,
  `calc_daily_dose_nlp_advanced()`** accept `equiv_table = NULL`. When
  `filter_oral = TRUE`, the drug allow-list is now derived from the
  user-supplied table rather than always using the built-in 7-steroid table,
  making it straightforward to study additional corticosteroids.

* **`calc_daily_dose_nlp()`** gains `max_daily_dose_mg = 2000`, matching the
  cap already present in the baseline and advanced-NLP functions.

* **SQL template** (`inst/sql/extract_drug_exposure.sql`): vocabulary table
  joins (`concept`, `drug_strength`) now use `@vocab_schema` instead of
  `@cdm_schema`, enabling sites that store vocabulary tables in a separate
  schema to connect without modifying the SQL.

* **`connector.R`**: `vocab_schema` is now passed to the SQL renderer.
  `create_omop_connector(vocab_schema = ...)` already accepted the parameter
  (defaulting to `cdm_schema`); it is now actually used.

* **`evaluate_against_gold()`** gains `dose_breaks` and `dose_labels`
  parameters to customise the dose-range stratification bins. The defaults
  (`c(0, 10, 20, 40, Inf)`) are unchanged.

---

# SteroidDoseR 0.1.5

## Enhancements

* **`calc_daily_dose_baseline()`** now exposes the four intermediate method
  columns in its output — `dose_from_original` (M1), `dose_from_tablets_freq`
  (M2), `dose_from_supply` (M3), and `dose_from_actual_duration` (M4) — using
  the same names as the original Version2 `Baseline.qmd` output. Previously
  these were computed internally and then dropped; they are now retained so
  users can audit which method succeeded record-by-record and reproduce the
  Version2 output format exactly.

---

# SteroidDoseR 0.1.4

## Bug fixes

* **SQL template path fallback** (`connector.R`): `fetch_drug_exposure()` now
  falls back to `file.path(getwd(), "inst", "sql", "extract_drug_exposure.sql")`
  when `system.file()` returns `""` — the common situation when the package is
  loaded with `devtools::load_all()` or the installed copy is stale. The error
  message now includes reinstall instructions. (#BUG-7)

---

# SteroidDoseR 0.1.3

## New functions

* **`plot_patient_episodes(episode_list, patient_ids, ...)`** — static
  `ggplot2` timeline overlaying Baseline, NLP, Advanced NLP, and gold-standard
  episodes as horizontal step segments, faceted by patient × drug. Supports
  `dose_col` choice (median / min / max / mean), optional gold standard, and
  custom colours. Save with `ggplot2::ggsave()`.

* **`launch_dose_dashboard(episode_list, ...)`** — interactive Shiny review
  dashboard. Controls: patient multi-select, drug filter, dose metric selector,
  method toggles, line-width slider, PDF/CSV download. Requires `shiny` and
  `DT` packages.

## Enhancements

* **`build_episodes()`** now returns a `mean_daily_dose` column — the
  duration-weighted mean daily dose within each episode
  (`sum(dose_i × days_i) / sum(days_i)` for non-NA records). Use
  `computed_dose_col = "mean_daily_dose"` in `evaluate_against_gold()` to
  weight longer prescription periods more heavily than short ones.

* **`evaluate_against_gold()`** — `computed_dose_col` can now be set to
  `"min_daily_dose"`, `"max_daily_dose"`, or `"mean_daily_dose"` to change
  the dose metric used for comparison (previously only `"median_daily_dose"`
  was documented). The `episode_list`-style multi-method comparison is
  demonstrated in `tests/run_analysis.R` section 10.

---

# SteroidDoseR 0.1.2

## New functions

* **`calc_daily_dose_nlp_advanced()`** — enhanced NLP pipeline that extends
  `calc_daily_dose_nlp()` with richer vocabulary, taper decomposition, and a
  dose plausibility cap matching the Baseline method.

* **`parse_sig_one_advanced()`** / **`parse_sig_advanced()`** — drop-in
  replacements for `parse_sig_one()` / `parse_sig()` with the expanded
  vocabulary below. Return identical columns for full compatibility.

* **`parse_taper_schedule()`** — standalone taper decomposer. Given a SIG
  string, returns a tibble of taper steps with `daily_dose_mg`,
  `freq_per_day`, `duration_days`, `step_start_day`, and `step_end_day`.
  Returns `NULL` when the SIG cannot be decomposed into ≥ 2 steps.

## Enhanced SIG vocabulary (applies to all `*_advanced` functions)

* **Word-form tablet counts**: `"one tablet"`, `"two tablets"`, `"half a
  tablet"`, `"a tablet"`, and `"N and a half tablets"` compounds.
* **Fractional tablets**: `"1/2 tablet"`, `"½ tab"`.
* **Weekly frequencies**: `"once weekly"`, `"twice a week"`,
  `"3 times a week"`, `"q7d"`, `"every 7 days"`.
* **Monthly frequencies**: `"monthly"`, `"once a month"`, `"q30d"`.
* **Generalised every-N-days**: `"every 3 days"`, `"q48h"` (= 0.5/day),
  `"q72h"` (= 1/3 day), `"every 4 days"`.
* **Sub-daily every-N-hours**: `"every 4 hours"` / `"q4h"` (= 6/day),
  alongside the existing `q6h`, `q8h`, `q12h`.

## Taper decomposition

* `calc_daily_dose_nlp_advanced(expand_tapers = TRUE)` expands taper records
  into per-step rows. Each expanded row carries `taper_step` (step number),
  `step_start_day`, and `step_end_day` (days from prescription start).
  `parsed_status` is set to `"taper_ok"` for successfully decomposed steps.
  Two patterns are supported:
  - *Explicit multi-step*: `"60 mg daily for 4 weeks, then 40 mg daily for
    4 weeks, then 20 mg daily for 4 weeks"` — split on `"then"`, commas, or
    semicolons; each piece parsed independently.
  - *Decrement*: `"start 60 mg then decrease by 10 mg every week"` —
    generates descending dose steps down to the decrement amount.

## New parameter

* **`max_daily_dose_mg`** added to `calc_daily_dose_nlp_advanced()` (default
  2000 mg/day). Records above this threshold are set to `NA` with status
  `"implausible"` and a diagnostic warning — identical behaviour to the
  Baseline method.

---

# SteroidDoseR 0.1.1

## New parameters

* **`filter_oral`** added to `calc_daily_dose_baseline()` (default `FALSE`).
  When set to `TRUE`, restricts to oral-route records and to drugs present in
  the prednisone-equivalency table before imputing — identical to the behaviour
  of `calc_daily_dose_nlp(filter_oral = TRUE)`. Default `FALSE` preserves
  backward compatibility for callers that pre-filter upstream.

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
