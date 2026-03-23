# SteroidDoseR 0.2.1

## Enhancements

* **NLP/Advanced NLP — baseline cascade fallback** (`nlp.R`, `nlp_advanced.R`):
  After SIG parsing, records still with `daily_dose_mg = NA` (status `"no_parse"` or
  `"empty"`) are now passed through the Baseline M1/M3/M4 cascade
  (`original`, `actual_duration`, `supply_based`). This ensures NLP coverage is
  always ≥ Baseline — anything Baseline can calculate, NLP now also returns.
  Imputation label is set to `"fallback_<method>"` in `parsed_status`.

* **`parse_sig_one()` / `parse_sig_one_advanced()` — new `.preprocess_sig()` helper**
  (`nlp.R`, `nlp_advanced.R`):
  Applied before pattern matching to (1) translate Spanish number words
  (`uno`→`1`, `dos`→`2`, `tres`→`3`, `cuatro`→`4`, `cinco`→`5`, `seis`→`6`,
  `siete`→`7`, `diez`→`10`) and tablet synonyms (`tabletas`→`tablet`,
  `diario`→`daily`), and (2) strip pure-word parentheticals such as `(twelve)`
  that were blocking numeric patterns while preserving dose parentheticals like
  `(10 mg per dose)`.

* **`parse_sig_one()` / `parse_sig_one_advanced()` — additional frequency patterns**
  (`nlp.R`, `nlp_advanced.R`):
  Four new patterns now return `freq_per_day = 1`:
  - `"in am"` / `"in the morning"` / `"each morning"` — morning-dose SIGs
  - `"once for N dose(s)"` — single-dispense records
  - Bare `"X mg."` strings with no frequency keyword
  - `"by mouth"` / `"po"` / `"orally"` with no time qualifier (hours/before/after/procedure)

# SteroidDoseR 0.2.0

## Enhancements

* **`classify_route()` — `drug_source_value` used as route fallback** (`utils-validate.R`):
  When both `route_concept_name` and `route_source_value` are absent or NA, the function
  now falls back to `drug_source_value` to infer route. Many EHR systems encode route
  in the drug name string (e.g. `"METHYLPREDNISOLONE 125MG/2ML IV SOL"` or
  `"PREDNISONE 5MG ORAL TAB"`). All three imputation functions pass `drug_source_value`
  to `classify_route()` so injections with no route columns are correctly excluded.

* **`classify_route()` — injection pattern now matches "intravenous"** (`utils-validate.R`):
  The previous pattern `intravein` did not match the string `"intravenous"` (the most
  common form in EHR drug name strings). Replaced with `intraven` which correctly matches
  `"intravenous"`, `"intravenously"`, etc. Added `infusion`, `\\bsq\\b`, and `\\binjec\\b`
  to cover additional common source value patterns.

* **`classify_route()` — oral pattern now matches `\\btab\\b`** (`utils-validate.R`):
  Shortened form `"TAB"` (common in drug_source_value strings like `"PREDNISONE 5MG ORAL TAB"`)
  is now recognised as oral route.

# SteroidDoseR 0.1.9

## Enhancements

* **`parse_sig_one()` / `parse_sig_one_advanced()` — new once-daily SIG patterns** (`nlp.R`, `nlp_advanced.R`):
  Three common real-world SIG strings that previously returned `freq_per_day = NA` now
  parse correctly as `freq_per_day = 1`: `"Once Oral"`, `"Every evening Oral"`, and
  `"Nightly Oral"` (leading whitespace is normalised before matching).

* **`parse_sig_one()` / `parse_sig_one_advanced()` — `tablets` defaults to 1** (`nlp.R`, `nlp_advanced.R`):
  When a SIG string is parseable (freq detected, mg detected) but contains no explicit
  tablet count, `tablets` is set to 1 rather than `NA`. This allows baseline M2
  (`tablets × freq × strength`) to complete for SIG strings such as `"5 mg daily"` or
  `"Once Oral"` where the prescriber omitted the tablet count.

# SteroidDoseR 0.1.8

## Bug fixes

* **`calc_daily_dose_baseline()` — `filter_oral` default changed to `TRUE`** (`baseline.R`):
  Previously defaulted to `FALSE`, causing baseline to return all drug routes
  (injectables, inhalationals, topicals) alongside oral tablets. Now matches the
  NLP method default. Pass `filter_oral = FALSE` only when the input is already
  pre-filtered to oral corticosteroids.

* **`calc_daily_dose_baseline()` / `run_pipeline()` — `m2_sig_parse` default changed to `"auto"`** (`baseline.R`, `pipeline.R`):
  Previously defaulted to `"warn"`, which silently skipped SIG-based M2 for all
  real-world OMOP data where `tablets` and `freq_per_day` columns are absent (the
  common case). M2 now automatically parses the `sig` column when present before
  falling through to the M3 Burkard formula or M4 supply-based fallback.

* **`.resolve_drug_df()` — `sig_source` now applied on the data-frame path** (`connector.R`):
  When `connector_or_df` is a plain data frame, `.resolve_drug_df()` previously
  returned it immediately without calling `.apply_sig_source()`, silently ignoring
  `sig_source = "drug_source_value"`. The alias from `drug_source_value` into `sig`
  is now applied consistently on all three input paths (data frame, df_connector,
  omop_connector).

* **`calc_daily_dose_baseline()` — M2 SIG-parse guard checks usable values, not column existence** (`baseline.R`):
  The guard that triggers `parse_sig()` previously checked whether `tablets` and
  `freq_per_day` *columns existed*. A column that exists but is entirely NA (common
  in CDM extracts and any caller that pre-initialises these columns) caused the guard
  to evaluate FALSE, silently suppressing auto-parsing even with `m2_sig_parse = "auto"`.
  The guard now uses value-aware flags (`any(!is.na(...))`) so all-NA columns are
  treated identically to absent columns. Existing all-NA columns are also dropped
  before `parse_sig()` is called to prevent a `bind_cols` duplicate-column crash.

# SteroidDoseR 0.1.7

## Changes

* **`calc_daily_dose_baseline()` — Burkard formula cascade reorder** (`baseline.R`):
  The default `methods` cascade now places `"actual_duration"` before
  `"supply_based"`, changing the default from
  `c("original", "tablets_freq", "supply_based", "actual_duration")` to
  `c("original", "tablets_freq", "actual_duration", "supply_based")`.
  For oral tablet formulations the OHDSI-standard Burkard (2024) formula is
  `(amount_value × quantity) / (end_date − start_date + 1)`, which corresponds
  to `"actual_duration"`. Using `days_supply` as the denominator (`"supply_based"`)
  is now a fallback only, applied when date range is unavailable.

# SteroidDoseR 0.1.6

## Bug fixes

* **`calc_daily_dose_baseline()` — concept-0 unit sentinel** (`baseline.R`):
  `amount_unit_concept_id = 0` (the OMOP "No matching concept" sentinel used
  by many production CDMs in place of NULL) is now treated identically to `NA`
  (unknown unit) when deciding whether to accept `amount_value` as milligrams.
  Previously, concept 0 was rejected as a non-mg unit, silently setting
  `strength_mg = NA` for every record at affected sites and causing all
  imputation methods to return `"missing"`. (#BUG-8)

* **`calc_daily_dose_baseline()` — diagnostic warning** (`baseline.R`):
  When `strength_mg` is NA for every row after the `amount_value` + string
  fallback steps, a descriptive warning now reports the count of usable
  `amount_value` rows and `drug_source_value` mg matches so the root cause
  can be identified quickly.

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
