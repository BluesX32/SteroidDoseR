# Known Bugs & Decisions Needed

## baseline.R

### BUG-1 `strict_legacy` is a dead parameter ✅ FIXED
`strict_legacy` is accepted but never referenced in the function body. Removed.

### BUG-2 `methods` controls inclusion but not ordering ✅ FIXED
Docs say "ordered list of methods to attempt" but the cascade in step 5 is
hardcoded `m1 → m2 → m3 → m4` regardless of the order in `methods`.
Fixed: cascade now follows the user-supplied `methods` order.

### BUG-3 M1 column name mismatch with Version2 ✅ FIXED
Version2 uses `daily_dose_mg`; package looks for `daily_dose`. M1 silently
never fires when data comes from the V2 pipeline.
Fixed: accepts both `daily_dose` and `daily_dose_mg` (prefers `daily_dose`).

### DECISION-4 `amount_value` missing from SQL extraction ✅ FIXED (Option C)
Added a `LEFT JOIN` to `drug_strength` via an aggregating subquery
(`GROUP BY drug_concept_id`, `MAX(amount_value)`) that avoids duplicate rows
from combination drugs. `amount_value` and `amount_unit_concept_id` are now
selected. `baseline.R` already uses `coalesce(amount_value, str_from_source)`,
so the string fallback remains active when `drug_strength` has no match.

### DECISION-5 M2 requires NLP output that baseline never produces ✅ FIXED
M2 needs `tablets` and `freq_per_day`, which only exist after `parse_sig()`.
`calc_daily_dose_baseline()` never calls the NLP parser, so M2 was effectively
dead in the standard workflow.

Fixed: added `m2_sig_parse` parameter to both `calc_daily_dose_baseline()` and
`run_pipeline()`. All three options are available:
- `"warn"` (default) — warn and skip M2 when columns are absent
- `"auto"` — call `parse_sig()` internally when `sig` is present (Option A)
- `"nlp_first"` in `run_pipeline()` — run full NLP pass before baseline (Option C)
- `"none"` — silently skip M2

### DECISION-6 `drug_exposure_end_date` is a hard requirement ✅ FIXED (Option A)
`assert_required_cols` errored if `drug_exposure_end_date` was absent, even
though only M4 needs it. Many minimal data frames don't have it.

Fixed: column is now optional. When absent, `Sys.Date()` is substituted for
every row and a one-time warning is issued. M1–M3 are unaffected; M4 produces
a rough upper-bound duration estimate (today − start_date + 1).

### BUG-6 Baseline doses up to 32 billion mg/day due to unit mismatch and no cap ✅ FIXED
`drug_strength.amount_value` was used blindly as mg regardless of
`amount_unit_concept_id`. Records where the unit is mcg (9655), g (8504), or
any non-mg concept are treated as mg, making M3/M4 produce astronomical values
(observed max: 32,000,000,000 mg/day; mean ~776,000 mg/day vs median 40 mg/day).

**Fix 1**: `amount_value` is now only accepted when `amount_unit_concept_id`
is mg (OMOP concept 8576) or absent/unknown. Other units fall through to the
`drug_source_value` string-extraction fallback.

**Fix 2**: Added `max_daily_dose_mg` parameter (default 2000 mg/day). Records
exceeding this threshold are set to NA with a warning, protecting episode-level
summaries from residual data-quality outliers. Pass `NULL` to disable the cap.

### BUG-5 NLP method produces no "ok" records when SIG has no mg ✅ FIXED
In live OMOP data the `sig` field typically contains administration instructions
("take 1 tablet daily") without a dose amount; the mg strength lives in
`drug_strength.amount_value` and `drug_concept_name`.  The NLP parser only
looked at the SIG string, so `mg_per_admin` was always NA → `daily_dose_mg` NA
→ every parseable record landed in `no_parse`.

Fixed in `calc_daily_dose_nlp()`: after `parse_sig()`, records with
`parsed_status == "no_parse"` and `mg_per_admin == NA` receive a strength
fallback — `amount_value` first, then mg extracted from `drug_concept_name` /
`drug_source_value`.  When the fallback provides a strength and `freq_per_day`
was already parsed from the SIG, `daily_dose_mg` is computed and
`parsed_status` is updated to `"ok"`.

## connector / SQL

### route_concept_name missing from SQL extraction ✅ FIXED
`extract_drug_exposure.sql` fetched `route_concept_id` but not
`route_concept_name`. `calc_daily_dose_nlp()` checks for `route_concept_name`
and `route_source_value`; neither was present, so the oral-route filter was
silently skipped with a warning.

Fixed: added `LEFT JOIN concept rc ON de.route_concept_id = rc.concept_id`
and selected `rc.concept_name AS route_concept_name`.
