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

### BUG-9 `classify_route()` missed "intravenous" + ignores `drug_source_value` ✅ FIXED

Two separate failures meant injection records could slip through `filter_oral`:

1. **Pattern bug**: the injection regex contained `intravein`, which does *not*
   match the string `"intravenous"` (the most common form in EHR drug names).
   The correct prefix is `intraven` (which is a substring of "intravenous").
2. **Missing column**: `drug_source_value` was never passed to `classify_route()`.
   Many EHR systems encode route only in the drug name string (e.g.
   `"METHYLPREDNISOLONE 125MG/2ML IV SOL"` or `"DEXAMETHASONE INJECTION"`).
   Records with no `route_concept_name` and no `route_source_value` fell through
   as `"other"` (not `"injection"`) and were retained.

Fixed in v0.2.0: `classify_route()` accepts a third `drug_source` argument;
all three imputation functions now pass `drug_source_value` as a coalesce
fallback. Regex updated: `intravein` → `intraven`; added `infusion`, `\\bsq\\b`,
`\\binjec\\b`; oral pattern extended with `\\btab\\b`.

### BUG-8 `amount_unit_concept_id = 0` rejected as non-mg unit ✅ FIXED
Many production CDMs store `0` (the OMOP "no matching concept" sentinel) instead
of `NULL` when the drug strength unit has not been mapped. The amount_value guard
previously only accepted concept 8576 (mg) or `NA`; concept 0 fell through to the
string-extraction fallback, silently setting `strength_mg = NA` for every record
at such sites and causing all imputation methods to return `"missing"`.

Fixed in v0.1.6: concept ID `0` is now treated identically to `NA` (unknown unit)
and `amount_value` is accepted as milligrams in that case, matching the behaviour
sites that have not mapped the unit concept.

### BUG-10 NLP structural fallback skipped prn/taper/free_text records ✅ FIXED

`calc_daily_dose_nlp()` and `calc_daily_dose_nlp_advanced()` ran the structural
baseline fallback (M1/M3/M4) only for records with `parsed_status %in%
c("no_parse", "empty")`. Records flagged as `"prn"`, `"taper"`, or
`"free_text"` where SIG parsing could not yield a `daily_dose_mg` were excluded
from the fallback and left as `NA`.

Baseline always continues through the M3/M4 cascade regardless of PRN/taper
status (it uses quantity × strength / days_supply or actual_duration), so NLP
was producing systematically lower coverage than baseline for those categories.

Fixed: `still_na <- is.na(result$daily_dose_mg)` — the structural fallback now
fires for any record whose dose is still unresolved after SIG parsing, regardless
of parsed_status. When baseline also fails, the original NLP status (e.g., `"prn"`,
`"taper"`) is preserved so callers know which category was unresolved.

### BUG-11 NLP strength fallback used `amount_value` without unit check ✅ FIXED

The strength fallback in `calc_daily_dose_nlp()` and `calc_daily_dose_nlp_advanced()`
called `safe_as_numeric(result$amount_value)` without verifying
`amount_unit_concept_id == 8576`. Baseline has a unit guard that discards
microgram (9655) and gram (8504) values. Without the guard, NLP could treat
a mcg-strength value as mg, producing a 1000× dose error.

Fixed: same unit guard as baseline — if `amount_unit_concept_id` is present and
is not 8576 (mg), `amount_value` is set to `NA` and the string-extraction fallback
is used instead.

## connector / SQL

### BUG-7 SQL template not found when package is loaded via devtools::load_all() or stale install ✅ FIXED
`system.file("sql", "extract_drug_exposure.sql", package = "SteroidDoseR")`
returns `""` when (a) the package was loaded with `devtools::load_all()` instead
of installed, or (b) the installed copy at `C:/Program Files/RPackages/` is
outdated and lacks the `inst/sql/` directory.

**Fix**: Added a fallback path check in `.fetch_drug_exposure_omop()`. After
`system.file()` is called, if the result is empty or the file does not exist,
the code checks `file.path(getwd(), "inst", "sql", "extract_drug_exposure.sql")`.
This covers the source-package / `load_all()` workflow where the working
directory is the package root. The error message now also includes actionable
reinstall instructions.

### route_concept_name missing from SQL extraction ✅ FIXED
`extract_drug_exposure.sql` fetched `route_concept_id` but not
`route_concept_name`. `calc_daily_dose_nlp()` checks for `route_concept_name`
and `route_source_value`; neither was present, so the oral-route filter was
silently skipped with a warning.

Fixed: added `LEFT JOIN concept rc ON de.route_concept_id = rc.concept_id`
and selected `rc.concept_name AS route_concept_name`.

## analysis scripts

### BUG-12 Primary comparison labels native-drug mg as prednisone-equivalent ⚠️ OPEN

`CodeToRun.R` builds Baseline and NLP episodes directly from
`daily_dose_mg_imputed` / `daily_dose_mg`, and `LLMtoRun.R` similarly builds
episodes from extracted native-drug mg. `CompareToRun.R` then labels these
values as mg prednisone-equivalent and compares them with the gold column
`dose_daily_mg_equiv`. The explicit `convert_pred_equiv()` step is used only in
the NLP equivalency sensitivity analysis.

Until fixed, primary results are valid in a common unit only when all retained
records are already prednisone/prednisolone or were converted upstream. The
recommended correction is to call `convert_pred_equiv()` on every method's
record-level output before `build_episodes()` and to verify that the gold
standard is truly prednisone-equivalent rather than only normalized to mg/day.

### DECISION-13 Episode-level and cross-sectional evaluation are intentionally separate ✅ FIXED

The PI asked how time windows work across the three methods, having noticed
episodes in `comparison_baseline.csv` spanning multiple years. That's expected:
`build_episodes()` gap-bridges a patient's prescriptions for one drug into a
continuous episode whenever consecutive records are ≤`gap_days` apart, so
continuous refilling produces one long episode. `evaluate_against_gold()` then
compares one dose value per *gold* episode (also variable-length), not per
fixed calendar window. There was no shorter, encounter-level granularity
anywhere in the codebase.

**Fix**: added a second, independent evaluation path answering "on this
specific office-visit date, what dose does each method say the patient was
on" — `fetch_visit_occurrence()` (new `VISIT_OCCURRENCE` query),
`dose_at_visits()` (point-in-time episode lookup, `R/episodes.R`), and
`dose_agreement_metrics()` (episode-level metric formulas applied to plain
dose vectors, `R/eval.R`), wired into `CodeToRun.R` (STEP 2c) and
`CompareToRun.R` (section 11b). This does **not** replace the episode-level
comparison — the two answer different questions and are both retained. See
`docs/pipeline.html` for the user-facing explanation.

### DECISION-14 Cumulative/trajectory steroid exposure has no gold standard ⚠️ OPEN (by design, not a bug)

The PI also noted that steroid *trajectory* (dose over time, or cumulative
exposure) is clinically important but "much harder to determine" and has no
gold standard to validate against — the manually reviewed gold standard only
covers discrete chart-reviewed intervals (`episode_start`/`episode_end` +
one dose), not a continuous dose curve. Cross-sectional evaluation
(DECISION-13) validates individual point-in-time doses, which is a necessary
building block, but does not itself validate a trajectory or cumulative-dose
metric. No gold standard currently exists to make that possible, and building
one would require new chart-review effort outside this package's scope. Not
tracked as a bug to fix — recorded so it stops resurfacing as ambiguity.

## discovered during cross-sectional evaluation work (not fixed — out of scope)

### BUG-13 `MakeSyntheticData.R` gold-standard tribble uses `patient_id`, checked-in CSV uses `person_id` ⚠️ OPEN

`extras/MakeSyntheticData.R`'s `synthetic_gold_standard` tribble is defined
with `~patient_id` as its first column, but the committed
`inst/extdata/synthetic_gold_standard.csv` has `person_id` in that position.
Re-running the generator script (e.g. `Rscript extras/MakeSyntheticData.R`)
reproduces this drift and silently breaks any downstream code that expects
`person_id`. Discovered while regenerating synthetic fixtures for
`synthetic_visit_occurrence.csv` — not fixed here since it touches a shared
gold-standard test fixture and the intended column name should be confirmed
first.

### BUG-14 `CompareToRun.R`'s gold-standard loader doesn't match the bundled synthetic gold CSV ⚠️ OPEN

`CompareToRun.R` loads `GOLD_STD_PATH` and always calls
`parse_steroid_gold(gold_std_raw)` with its default column names
(`myositis_omop_person_id`, `dmardname`, `dmarddose`, ...) — the raw
chart-review export schema. `inst/extdata/synthetic_gold_standard.csv` uses
the already-parsed schema (`person_id`, `episode_start`, `episode_end`,
`median_daily_dose`) instead, so `USE_SYNTHETIC = TRUE` cannot currently run
`CompareToRun.R` end-to-end against the bundled synthetic gold standard
without either overriding `parse_steroid_gold()`'s column arguments or
skipping it for a direct read. Discovered while manually verifying the new
cross-sectional comparison end-to-end (worked around locally by reading the
CSV directly); not fixed here since it's pre-existing and orthogonal to this
change.

### BUG-15 NAMESPACE was missing exports for functions marked `@export` in source ⚠️ OPEN

Regenerating documentation with `devtools::document()` revealed that
`build_cohort_sql()`, `fetch_cohort_ids()` (`R/cohort.R`), and
`make_validation_split()` (already present as a bullet in `docs/reference.html`
but missing from the committed `NAMESPACE`) are tagged `@export` in source but
were absent from the checked-in `NAMESPACE`, meaning `library(SteroidDoseR)`
users cannot currently call them without `:::`. Also several `importFrom`
entries (`dplyr::slice_max`, `stringr::str_remove`, `str_remove_all`,
`str_trim`) are used in source but missing from `NAMESPACE`. Not fixed here to
keep this change's diff scoped to the cross-sectional feature — a full
`devtools::document()` pass touches ~15 unrelated files. Worth a dedicated
`docs`/`chore` commit.
