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

### DECISION-4 `amount_value` missing from SQL extraction ⏳ AWAITING DECISION
`extract_drug_exposure.sql` does not join `drug_strength`, so `amount_value`
is never populated via the OMOP connector path. `strength_mg` always falls back
to regex-parsing `drug_source_value`.

Options:
- A: Add `LEFT JOIN drug_strength` to SQL → reliable, standard OMOP
- B: Keep string fallback only → simpler, fragile for non-standard source values
- C: Both — join first, fall back to string if NULL

### DECISION-5 M2 requires NLP output that baseline never produces ⏳ AWAITING DECISION
M2 needs `tablets` and `freq_per_day`, which only exist after `parse_sig()`.
`calc_daily_dose_baseline()` never calls the NLP parser, so M2 is effectively
dead in the standard workflow.

Options:
- A: Call `parse_sig()` internally when `sig` is present and columns are absent
- B: Keep separate; warn user when `sig` present but `tablets`/`freq_per_day` missing
- C: In `run_pipeline(method="baseline")`, run NLP first to populate those columns

### DECISION-6 `drug_exposure_end_date` is a hard requirement ⏳ AWAITING DECISION
`assert_required_cols` errors if `drug_exposure_end_date` is absent entirely,
even though only M4 needs it. Many minimal data frames won't have it.

Options:
- A: Make optional — treat as all-NA if absent, only M4 affected
- B: Keep as required — acceptable if all target sites populate the column