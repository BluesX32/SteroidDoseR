# CLAUDE.md — SteroidDoseR Development Instructions

This file defines mandatory standard operating procedures for all code changes
to the SteroidDoseR package. Follow these instructions exactly after every
modification, regardless of scope.

---

## Standard procedure after every change

### 1. Update documentation

**README.md** — update any section affected by the change:
- Key features bullet if a capability was added or fixed
- Function reference table row if parameters or behaviour changed
- Minimum required columns table if column requirements changed
- Troubleshooting section if a new failure mode or fix is relevant

**docs/** — multi-page HTML manual. Mirror all README updates plus:
- `docs/index.html` — overview, installation, quick start
- `docs/connectors.html` — connector types, live OMOP database, capability detection, column contract
- `docs/methods.html` — Baseline (M1–M4), NLP, Advanced NLP, taper parser, SIG vocabulary
- `docs/pipeline.html` — prednisone equivalency, episode building, run_pipeline(), evaluation
- `docs/reference.html` — function reference table, data quality issues, troubleshooting, changelog
- Add or update the parameter table row (`<tr>`) in the relevant function section
- Apply the correct badge: `<span class="badge badge-new">v0.x.x</span>` for new
  features, `<span class="badge badge-fix">v0.x.x</span>` for bug fixes
- Update the Changelog section in `docs/reference.html`
- `docs/manual.html` redirects to `docs/index.html` via meta-refresh (do not edit)

**NEWS.md** — add a bullet under the current version section describing what
changed and why (one bullet per logical change).

**bug.md** — if the change fixes a tracked bug, mark it `✅ FIXED` and append
a short explanation of the fix. If the change introduces a new known issue or
design decision, add a new entry.

### 2. Commit the change

Use a concise, accurate commit message following this format:

```
<type>(<scope>): <short description>

<optional body — one paragraph explaining why, not what>
```

**Types:** `fix`, `feat`, `docs`, `refactor`, `test`, `chore`
**Scope:** the R source file or subsystem (e.g. `baseline`, `nlp`, `connector`, `episodes`, `eval`, `run_analysis`)

Examples:
```
fix(baseline): add unit-safe amount_value check and dose plausibility cap
feat(baseline): add filter_oral parameter matching NLP method
fix(nlp): add amount_value strength fallback for SIGs without mg
docs: update README, manual.html, NEWS for v0.1.1 bug fixes
```

Stage only the files relevant to the change. Do not stage unrelated files.

---

## Package structure

```
R/
  baseline.R      — calc_daily_dose_baseline() — M1–M4 cascading imputation
  nlp.R           — calc_daily_dose_nlp(), parse_sig(), parse_sig_one()
  nlp_advanced.R  — calc_daily_dose_nlp_advanced(), parse_sig_advanced(),
                    parse_sig_one_advanced(), parse_taper_schedule()
  connector.R     — create_omop_connector(), create_df_connector(), etc.
  connection.R    — create_omop_connection(), create_connection_from_env()
  conversion.R    — convert_pred_equiv()
  episodes.R      — build_episodes()
  eval.R          — evaluate_against_gold()
  pipeline.R      — run_pipeline(), drug_df_contract docs
  sql_helpers.R   — render_translate_sql(), query_omop() (internal)
  utils-validate.R — assert_required_cols(), safe_as_date(), safe_as_numeric()

inst/sql/
  extract_drug_exposure.sql — parameterised OMOP query

tests/
  run_analysis.R            — live/synthetic end-to-end analysis script
  testthat/                 — 112 unit tests across 5 files

docs/
  manual.html               — self-contained HTML package manual
  reconstruction-manual.md  — how this package was built from legacy notebooks

vignettes/
  baseline-workflow.Rmd
  nlp-workflow.Rmd
  connector-workflow.Rmd
  evaluation-workflow.Rmd
```

---

## Key design rules

- **Never throw errors** — malformed input returns NA with a warning, not a stop().
  `parse_sig_one()` is the canonical example: wrap `.parse_sig_one_impl()` in
  `tryCatch` and return `.empty_parse_row()` on failure.
- **Oral filter defaults to TRUE** — `filter_oral = TRUE` is the default for all
  three imputation functions (baseline, NLP, advanced NLP). Pass `filter_oral = FALSE`
  only when the input is already pre-filtered to oral corticosteroids.
- **Unit-safe `amount_value`** — always check `amount_unit_concept_id == 8576`
  (mg) before treating `amount_value` as milligrams. Other units (mcg = 9655,
  g = 8504) must be discarded and the `drug_source_value` string fallback used.
- **Plausibility cap** — `max_daily_dose_mg = 2000` protects downstream
  summaries from data-quality outliers. The cap warns; it does not silently drop.
- **Strength fallback order** (NLP): SIG mg → `amount_value` (unit-checked) →
  mg from `drug_concept_name` → mg from `drug_source_value`.
- **Connector-first** — all public imputation functions accept either a
  `steroid_connector` or a plain data frame as the first argument via
  `.resolve_drug_df()`. Connector-path arguments are silently ignored on the
  data-frame path.
- **Internal helpers are unexported** — use `@noRd` and do not list in
  NAMESPACE. Only the five core pipeline functions plus connectors/utilities
  are exported.

---

## OMOP concept IDs used in code

| Concept | ID | Notes |
|---|---|---|
| milligram (mg) | 8576 | Only accepted unit for `amount_value` |
| microgram (mcg) | 9655 | Discarded — would be misread as mg |
| gram (g) | 8504 | Discarded — would be misread as mg |

---

## Tracked bugs

See `bug.md` for the full list. All entries through BUG-6 are resolved.

---

## Citation

Xiong C et al. *AgentDose: Automated Steroid Dose Calculation from EHR Data.*
OHDSI Global Symposium 2025. Abstract 205.
