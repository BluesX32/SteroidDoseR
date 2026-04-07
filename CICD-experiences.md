# CI/CD Experiences — SteroidDoseR

Accumulated lessons from setting up and debugging `R CMD check` on GitHub
Actions. Each entry has: what triggered it, why it happens, and the fix.

---

## 1. Interactive script in `tests/` crashes R CMD check

**Symptom**

```
Error in library(devtools) : there is no package called 'devtools'
```

**Root cause**

R CMD check runs **every `.R` file** found directly in `tests/` via
`R CMD BATCH` (non-interactively). It is not limited to `testthat.R`.
If any of those files calls packages not declared in `DESCRIPTION` (or makes
filesystem assumptions valid only on the developer's machine), the build fails.

In our case `tests/run_analysis.R` line 33 had:

```r
devtools::install_local("./SteroidDoseR/", force = TRUE)
```

That path does not exist inside the CI workspace, and `devtools` was not in
`Suggests`.

**Fix**

Add an early exit guard at the very top of any interactive-only script:

```r
if (!interactive()) quit(status = 0L, save = "no")
```

This makes `R CMD BATCH` exit cleanly (status 0 = no error) before any
interactive code runs.

**Also exclude the file from the built package** with `.Rbuildignore` if it
is a dev/analysis script rather than a formal test.

---

## 2. R CMD check static-scans `tests/` even past an early `quit()`

**Symptom**

```
Warning: 'scales' namespace required but not declared in DESCRIPTION
```

even though `tests/run_analysis.R` starts with
`if (!interactive()) quit(status = 0L, save = "no")`.

**Root cause**

R CMD check's dependency scanner reads all `::` calls in files under `tests/`
**statically** (text parsing), without executing the code. An early `quit()`
does not protect `scales::label_number()` on line 457 from being seen by the
scanner.

**Fix**

Declare every package referenced anywhere in `tests/` (even in dead code
paths) in `DESCRIPTION` under `Suggests`:

```
Suggests:
    scales
```

---

## 3. Spurious warnings from testthat tests: "M2 skipped"

**Symptom** (testthat output / CI log)

```
Warning: M2 (tablets_freq) skipped: `tablets` and `freq_per_day` are absent
but a `sig` column is present.
```

Seven of these appeared, from connector-dispatch tests that did not test M2
behaviour at all.

**Root cause**

`calc_daily_dose_baseline()` previously defaulted to `m2_sig_parse = "warn"` (now
changed to `"auto"` in v0.1.8). When the test data has a `sig` column but no
pre-parsed `tablets`/`freq_per_day` columns, the old default emitted a warning on
every call — including calls whose only purpose is to verify the connector dispatch
path.

**Fix**

Pass `m2_sig_parse = "none"` in all tests that do not exercise M2:

```r
out <- calc_daily_dose_baseline(con, m2_sig_parse = "none")
run_pipeline(con, method = "baseline", m2_sig_parse = "none")
```

**General lesson**: always silence optional warnings in tests that do not
test that specific behaviour. Use the parameter designed for that purpose
(`"none"` / `"quiet"` / `suppress_warnings()`) rather than wrapping in
`suppressWarnings()` globally, so real warnings in other tests still surface.

---

## 4. Non-ASCII characters in R source files

**Symptom**

```
Warning: found non-ASCII characters in R/baseline.R
Warning: found non-ASCII characters in R/viz.R
```

(R CMD check lists every affected file; in practice there were 9 R files and
10 Rd files.)

**Root cause**

Copy-pasting from documentation, word processors, or web pages introduces
Unicode characters that R CMD check flags:

| Character | Unicode | Common source |
|-----------|---------|---------------|
| `—` (em dash) | U+2014 | Word / Markdown / copy-paste |
| `–` (en dash) | U+2013 | Same |
| `×` (multiplication) | U+00D7 | Mathematical notation |
| `≤` (less-or-equal) | U+2264 | Mathematical notation |
| `→` (right arrow) | U+2192 | Diagrams, code comments |
| `…` (ellipsis) | U+2026 | Prose |
| `•` (bullet) | U+2022 | Bulleted comment blocks |
| `½` (one-half) | U+00BD | Fraction |

**Fix**

Replace with ASCII equivalents:

| Original | Replacement |
|----------|-------------|
| `—` / `–` | `--` / `-` |
| `×` | `*` |
| `≤` | `<=` |
| `→` | `->` |
| `…` | `...` |
| `•` | `-` |
| `½` | `1/2` |

Find all occurrences before committing:

```bash
grep -Prn "[^\x00-\x7F]" R/ man/
```

Non-ASCII is also forbidden in `.Rd` files — check `man/` too.

**Prevention**: add the grep command to a pre-commit hook or lint step.

---

## 5. Undeclared package dependencies (rJava, scales)

**Symptom**

```
Warning: 'rJava' namespace required but not declared in DESCRIPTION
```

**Root cause**

Any `package::function()` call or `requireNamespace("package")` in **any**
file under `R/` or `tests/` (including test helpers) triggers this check.
The package must appear in `Imports` or `Suggests` even if it is only
used conditionally (`if (requireNamespace(...)) { ... }`).

**Fix**

Add the package to `Suggests` (for optional / test-only dependencies):

```
Suggests:
    rJava,
    scales
```

Use `Imports` only for packages that are unconditionally required at runtime.

---

## 6. Undocumented exported functions

**Symptom**

```
Warning: undocumented objects:
  'calc_daily_dose_nlp_advanced' 'launch_dose_dashboard'
  'parse_sig_advanced' 'parse_sig_one_advanced'
  'parse_taper_schedule' 'plot_patient_episodes'
```

**Root cause**

Every function listed in `NAMESPACE` (i.e., tagged `@export` in roxygen)
must have a matching `man/*.Rd` file with an `\alias{}` that equals the
function name. Functions added without a roxygen block, or with `@noRd`
accidentally kept, are exported but undocumented.

**Fix**

Either:

- Write a proper roxygen block above the function and run `devtools::document()`
- Or, if you cannot regenerate Rd files automatically, write a minimal Rd
  file by hand:

```rd
\name{my_function}
\alias{my_function}
\title{Short title}
\usage{my_function(arg1, arg2 = NULL)}
\arguments{
  \item{arg1}{Description.}
  \item{arg2}{Description. Default: \code{NULL}.}
}
\value{Return value description.}
\description{One-paragraph description.}
```

**Prevention**: run `devtools::check()` locally before every PR. New
`@export` functions without Rd files will fail the check immediately.

---

## 7. Codoc mismatches: Rd usage does not match function signature

**Symptom**

```
Warning: Codoc mismatches from documentation object 'calc_daily_dose_baseline':
  Functions or methods with usage in documentation object
  'calc_daily_dose_baseline' but not in code:
    calc_daily_dose_baseline(connector_or_df, methods, ...)

  Functions or methods in code not in documentation object
  'calc_daily_dose_baseline':
    calc_daily_dose_baseline(connector_or_df, methods, ...,
      max_daily_dose_mg, filter_oral, equiv_table, drug_name_map)
```

**Root cause**

R CMD check compares the `\usage{}` block in each `.Rd` file against the
actual function signature in the installed package. When new parameters are
added to a function (e.g., `max_daily_dose_mg`, `filter_oral`) but the
corresponding Rd file is not updated, a "codoc mismatch" warning fires.

This happens when:
- Rd files are hand-edited and then the function signature changes
- `devtools::document()` is not re-run after adding parameters

**Affected files in this project** (v0.1.6 additions):

| Rd file | Missing parameters |
|---------|--------------------|
| `calc_daily_dose_baseline.Rd` | `max_daily_dose_mg`, `filter_oral`, `equiv_table`, `drug_name_map` |
| `calc_daily_dose_nlp.Rd` | `max_daily_dose_mg`, `equiv_table`, `drug_name_map` |
| `convert_pred_equiv.Rd` | `drug_name_map` |
| `evaluate_against_gold.Rd` | `dose_breaks`, `dose_labels` |
| `standardize_drug_name.Rd` | `drug_name_map` |

**Fix**

Update `\usage{}` to match the full function signature, and add `\item{}`
entries in `\arguments{}` for each new parameter.

**Prevention**: always run `devtools::document()` after changing a function
signature. Commit the updated Rd files in the same PR as the code change.

**Related variant — "Undocumented arguments" WARNING**

A subtler form of the same problem: the parameter is present in `\usage{}` (so
the function signature matches) but is missing from `\arguments{}`:

```
Warning: Undocumented arguments in Rd file 'calc_daily_dose_baseline.Rd'
  'equiv_table' 'drug_name_map'
```

This happens when a new parameter is added to `\usage{}` (e.g., by copying the
full signature from the R source) but the matching `\item{equiv_table}{...}`
entry is never written in `\arguments{}`.

Fix: add the `\item{}` block to `\arguments{}`. The entry must appear between
the last documented argument and the closing `}` of the `\arguments{}` block.

---

## 8. Global variable binding NOTEs (dplyr column names)

**Symptom**

```
Note: no visible binding for global variable '.m1'
Note: no visible binding for global variable '.m2'
Note: no visible binding for global variable '.m3'
Note: no visible binding for global variable '.m4'
```

**Root cause**

R's static analyzer cannot resolve variables that are created dynamically
inside `dplyr::mutate()`. When `dplyr` creates intermediate columns named
`.m1`, `.m2`, etc. and those names are later referenced in `dplyr::rename()`,
the analyzer sees them as "undefined global variables".

**Fix**

Declare the names at the top of the file (or in a package-level file):

```r
utils::globalVariables(c(".m1", ".m2", ".m3", ".m4"))
```

This suppresses the NOTE without changing runtime behaviour.

---

## 9. Non-standard top-level files (NOTEs)

**Symptom**

```
Note: Non-standard files/directories found at top level:
  'CLAUDE.md' 'LICENSE.md' '.env.example'
```

**Root cause**

R CMD check expects only specific files and directories at the package root
(DESCRIPTION, NAMESPACE, R/, man/, etc.). Developer tools, workflow docs, or
example config files that live at the root are flagged as "non-standard".

**Fix**

Add entries to `.Rbuildignore`. Each line is an ERE (extended regular
expression) matched against the path relative to the package root:

```
^CLAUDE\.md$
^LICENSE\.md$
^\.env\.example$
^\.github$
^docs$
^bug\.md$
^data-raw$
```

Files matched here are excluded from `R CMD build` tarballs and from R CMD
check's directory scan.

**Tip**: any file you would not want end users to receive when they
`install.packages()` belongs in `.Rbuildignore`.

---

## 10. GitHub Actions Node.js 20 deprecation warning

**Symptom** (GitHub Actions log)

```
Warning: Node.js 20 actions are deprecated. The following actions are running
on Node.js 20 and may not work as expected: actions/cache@v4,
actions/checkout@v4, actions/upload-artifact@v4,
r-lib/actions/setup-pandoc@v2, r-lib/actions/setup-r@v2.
Actions will be forced to run with Node.js 24 by default starting
June 2nd, 2026.
```

**Root cause**

GitHub Actions runners internally execute each action's JavaScript using
Node.js. As Node.js versions EOL, GitHub announces deprecation periods and
eventually forces upgrades. The v4/v2 action tags currently ship Node.js 20
bundles; from June 2026 they will be forced onto Node.js 24.

**Fix (opt in early)**

Add `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true` to the workflow `env`:

```yaml
env:
  GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
  R_KEEP_PKG_SOURCE: yes
  FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true
```

This silences the warning immediately and ensures you are testing against the
future default.

**Alternative (update action versions)**

If the action maintainers release new major versions that bundle Node.js 24
natively (e.g., `actions/checkout@v5`), pin to those versions instead and
remove the env variable.

**Scope**: this env key only applies to the job it is declared in. If you
have multiple jobs, add it to each job's `env:` block, or set it at the
workflow level:

```yaml
env:
  FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true
```

---

## 11. Test fixtures must include route columns when filter_oral = TRUE is the default

**Symptom**

```
Warning: No route column found (route_concept_name or route_source_value).
Oral filter skipped — all records retained.
```

Appeared for every test (17 warnings), even tests that had nothing to do with
oral filtering.

**Root cause**

`filter_oral = TRUE` became the default for `calc_daily_dose_baseline()`,
`calc_daily_dose_nlp()`, and `calc_daily_dose_nlp_advanced()`. The helper
function that builds test fixtures (`make_row()`) did not include a
`route_concept_name` column, so the filter could not run and warned on every
call.

**Fix**

Add a route column to every fixture — including the `make_row()` helper and all
inline tibbles — even when the test does not exercise oral filtering:

```r
make_row <- function(..., route_concept_name = "Oral") {
  tibble::tibble(route_concept_name = route_concept_name, ...)
}
```

And in inline tibbles:

```r
tibble::tibble(
  route_concept_name      = "Oral",
  drug_concept_name       = "Prednisone 5 mg oral tablet",
  ...
)
```

**Prevention**: whenever you add a parameter with a new default that requires an
additional column, immediately update the test fixture helper and every inline
tibble in the test suite.

---

## 12. parse_sig() adds daily_dose_mg — vignette rename will collide

**Symptom**

```
Error in `dplyr::rename()`:
! Names must be unique.
x These names are duplicated: "daily_dose_mg" [1, 2]
```

**Root cause**

`m2_sig_parse = "auto"` (the default since v0.1.8) calls `parse_sig()` inside
`calc_daily_dose_baseline()`. `parse_sig()` adds a `daily_dose_mg` column to
the data frame. If a downstream step then tries to
`dplyr::rename(daily_dose_mg = daily_dose_mg_imputed)`, the column name already
exists and dplyr errors.

This hit the baseline-workflow vignette because Step 2 contained:

```r
drug_baseline <- drug_baseline |>
  dplyr::rename(daily_dose_mg = daily_dose_mg_imputed)   # WRONG — collision!
```

**Fix**

Never rename `daily_dose_mg_imputed` to `daily_dose_mg`. Use the output column
name as-is throughout downstream steps:

```r
drug_pe <- convert_pred_equiv(
  drug_baseline,
  dose_col = "daily_dose_mg_imputed"   # use the actual output column name
)
```

And in `build_episodes()`:

```r
episodes <- build_episodes(drug_for_episodes, dose_col = "pred_equiv_mg")
```

**Prevention**: before renaming any column in vignettes or analysis scripts,
check whether `parse_sig()` or any upstream function already emits that name.

---

## 13. Gold standard must be converted to prednisone equivalents before evaluation

**Symptom**

Systematic large errors when comparing computed doses (in pred-equiv mg) against
the gold standard: baseline computed ~5 mg pred-equiv for a prednisone record,
gold showed 5 mg — should match — but methylprednisolone records showed 4 mg
gold vs 5 mg computed, giving a spurious 25% error.

**Root cause**

The gold standard `median_daily_dose` column stores the dose in the **native
drug unit** (e.g., 4 mg methylprednisolone, not 5 mg prednisone-equivalent).
The computed side goes through `convert_pred_equiv()` before episode building.
Comparing native-unit gold against pred-equiv computed doses produces systematic
errors for any drug other than prednisone itself.

**Fix**

After loading the gold standard, overlap-join it to the oral-filtered drug data
frame to identify the drug name for each gold episode, then apply
`convert_pred_equiv()`:

```r
# 1. Identify drug for each gold episode via date overlap
gold_drug_map <- baseline_df |>
  dplyr::select(person_id, drug_name_std, drug_exposure_start_date,
                drug_exposure_end_date) |>
  dplyr::inner_join(
    gold_std |>
      dplyr::rename(person_id = patient_id) |>
      dplyr::select(person_id, episode_start, episode_end, median_daily_dose),
    by = "person_id"
  ) |>
  dplyr::filter(drug_exposure_start_date <= episode_end,
                drug_exposure_end_date   >= episode_start) |>
  dplyr::group_by(person_id, episode_start, episode_end) |>
  dplyr::slice_min(drug_exposure_start_date, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

# 2. Convert gold dose to pred-equiv
gold_drug_conv <- convert_pred_equiv(
  gold_drug_map,
  drug_col = "drug_name_std",
  dose_col = "median_daily_dose"
)

# 3. Write back; fall back to raw dose if conversion failed
gold_std <- gold_std |>
  dplyr::left_join(
    gold_drug_conv |>
      dplyr::rename(patient_id = person_id) |>
      dplyr::select(patient_id, episode_start, episode_end, pred_equiv_mg),
    by = c("patient_id", "episode_start", "episode_end")
  ) |>
  dplyr::mutate(
    median_daily_dose = dplyr::coalesce(pred_equiv_mg, median_daily_dose)
  ) |>
  dplyr::select(-pred_equiv_mg)
```

**Prevention**: every evaluation pipeline must ensure both sides are on the same
dose scale. Document the expected unit (pred-equiv mg) in the gold standard
schema.

---

## 14. Use oral-filtered df as source for gold drug map — not raw drug_df

**Symptom**

Very large errors (>100 mg) for patients who also had IV methylprednisolone
pulse therapy. Gold episode showed ~5 mg oral prednisone; computed episode
showed ~1000 mg because the gold drug map identified the drug as
"methylprednisolone injection" and the conversion factor amplified the native
dose.

**Root cause**

The overlap-join used to map gold episodes to drug names (see #13 above) was
sourced from `drug_df` (all routes). When a patient had both oral prednisone
and IV methylprednisolone pulse therapy on overlapping dates, the join could
pick the IV record, causing a drastically wrong conversion factor.

Similarly, `calc_daily_dose_nlp_advanced()` was called without
`filter_oral = TRUE`, so injection records entered the advanced NLP imputation
and produced implausibly high daily doses.

**Fix**

1. Always use the oral-filtered data frame (`baseline_df`, already filtered) as
   the source for the gold drug map join.
2. Pass `filter_oral = TRUE` explicitly to `calc_daily_dose_nlp_advanced()` even
   though it is the default, to make the intent visible in analysis scripts.

```r
# Use baseline_df (oral-filtered), not drug_df (all routes)
gold_drug_map <- baseline_df |> ...

# Explicit filter_oral in advanced NLP
adv_nlp_df <- calc_daily_dose_nlp_advanced(con, filter_oral = TRUE)
```

**Prevention**: never source a drug-name lookup from unfiltered data when the
downstream pipeline is oral-only.

---

## 15. Cascade reorder invalidates tests that rely on default method ordering

**Symptom**

```
-- Failure: supply_based (M4 default) --
Expected: 33.3
  Actual: 33.3   # same value, but imputation_method is "actual_duration", not "supply_based"
```

**Root cause**

The cascade order in `calc_daily_dose_baseline()` was changed (v0.1.7) so that
`actual_duration` (M3/Burkard) runs before `supply_based` (M4). A test that
relied on the default `methods` vector and expected `imputation_method ==
"supply_based"` silently produced the same numeric result but from M3, so the
method assertion failed.

**Fix**

Pass an explicit `methods` argument in tests that need to exercise a specific
imputation step:

```r
# Test M4 specifically — do not rely on the default cascade
out <- calc_daily_dose_baseline(make_row(...), methods = c("supply_based"))
expect_equal(out$imputation_method, "supply_based")
```

**Prevention**: after any change to the cascade order or `methods` default,
search for all tests that assert `imputation_method` and verify that their
expected values still match. Tests should pin the exact `methods` they are
exercising, not rely on the full default cascade.

---

## 16. New exported functions not found after package update

**Symptom**

```
Error in create_connection_from_safer_env(...) :
  could not find function "create_connection_from_safer_env"
```

Also manifests as `R CMD check` WARNING:

```
Warning: create_safer_connection: no documentation for 'create_safer_connection'
Warning: create_connection_from_safer_env: no documentation for ...
```

**Root cause**

When new functions are added to `R/` with `@export` roxygen tags and the
`NAMESPACE` is updated (manually or via `devtools::document()`), `R CMD check`
requires a corresponding `man/<fn>.Rd` file for every exported symbol.
Without it, the check emits "undocumented objects" warnings and the installed
package may fail to load the function into the search path.

In v0.3.0–v0.3.1, four new connection functions were added and exported:
`create_safer_connection`, `create_connection_from_safer_env`,
`create_discovery_connection`, and `create_connection_from_discovery_env`.
Their `man/*.Rd` files were not created at the same time, causing the check
failure and the runtime "could not find function" error.

**Fix**

Create `man/<fn>.Rd` for every new exported function at the time it is added.
Each Rd file must have `\name{}`, `\alias{}`, `\title{}`, `\usage{}` (matching
the exact function signature), `\arguments{}` (one `\item` per parameter), and
`\value{}`. Missing or mismatched `\usage{}` triggers a separate codoc ERROR
(see entry #7).

**Prevention**: adopt as a standing rule — every `@export` tag in `R/` needs a
corresponding entry in `man/`. After adding any exported function, run:

```r
devtools::check(document = FALSE)   # confirm no "undocumented" warnings
```

If using roxygen2 workflow, run `devtools::document()` first so roxygen
generates the Rd and updates NAMESPACE together. If editing NAMESPACE manually,
create the Rd manually at the same time. Never commit an `export(fn)` line in
NAMESPACE without a matching `man/fn.Rd`.

---

## Quick reference

| Check level | Issue | File(s) to edit |
|-------------|-------|----------------|
| ERROR | Interactive script in `tests/` | Add `if (!interactive()) quit(status = 0L)` |
| WARNING | Non-ASCII in R source or Rd | Replace with ASCII; grep with `[^\x00-\x7F]` |
| WARNING | Undeclared package dependency | Add to `Suggests` in `DESCRIPTION` |
| WARNING | Undocumented exported function | Create `man/<fn>.Rd` |
| WARNING | Codoc mismatch (`\usage{}` wrong) | Update `\usage{}` in Rd to match function signature |
| WARNING | Undocumented arguments (in `\usage{}` but not `\arguments{}`) | Add `\item{param}{desc}` to `\arguments{}` in Rd |
| NOTE | Rd line > 100 chars in `\examples` | Wrap with `paste()` or split string literal |
| WARNING | Spurious test warnings | Pass a "silent" mode parameter to the function |
| WARNING | No route column / filter_oral skipped | Add `route_concept_name` to all test fixtures |
| NOTE | Non-standard top-level files | Add pattern to `.Rbuildignore` |
| NOTE | Global variable binding | `utils::globalVariables(c("var1", ...))` |
| GHA warn | Node.js deprecation | `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true` in workflow |
| Logic bug | Rename collision after parse_sig() | Use `dose_col = "daily_dose_mg_imputed"` directly |
| Logic bug | Gold in native units vs pred-equiv | Convert gold with `convert_pred_equiv()` before eval |
| Logic bug | Injection contamination in drug map | Use oral-filtered df (not `drug_df`) as join source |
| Logic bug | Wrong imputation_method in test | Pin explicit `methods = c("supply_based")` in test |
| WARNING | Exported function missing Rd file | Create `man/<fn>.Rd` at same time as `@export` |
