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

`calc_daily_dose_baseline()` defaults to `m2_sig_parse = "warn"`. When the
test data has a `sig` column but no pre-parsed `tablets`/`freq_per_day`
columns, the function emits a warning on every call — including calls whose
only purpose is to verify the connector dispatch path.

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

## Quick reference

| Check level | Issue | File(s) to edit |
|-------------|-------|----------------|
| ERROR | Interactive script in `tests/` | Add `if (!interactive()) quit(status = 0L)` |
| WARNING | Non-ASCII in R source or Rd | Replace with ASCII; grep with `[^\x00-\x7F]` |
| WARNING | Undeclared package dependency | Add to `Suggests` in `DESCRIPTION` |
| WARNING | Undocumented exported function | Create `man/<fn>.Rd` |
| WARNING | Codoc mismatch | Update `\usage{}` and `\arguments{}` in Rd |
| WARNING | Spurious test warnings | Pass a "silent" mode parameter to the function |
| NOTE | Non-standard top-level files | Add pattern to `.Rbuildignore` |
| NOTE | Global variable binding | `utils::globalVariables(c("var1", ...))` |
| GHA warn | Node.js deprecation | `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24: true` in workflow |
