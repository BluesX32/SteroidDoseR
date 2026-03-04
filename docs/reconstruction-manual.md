# Reconstruction Manual: From Analysis Scripts to R Package

**How SteroidDoseR was built from legacy research notebooks**

---

## 1. Introduction

Most research pipelines begin the same way: an analyst opens a notebook, loads some data, writes code to answer a question, and saves the output. This works well at first. The code runs, the figures look right, the results go into a paper.

The problem comes later — when a collaborator wants to run the same pipeline on new data, or when a reviewer asks for a minor change, or when you return to the project six months later and can no longer remember which script to run first, or in what order.

Analysis scripts are excellent for **exploration**. They are poor for **reproducibility**. The fix is to convert the pipeline into an R package.

An R package is just a structured collection of functions with documentation, tests, and metadata. Once packaged, your pipeline becomes something others can install, call, and trust. It becomes a tool, not just a notebook.

**This project — SteroidDoseR — is a direct example of that conversion.** The starting point was a collection of `.qmd` and `.Rmd` notebooks implementing two methods for computing prednisone-equivalent daily doses from electronic health record data:

- A **Baseline method** using structured OMOP CDM fields (tablet strength, quantity, days supply) with cascading imputation when data are missing
- An **NLP method** that parses free-text prescription instructions (called SIG strings) using rule-based regex patterns

Both methods fed into an **evaluation pipeline** that compared computed dose episodes against a manually-reviewed gold standard.

The goal was to turn these three pipelines into a single, reusable R package that any researcher could install and run against their own OMOP-structured dataset.

---

## 2. Starting Point: Legacy Research Code

The legacy project had a structure that will look familiar to most researchers:

```
DosageCalculation/
  Baseline/
    Baseline.qmd          # structured-field dose calculation
  NLP_Parser/
    NLP_parser.Rmd        # SIG text parsing
  Comparisons/
    Comparisons_Sep28.qmd # evaluation against gold standard
  exploratory/            # ad hoc scripts, scratch work
```

This structure has several problems:

**Logic is scattered.** The baseline imputation logic lives in one file, the NLP parser in another, the evaluation metrics in a third. There is no single entry point. To reproduce the full pipeline, you need to know the correct order, handle intermediate files between notebooks, and hope that the file paths still work on your machine.

**There are no tests.** If you change one function to fix a bug, nothing warns you that something else broke. You find out when the outputs look wrong — or you don't find out at all.

**Reuse is hard.** If a collaborator wants to run just the NLP parser on a new dataset, they need to extract that section of the notebook, figure out what dependencies it has, and adapt the code to their context.

**Data paths are hardcoded.** Scripts usually reference absolute paths or assume a specific working directory. On a different machine, they fail immediately.

The first step in fixing this is not to start writing a package. It is to **read the existing code carefully** and understand what it actually does.

---

## 3. Audit the Legacy Code

Before writing a single new file, spend time with the existing notebooks. The goal is to produce a **functional map**: a description of what each script does, what its inputs are, and what it produces.

For this project, the audit looked like this:

| File | Core algorithm | Key inputs | Key outputs |
|------|----------------|------------|-------------|
| `Baseline.qmd` | Four-step cascading imputation (M1–M4) | OMOP `drug_exposure` with structured fields | Daily dose estimate + imputation method label |
| `NLP_parser.Rmd` | Regex-based SIG parsing | Free-text SIG strings | Tablets, frequency, mg amount, duration, daily dose |
| `Comparisons_Sep28.qmd` | Date-overlap join + error metrics | Computed episodes + gold standard | MAE, MBE, RMSE, coverage percentage |

This audit also surfaces the **data contracts** — what column names and types each script expects. In this project, the baseline method required columns like `amount_value`, `quantity`, `days_supply`, and `daily_dose` (optional). The NLP method required a `sig` column containing the free-text instructions. Writing these down explicitly, before touching the package structure, saves substantial debugging time later.

The audit also identified one significant limitation worth documenting: the OHDSI DrugUtilisation package's `patternTable()` function finds no patterns for corticosteroids, making its built-in dose coverage functions unusable. The cascading fallback imputation in the Baseline method exists precisely because of this gap. These kinds of contextual notes belong in the package documentation — they tell future users why the code is structured the way it is.

---

## 4. Define the Package Interface

Once you understand the legacy pipeline, decide what functions to expose. A good package exposes **a small, stable set of functions** with clear inputs and outputs. Resist the urge to expose every helper function. Most internal logic should stay internal.

For SteroidDoseR, the public API was defined as five functions:

```r
calc_daily_dose_baseline()   # Structured-field imputation (M1–M4)
calc_daily_dose_nlp()        # SIG text parsing pipeline
convert_pred_equiv()         # Multiply by prednisone-equivalency factors
build_episodes()             # Merge prescriptions into continuous dose episodes
evaluate_against_gold()      # Compare episodes against a gold standard
```

Each function corresponds to a distinct step in the pipeline. A user can call them in sequence for the full workflow, or call any one individually on their own data. This modularity is the key architectural decision: it makes the package composable.

When defining function signatures, think about what the **minimum required inputs** are and what reasonable defaults look like. For example, `build_episodes()` needs a gap threshold (how many days between prescriptions counts as a new episode). Thirty days is a common clinical convention, so `gap_days = 30L` becomes the default. The user can change it, but they don't have to.

---

## 5. Create the Package Skeleton

With the interface defined, create the package structure. The `usethis` package makes this straightforward:

```r
usethis::create_package("SteroidDoseR")
usethis::use_roxygen_md()
usethis::use_testthat()
usethis::use_mit_license("Your Name")
usethis::use_readme_rmd()
```

These commands generate the standard directories and configuration files:

- **`DESCRIPTION`** — Package metadata: name, version, authors, dependencies, license. This is the single most important file. Every package you import must be listed here.
- **`NAMESPACE`** — Declares which functions your package exports. Generated automatically by `roxygen2`; do not edit by hand.
- **`R/`** — All function source code lives here, one logical group per file.
- **`tests/testthat/`** — Unit tests. Each test file mirrors a source file.
- **`vignettes/`** — Long-form tutorials that demonstrate real workflows.

One configuration detail that is easy to overlook: if your package has vignettes, you must declare `VignetteBuilder: knitr` in `DESCRIPTION` and list `knitr` and `rmarkdown` in `Suggests`. Without this, R CMD check will not build the vignettes and CI will not catch vignette errors.

---

## 6. Move Logic into Functions

This is the core of the conversion process. Take the algorithm from each notebook and rewrite it as a function that accepts a data frame and returns a data frame.

The transformation follows a simple pattern:

**Before (script style):**
```r
# load data
df <- read.csv("data/drug_exposure.csv")

# process
df$strength_mg <- as.numeric(gsub(".*?(\\d+) MG.*", "\\1", df$drug_source_value))
df$daily_dose_mg <- df$strength_mg * df$tablets * df$freq_per_day

# save output
write.csv(df, "output/doses.csv")
```

**After (function style):**
```r
calc_daily_dose_baseline <- function(drug_df, methods = c("original", "tablets_freq", ...)) {
  # validate inputs
  # compute strength_mg
  # apply imputation cascade
  # return augmented data frame
}
```

The function takes data as an argument, operates on it, and returns a result. It does not read from disk. It does not write to disk. It does not assume anything about file paths. This makes it testable and reusable.

For this project, the source files map one-to-one with the legacy notebooks:

```
R/baseline.R    ← Baseline.qmd imputation logic
R/nlp.R         ← NLP_parser.Rmd SIG parsing logic
R/eval.R        ← Comparisons_Sep28.qmd metrics logic
R/episodes.R    ← gap-bridging episode construction
R/conversion.R  ← prednisone-equivalency table and conversion
R/utils-validate.R  ← shared input validation helpers
```

Separating validation helpers into their own file (`utils-validate.R`) prevents every function from having to duplicate the same input checks. Internal helpers are not exported — they have `@noRd` in their documentation comments.

One practical issue that arose during porting: certain R idioms that work interactively do not work inside `dplyr::mutate()`. Specifically, `names(.)` cannot be evaluated inside a `mutate()` call because the `.` pronoun is not in scope under NSE. The fix was to compute any needed boolean flags before the `mutate()` call and reference them by name inside it. These kinds of translation errors are common and are caught quickly by the unit tests.

---

## 7. Remove Dependence on Real Data

Research packages must never ship private patient data, even in anonymized form. This is both an ethical requirement and a practical one — datasets with real patient information cannot be posted to public repositories.

The solution is **synthetic data**: small, realistic-looking datasets generated entirely from scratch with no connection to real patients. For this project, two synthetic fixtures were created:

- `inst/extdata/synthetic_drug_exposure.csv` — 29 rows covering five fictional patients, with a range of steroids, dosing patterns (daily, BID, taper), and edge cases (missing SIG, ambiguous instructions, overlapping date ranges)
- `inst/extdata/synthetic_gold_standard.csv` — 10 manually constructed episode rows for a subset of the fictional patients

These files live in `inst/extdata/` rather than `data/` because they are not R objects (`.rda`) but raw files accessed via `system.file()`. This is the correct location for example data that vignettes and tests load at runtime.

The `data-raw/` directory contains the script that generated these fixtures. Keeping the generation script in the repository means the synthetic data can always be regenerated if the format needs to change.

---

## 8. Add Unit Tests

Unit tests are the primary way you know the package still works after any change. Each test checks that a specific function, given specific inputs, produces the expected output.

For SteroidDoseR, tests were organized into five files mirroring the source files:

```
tests/testthat/test-nlp.R        # SIG parsing edge cases
tests/testthat/test-baseline.R   # imputation method selection
tests/testthat/test-conversion.R # equivalency factors
tests/testthat/test-episodes.R   # gap-bridging logic
tests/testthat/test-eval.R       # evaluation metrics
```

Good tests cover not just the happy path but the edge cases. For the NLP parser: what happens with an empty SIG string? With a taper instruction? With "as directed"? With a frequency of "every other day"? Each of these has its own test that pins the expected behavior.

One test revealed a genuine semantic ambiguity: does "(20 mg per dose)" mean 20 mg regardless of how many tablets are taken, or should it be multiplied by the tablet count? The clinical answer — "(X mg per dose)" is the total per-administration amount, not a per-tablet amount — was confirmed and encoded both in the function logic and in the test. Tests do not just verify code; they document intent.

---

## 9. Write Vignettes

Vignettes are long-form tutorials included with the package. They show a complete, realistic workflow from data loading through interpretation of results. Unlike function documentation, which documents individual functions, vignettes demonstrate how the functions work together.

This package ships three vignettes:

- **Baseline Workflow** — loads synthetic data, runs the structured-field imputation, converts to prednisone equivalents, builds episodes, and displays a summary table
- **NLP Workflow** — runs the SIG parser on the same data, reports parse coverage (what fraction of records yielded a usable dose estimate), and compares to baseline
- **Evaluation Workflow** — joins the computed episodes against the synthetic gold standard and reports MAE, MBE, and agreement categories

Each vignette references only the synthetic fixtures shipped with the package — never any external file. This ensures that anyone who installs the package can knit the vignettes on their own machine and see working output.

---

## 10. Continuous Integration

GitHub Actions runs R CMD check automatically on every push and pull request, across three operating systems (Ubuntu, macOS, Windows). This catches problems that only appear on specific platforms — file path separators, locale differences, missing system libraries — before they reach collaborators.

The workflow uses the standard `r-lib/actions` templates and installs all declared dependencies automatically. The key configuration choice was to include vignette building in the check (removing the `--no-build-vignettes` flag). This means every CI run also confirms that the vignettes knit without errors on a clean machine.

CI transforms "it works on my laptop" into "it works, demonstrably, on three platforms."

---

## 11. Validation Against Original Analysis

Packaging is not complete until you have verified that the package **reproduces the original results**. This is the step that closes the loop between the legacy scripts and the new package.

For this project, validation meant running the package functions on the same input data used in the original notebooks and comparing outputs:

- Episode counts should match between the legacy pipeline and `build_episodes()`
- Dose estimates from `calc_daily_dose_baseline()` should agree with the Baseline.qmd outputs for the same records
- MAE and MBE from `evaluate_against_gold()` should match the values reported in the Comparisons notebook

If numbers diverge, the discrepancy is almost always traceable to either a porting error (logic was copied incorrectly) or a deliberate improvement (a bug was fixed during the refactor). Both should be documented. The former needs to be corrected; the latter should be noted in `NEWS.md`.

---

## 12. Lessons Learned

**Scripts are for exploration; packages are for delivery.** The two modes serve different purposes. Write scripts freely when figuring things out. Once you know what you are doing, invest the effort to package it.

**Functions clarify logic.** The act of converting a script into a function forces you to name the inputs, name the outputs, and think about edge cases. This process consistently surfaces assumptions that were previously invisible.

**Tests prevent regression.** Every bug fixed during development should become a test. The tests for this package caught a real semantic error (the `(X mg per dose)` ambiguity) that would have silently produced wrong results in production.

**Audit before building.** The most valuable step in this project was reading the legacy notebooks carefully before writing any new code. Understanding the existing pipeline — its inputs, its logic, its limitations — made the design decisions obvious.

**CI ensures long-term reliability.** A package that passes R CMD check today may fail in six months when a dependency updates. Continuous integration catches these failures immediately, rather than when a collaborator reports that the package no longer works.

The transition from analysis scripts to an R package is not primarily a technical challenge. The hard part is deciding what the package should do and where the boundaries between functions should be. Once those decisions are made, the implementation follows naturally.

---

*SteroidDoseR Phase 1 — built from the OHDSI AgentDose analysis pipeline, JHU 2025.*
