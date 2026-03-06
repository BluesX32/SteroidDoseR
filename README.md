# SteroidDoseR

**Compute prednisone-equivalent daily doses from OMOP CDM corticosteroid records.**

`SteroidDoseR` is an R package that extracts corticosteroid drug-exposure data from an OMOP Common Data Model (CDM) database, imputes daily doses using structured fields or free-text SIG instructions, converts doses to prednisone equivalents, and aggregates records into continuous treatment episodes. It is designed for myositis and other rheumatologic cohort studies.

---

## Overview

Computing corticosteroid doses from electronic health records is difficult. Prescription records are often incomplete: dose fields may be missing, SIG instructions (the text directions on a prescription) vary widely in format, and the same drug may appear under many names. Manual chart review does not scale.

`SteroidDoseR` automates this process. It works directly with OMOP CDM `drug_exposure` data — either from a live database connection or from a data frame you supply — and applies two complementary imputation methods:

- **Baseline method:** fills in missing daily doses using structured OMOP fields (tablet strength, quantity, days supply) in a four-step cascade.
- **NLP method:** parses free-text SIG strings using rule-based regular expressions to extract tablet count, frequency, and duration.

After imputation, doses are converted to prednisone-equivalent milligrams and aggregated into continuous treatment episodes. An optional evaluation step measures accuracy against a manually reviewed gold standard.

OMOP CDM is a standardized data format maintained by the OHDSI (Observational Health Data Sciences and Informatics) consortium. If your institution participates in an OMOP network, you can point this package directly at your database. If not, the package also accepts any data frame that follows the OMOP `drug_exposure` schema.

---

## Key features

- **Two imputation methods** — Baseline (structured OMOP fields, M1–M4 cascade) and NLP (rule-based SIG text parsing).
- **Prednisone-equivalency conversion** — built-in table covering 8 corticosteroids (prednisone, prednisolone, methylprednisolone, dexamethasone, hydrocortisone, triamcinolone, budesonide, and others).
- **Episode building** — gap-bridges overlapping and adjacent prescriptions into continuous treatment episodes using the OHDSI standard 30-day gap.
- **Connector abstraction** — identical calling code works against a live OMOP CDM database or an in-memory data frame; no code changes needed when switching.
- **Capability detection** — probes which `drug_exposure` fields are available at your site and adjusts automatically.
- **Filtering** — subset by patient IDs or OMOP drug concept IDs at the query level.
- **Evaluation** — compare computed episodes to a manually reviewed gold standard; returns MAE, MBE, RMSE, coverage, and correlation metrics.
- **Bundled synthetic data** — 29-record example dataset and 10-episode gold standard included; no database required to try the package.

---

## Installation

### Prerequisites

- **R ≥ 4.1** is recommended.
- The following core packages are installed automatically as dependencies: `dplyr`, `lubridate`, `purrr`, `rlang`, `stringr`, `tibble`, `tidyr`, `readr`.
- **For OMOP CDM database connectivity only** (optional): `DatabaseConnector` (≥ 3.0.0) and `SqlRender` (≥ 1.11.0). These are not installed automatically — see below.
- **Windows only**: Building from source requires [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Install the version matching your R version before attempting installation.

### Install from local source

`SteroidDoseR` is currently distributed as a source package. Install it from the directory where you have downloaded or cloned the repository:

```r
# Using devtools
devtools::install_local("path/to/SteroidDoseR/")

# Using pak (alternative, handles some Windows path issues better)
pak::local_install("path/to/SteroidDoseR/")
```

Replace `"path/to/SteroidDoseR/"` with the actual path to the package directory on your machine.

### Optional: database connectivity packages

Install these only if you plan to connect to a live OMOP CDM database:

```r
install.packages("DatabaseConnector")
install.packages("SqlRender")
```

### Troubleshooting: local installation on Windows

If `devtools::install_local()` fails on Windows, work through the following checks:

**1. Rtools not installed or wrong version.**
Download and install Rtools from [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/). The version must match your installed R version (e.g., R 4.3 requires Rtools43). After installing, restart R and run `pkgbuild::has_build_tools(debug = TRUE)` to confirm it is found.

**2. Library write-permission error.**
R may not have permission to write to the default system library. Check your active library paths:

```r
.libPaths()
```

If the first path requires administrator access, either run RStudio or Rgui as administrator (right-click → "Run as administrator"), or set a user-writable library path:

```r
# Create a user library if one does not exist
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(Sys.getenv("R_LIBS_USER"))
```

**3. Try `pak` instead of `devtools`.**
`pak` resolves some Windows-specific dependency and path issues that `devtools` does not handle:

```r
install.packages("pak")
pak::local_install("path/to/SteroidDoseR/")
```

**4. Verify the installation.**
After installing, confirm the package loaded correctly:

```r
library(SteroidDoseR)
sessionInfo()
```

The output should list `SteroidDoseR` under "other attached packages".

---

## Getting started

The quickest way to try the package is with the bundled synthetic data — no database required.

```r
library(SteroidDoseR)

# 1. Load the bundled synthetic drug-exposure data
extdata  <- system.file("extdata", package = "SteroidDoseR")
drug_exp <- readr::read_csv(file.path(extdata, "synthetic_drug_exposure.csv"),
                            show_col_types = FALSE)

# 2. Wrap the data frame in a connector
con <- create_df_connector(drug_exp)

# 3. Run the full pipeline in one call
episodes <- run_pipeline(con, method = "baseline")

# 4. Inspect results
episodes[, c("person_id", "drug_name_std", "episode_start",
             "episode_end", "n_days", "median_daily_dose")]
```

`run_pipeline()` handles fetching, dose imputation, prednisone-equivalency conversion, and episode building in a single call. The result is one row per patient–drug episode.

To use the NLP method instead, change `method = "baseline"` to `method = "nlp"`. This parses the free-text `sig` column rather than relying on structured numeric fields.

---

## Input data and connector setup

### Two connector types

| Connector | Constructor function | When to use |
|-----------|----------------------|-------------|
| Data-frame connector | `create_df_connector(drug_exp)` | Testing, synthetic data, no database available |
| OMOP CDM connector | `create_omop_connector(cd, cdm_schema = "...")` | Live OMOP CDM database |

The two connector types share the same interface. Functions like `run_pipeline()`, `calc_daily_dose_baseline()`, and `calc_daily_dose_nlp()` accept either type without modification.

### Connecting to a live OMOP CDM database

Before using the OMOP CDM connector you need:

- `DatabaseConnector` and `SqlRender` installed (see Installation above).
- Read-only database access to a schema containing the `drug_exposure` and `concept` tables.
- Valid connection credentials (server address, username, password or service account).

Store credentials in environment variables, not in your scripts. The example below reads them from the environment:

```r
library(SteroidDoseR)
library(DatabaseConnector)

# Build connection details (DatabaseConnector standard)
cd <- createConnectionDetails(
  dbms     = "postgresql",        # also: "sql server", "snowflake", "bigquery"
  server   = "myserver/omop_db",
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port     = 5432
)

# Wrap in a SteroidDoseR OMOP connector
con <- create_omop_connector(
  connectionDetails = cd,
  cdm_schema        = "cdm_531",   # schema containing drug_exposure, concept, ...
  vocab_schema      = "vocab",     # optional: separate vocabulary schema
  cdm_version       = "5.4"
)
```

The package opens a database connection only during the query and closes it immediately after. There is no persistent connection to manage.

### Detecting available fields

OMOP CDM implementations vary by site. Use `detect_capabilities()` to probe which `drug_exposure` columns are populated at your site:

```r
con <- detect_capabilities(con)
con$capabilities
#> $has_sig         TRUE
#> $has_days_supply TRUE
#> $has_quantity    TRUE
#> $has_route       TRUE
```

The baseline and NLP functions use this information to select the best available imputation strategy automatically.

### Sites without a `sig` column

Some OMOP CDM instances do not populate `drug_exposure.sig`. Use `sig_source = "drug_source_value"` to fall back to `drug_source_value` as the SIG text source for NLP parsing:

```r
doses <- calc_daily_dose_nlp(con, sig_source = "drug_source_value")
```

The package also detects automatically when `sig` is absent or entirely missing and aliases `drug_source_value` in its place.

### Filtering by patient or drug

Both `run_pipeline()` and the individual imputation functions accept optional filters when a connector is provided:

```r
# Restrict to specific patients
doses <- calc_daily_dose_baseline(con, person_ids = c(1001L, 1002L, 1003L))

# Restrict to specific OMOP drug concept IDs
doses <- calc_daily_dose_nlp(con, drug_concept_ids = c(1518254L, 40224131L))
```

These arguments are silently ignored when a data-frame connector is used, so the same calling code works in both paths.

### Minimum required columns (data-frame path)

When passing a plain data frame rather than an OMOP CDM connector, the frame must mirror the OMOP `drug_exposure` schema. The minimum required column is:

- `person_id`, `drug_exposure_start_date`

Recommended additional columns:

| Method | Columns that improve imputation |
|--------|---------------------------------|
| Baseline | `amount_value`, `quantity`, `days_supply`, `drug_exposure_end_date`, `drug_source_value` |
| NLP | `sig`, `drug_concept_name` |

See `?drug_df_contract` for the full column specification.

---

## Main workflow

The typical workflow follows these steps:

**1. Create a connector.**

Use `create_df_connector()` for a data frame or `create_omop_connector()` for a live database (see Connector setup above).

**2. (Optional) Detect field availability.**

```r
con <- detect_capabilities(con)
```

This updates the connector with information about which fields exist at your site. You can skip this step if you already know your data.

**3a. Option A — One-call pipeline.**

```r
episodes <- run_pipeline(con, method = "baseline")  # or method = "nlp"
```

`run_pipeline()` chains all steps in order: fetch data → impute daily dose → convert to prednisone equivalent → build episodes. Use `return_level = "exposure"` to get per-record output instead of episodes.

**3b. Option B — Step-by-step.**

For more control, call each function individually:

```r
# Impute daily dose
dose_df <- calc_daily_dose_baseline(con)
# or:
dose_df <- calc_daily_dose_nlp(con)

# Convert to prednisone-equivalent milligrams
equiv_df <- convert_pred_equiv(
  dose_df,
  drug_col = "drug_concept_name",
  dose_col = "daily_dose_mg_imputed"
)

# Build continuous treatment episodes
episodes <- build_episodes(
  equiv_df |> dplyr::filter(!is.na(pred_equiv_mg)),
  end_col  = "drug_exposure_end_date",
  dose_col = "pred_equiv_mg",
  gap_days = 30L
)
```

**4. (Optional) Evaluate against a gold standard.**

If you have a manually reviewed reference dataset, compare computed episodes to it:

```r
gold_std    <- readr::read_csv(file.path(extdata, "synthetic_gold_standard.csv"),
                               show_col_types = FALSE)
eval_result <- evaluate_against_gold(episodes, gold_std)
eval_result$summary[, c("coverage_pct", "MAE", "MBE", "RMSE")]
```

---

## Output

### Episodes

`run_pipeline()` and `build_episodes()` return a data frame with one row per patient–drug episode:

| Column | Description |
|--------|-------------|
| `person_id` | Patient identifier |
| `drug_name_std` | Standardized drug name |
| `episode_id` | Integer episode identifier within each patient–drug pair |
| `episode_start` | Date the episode begins |
| `episode_end` | Date the episode ends |
| `n_days` | Duration of the episode in days |
| `n_records` | Number of prescription records merged into the episode |
| `median_daily_dose` | Median prednisone-equivalent daily dose across records in the episode |
| `min_daily_dose` | Minimum daily dose in the episode |
| `max_daily_dose` | Maximum daily dose in the episode |

### Evaluation results

`evaluate_against_gold()` returns a named list with three elements:

- **`$summary`** — One-row summary across all episodes: `coverage_pct` (percentage of gold-standard episodes matched), `MAE` (mean absolute error in mg), `MBE` (mean bias error), `RMSE`, `MAPE`, Pearson and Spearman correlations.
- **`$comparison`** — Per-episode detail: matched gold-standard episode, computed dose, absolute error, relative error, and agreement category (Exact ≤ 5%, Good ≤ 20%, Moderate ≤ 50%, Poor > 50%).
- **`$stratified`** — Metrics broken down by dose range, SIG category, and taper status.

`coverage_pct` tells you what fraction of manually reviewed episodes the algorithm recovered. `MAE` tells you how far off the dose estimates are in absolute terms.

### Intermediate outputs

- **`calc_daily_dose_baseline()`** adds `daily_dose_mg_imputed`, `strength_mg`, and `imputation_method` columns (M1–M4 label or "missing").
- **`calc_daily_dose_nlp()`** adds `daily_dose_mg`, `parsed_status`, and parsing components: `tablets`, `freq_per_day`, `mg_per_admin`, `duration_days`, `taper_flag`, `prn_flag`, `free_text_flag`.
- **`convert_pred_equiv()`** adds `pred_equiv_mg`, `equiv_factor`, and `pred_equiv_status`.

---

## Function reference

| Function | Connector-first? | Description |
|----------|-----------------|-------------|
| `create_omop_connector()` | — | Build a live OMOP CDM connector. |
| `create_df_connector()` | — | Wrap a data frame as a connector (tests, synthetic data). |
| `detect_capabilities()` | — | Probe which `drug_exposure` fields are available at your site. |
| `run_pipeline()` | Yes | One-call fetch → impute → convert → episodes. |
| `calc_daily_dose_baseline()` | Yes | Structured-field cascading imputation (M1–M4). |
| `calc_daily_dose_nlp()` | Yes | Rule-based SIG text parsing. |
| `parse_sig()` | No | Vectorized SIG string parser (low-level). |
| `convert_pred_equiv()` | No | Multiply daily doses by prednisone-equivalency factors. |
| `build_episodes()` | Yes | Gap-bridge prescriptions into continuous episodes. |
| `evaluate_against_gold()` | No | Coverage, MAE, MBE, RMSE vs. manual gold standard. |

"Connector-first" means the function accepts a connector object and handles database extraction internally. Functions without connector support operate on data frames directly.

---

## Project status

This is version 0.1.0, research-stage software. It is under active development.

| Phase | Status | Content |
|-------|--------|---------|
| 1 | Complete | Baseline imputation, NLP parsing, prednisone conversion, episode building, evaluation |
| 1.5 | Complete | Connector abstraction layer (DatabaseConnector / df_connector) |
| 3 | Planned | LLM agent for complex or ambiguous SIG interpretation |

---

## Troubleshooting and support

If something is not working, check the following in order:

1. **Verify your R session**: run `sessionInfo()` and confirm `SteroidDoseR` is listed. Check that the core dependency versions (dplyr ≥ 1.1.0, lubridate ≥ 1.9.0, etc.) meet the requirements in `DESCRIPTION`.

2. **Isolate the issue**: try the data-frame path first. If `create_df_connector()` and `run_pipeline()` work with the bundled synthetic data, the problem is with your database connection or data, not the package itself.

3. **Check database settings**: if using `create_omop_connector()`, verify the schema name, DBMS type, and that your account has read access to `drug_exposure` and `concept`.

4. **Run the vignettes**: four vignettes cover the main usage patterns:
   - `vignette("connector-workflow")` — connector setup, one-call pipeline, step-by-step workflow
   - `vignette("baseline-workflow")` — structured-field imputation
   - `vignette("nlp-workflow")` — SIG text parsing
   - `vignette("evaluation-workflow")` — gold-standard comparison

5. **Contact**: mxiong5@jhu.edu

---

## Citation

Xiong M et al. *AgentDose: Automated Steroid Dose Calculation from EHR Data.*
OHDSI Global Symposium 2025. Abstract 205.

---

## Items for maintainer confirmation

The following details could not be fully verified from the repository and should be confirmed before publishing:

- **GitHub URL**: No public GitHub remote was found in the repository configuration. If the package will be available on GitHub, add an installation route using `remotes::install_github()` or `pak::pkg_install()` to the Installation section.
- **R version minimum**: The DESCRIPTION does not specify `Depends: R (>= ...)`. Confirm whether R ≥ 4.1 is an accurate minimum.
- **`budesonide` equivalency factor**: The built-in equivalency table marks budesonide as `NA` with a note that it is route-dependent. Consider adding a brief explanation of how users should handle budesonide records.
- **Phase numbering**: The roadmap jumps from Phase 1.5 to Phase 3. Confirm whether Phase 2 exists or was skipped.
