# SteroidDoseR

**Corticosteroid Daily Dose Calculation from OMOP CDM Data**

`SteroidDoseR` is an R package that provides two methods for computing
prednisone-equivalent daily doses from OMOP CDM drug-exposure records:

| Method | Description |
|--------|-------------|
| **Baseline** | Uses structured OMOP fields (tablet strength, quantity, days supply) with four-step cascading imputation. |
| **NLP** | Parses free-text SIG strings using rule-based regex to extract dose, frequency, and duration. |

Designed for myositis and other rheumatologic cohort studies.
Phase 1 replicates the analysis from the OHDSI 2025 AgentDose poster
(Xiong et al.).

---

## Installation

```r
# Install from local source (development):
devtools::install_local("SteroidDoseR/")
```

---

## Quick start

```r
library(SteroidDoseR)

# Load synthetic example data
extdata  <- system.file("extdata", package = "SteroidDoseR")
drug_exp <- readr::read_csv(file.path(extdata, "synthetic_drug_exposure.csv"))

# --- NLP method ---
drug_nlp <- calc_daily_dose_nlp(drug_exp)
drug_pe  <- convert_pred_equiv(drug_nlp, drug_col = "drug_name_std",
                                          dose_col = "daily_dose_mg")
episodes <- build_episodes(drug_pe |>
  dplyr::filter(!is.na(pred_equiv_mg)) |>
  dplyr::rename(daily_dose_mg_imputed = pred_equiv_mg),
  end_col = "drug_exposure_end_date")

# --- Evaluate against gold standard ---
gold_std <- readr::read_csv(file.path(extdata, "synthetic_gold_standard.csv"))
eval     <- evaluate_against_gold(episodes, gold_std)

eval$summary[, c("coverage_pct", "MAE", "MBE")]
```

---

## Connect to OMOP CDM

Supply a connector object instead of constructing `drug_df` manually.
The package handles SQL extraction, date filtering, and field-availability
detection automatically — across PostgreSQL, SQL Server, Snowflake, BigQuery,
and other DBMS supported by [DatabaseConnector](https://ohdsi.github.io/DatabaseConnector/).

```r
library(DatabaseConnector)

# Build connection details (no credentials stored in code)
cd <- createConnectionDetails(
  dbms     = "postgresql",
  server   = "myserver/omop_db",
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port     = 5432
)

con <- create_omop_connector(cd, cdm_schema = "cdm_531")

# One-call pipeline: fetch → impute → convert → episodes
episodes <- run_pipeline(con, method = "baseline")
```

For sites without a `sig` column use `sig_source = "drug_source_value"`.
See `vignette("connector-workflow")` for the complete walkthrough.

### Synthetic / test connector

No database? Use `create_df_connector()` to wrap any data frame with the
same interface — identical calling code, no extra dependencies:

```r
extdata  <- system.file("extdata", package = "SteroidDoseR")
drug_exp <- readr::read_csv(file.path(extdata, "synthetic_drug_exposure.csv"))

con      <- create_df_connector(drug_exp)
episodes <- run_pipeline(con, method = "baseline")
```

---

## Public API

| Function | Connector-first? | Description |
|----------|-----------------|-------------|
| `create_omop_connector()` | — | Build a live OMOP CDM connector. |
| `create_df_connector()` | — | Wrap a data frame as a connector (tests/vignettes). |
| `detect_capabilities()` | — | Probe which `drug_exposure` fields are available. |
| `run_pipeline()` | Yes | One-call fetch → impute → convert → episodes. |
| `calc_daily_dose_baseline()` | Yes | Structured-field cascading imputation (M1–M4). |
| `calc_daily_dose_nlp()` | Yes | Rule-based SIG text parsing. |
| `convert_pred_equiv()` | No | Multiply by prednisone-equivalency factors. |
| `build_episodes()` | Yes | Gap-bridge prescriptions into continuous episodes. |
| `evaluate_against_gold()` | No | Coverage, MAE, MBE vs. manual gold standard. |

See `vignette("connector-workflow")`, `vignette("baseline-workflow")`,
`vignette("nlp-workflow")`, and `vignette("evaluation-workflow")`.

---

## Data requirements

When using connectors, the package extracts data directly — no manual
`drug_df` construction needed. When supplying a plain data frame, columns
must mirror the OMOP `drug_exposure` domain. See `?drug_df_contract`,
`?calc_daily_dose_baseline`, and `?calc_daily_dose_nlp` for the full
column specification. No real patient data is shipped — only fully
synthetic examples.

---

## Phase roadmap

| Phase | Status | Content |
|-------|--------|---------|
| **1** | ✅ Complete | Baseline + NLP + conversion + evaluation |
| **1.5** | ✅ This release | Connector abstraction (DatabaseConnector / df_connector) |
| 3 | Planned | LLM agent (Claude) for complex SIG interpretation |

---

## Citation

Xiong M et al. *AgentDose: Automated Steroid Dose Calculation from EHR Data.*
OHDSI Global Symposium 2025. Abstract 205.
