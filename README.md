# SteroidDoseR

Compute prednisone-equivalent daily doses from OMOP CDM corticosteroid records.

## Installation

```r
devtools::install_local("path/to/SteroidDoseR/")

# Optional: live OMOP CDM database connectivity
install.packages("DatabaseConnector")
install.packages("SqlRender")
```

## Quick start

```r
library(SteroidDoseR)

# Bundled synthetic data — no database required
extdata  <- system.file("extdata", package = "SteroidDoseR")
drug_exp <- readr::read_csv(file.path(extdata, "synthetic_drug_exposure.csv"),
                            show_col_types = FALSE)
con      <- create_df_connector(drug_exp)

# One-call pipeline: fetch → impute → convert → episodes
episodes <- run_pipeline(con, method = "baseline")
episodes[, c("person_id", "drug_name_std", "episode_start",
             "episode_end", "n_days", "median_daily_dose")]
```

## Imputation methods

| Method | Approach | Best when |
|--------|----------|-----------|
| **Baseline** | 4-step cascade (M1–M4) using structured OMOP fields (original dose, tablets×freq×strength, quantity/actual_duration [Burkard], quantity/days_supply) | OMOP structured fields well-populated |
| **NLP** | Rule-based SIG string parsing; falls back to `amount_value` when SIG omits mg; recognises QD/BID/TID plus "Once Oral", "nightly", "every evening"; `tablets` defaults to 1 when not specified | `sig` column populated |
| **Advanced NLP** | NLP + word-form tablet counts, weekly/monthly frequencies, taper decomposition | Taper schedules need per-step expansion |

## Workflow

```
1. Cohort selection   identify patients via ICD-10 / phenotype (COHORT_PERSON_IDS)
2. Fetch              fetch_drug_exposure() — drug concept IDs + patient filter
3. Impute             calc_daily_dose_baseline() or calc_daily_dose_nlp()
4. Convert            convert_pred_equiv() → prednisone-equivalent mg
5. Aggregate          build_episodes() → one row per patient–drug episode
6. Evaluate           evaluate_against_gold() vs. manually reviewed reference
```

Or use `run_pipeline()` for steps 2–5 in a single call.

## Documentation

| Page | Contents |
|------|----------|
| [Overview & Quick Start](docs/index.html) | Installation, quick start, pipeline stages |
| [Connectors](docs/connectors.html) | Live OMOP database, env vars, capability detection, column contract |
| [Methods](docs/methods.html) | Baseline (M1–M4), NLP, Advanced NLP, taper parser, SIG vocabulary |
| [Pipeline](docs/pipeline.html) | Prednisone equivalency, episode building, run_pipeline(), evaluation |
| [Reference](docs/reference.html) | Full function list, data quality issues, troubleshooting, changelog |

## Citation

Xiong C et al. *AgentDose: Automated Steroid Dose Calculation from EHR Data.*
OHDSI Global Symposium 2025. Abstract 205.
