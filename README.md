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

## Public API

| Function | Description |
|----------|-------------|
| `calc_daily_dose_baseline()` | Structured-field cascading imputation (M1–M4). |
| `calc_daily_dose_nlp()` | Rule-based SIG text parsing. |
| `convert_pred_equiv()` | Multiply by prednisone-equivalency factors. |
| `build_episodes()` | Gap-bridge prescriptions into continuous episodes. |
| `evaluate_against_gold()` | Coverage, MAE, MBE vs. manual gold standard. |

See `vignette("baseline-workflow")`, `vignette("nlp-workflow")`, and
`vignette("evaluation-workflow")` for detailed examples.

---

## Data requirements

The package expects data frames whose columns mirror those produced by the
OMOP CDM SQL extraction. See `?calc_daily_dose_baseline` and
`?calc_daily_dose_nlp` for the required column lists. No real patient data
is shipped — only fully synthetic examples.

---

## Phase roadmap

| Phase | Status | Content |
|-------|--------|---------|
| **1** | ✅ This release | Baseline + NLP + conversion + evaluation |
| 2     | Planned | LLM agent (Claude) for complex SIG interpretation |

---

## Citation

Xiong M et al. *AgentDose: Automated Steroid Dose Calculation from EHR Data.*
OHDSI Global Symposium 2025. Abstract 205.
