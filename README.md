# SteroidDoseR

Compute prednisone-equivalent daily doses from OMOP CDM corticosteroid records.

Three complementary methods — **Baseline** (structured OMOP fields), **NLP** (free-text SIG parsing), and **Advanced NLP** (taper-aware) — produce episode-level dose trajectories that can be evaluated against a manually reviewed gold standard and explored in an interactive dashboard.

---

## Prerequisites

- R ≥ 4.1
- Core dependencies are auto-installed: `dplyr`, `stringr`, `tibble`, `rlang`, `readr`, `lubridate`, `tidyr`, `purrr`
- **Live OMOP database** (optional): `DatabaseConnector` ≥ 3.0.0 and `SqlRender` ≥ 1.11.0
- **Visualization** (optional): `ggplot2`, `shiny`, `DT`
- Windows source builds require Rtools matching your R version

---

## Installation

```r
# Install from local source (recommended)
devtools::install_local("path/to/SteroidDoseR/")

# Alternative: pak handles some Windows path issues better
pak::local_install("path/to/SteroidDoseR/")

# Install optional dependencies as needed
install.packages(c("DatabaseConnector", "SqlRender"))   # live OMOP DB
install.packages(c("ggplot2", "shiny", "DT"))           # visualization
```

---

## Quick start

No database required — the package ships with 29 synthetic drug-exposure records:

```r
library(SteroidDoseR)
library(readr)

# Load bundled synthetic data
extdata  <- system.file("extdata", package = "SteroidDoseR")
drug_exp <- read_csv(file.path(extdata, "synthetic_drug_exposure.csv"),
                     show_col_types = FALSE)
con      <- create_df_connector(drug_exp)

# One-call pipeline: fetch → impute → convert → episodes
episodes <- run_pipeline(con, method = "baseline")
episodes[, c("person_id", "drug_name_std", "episode_start",
             "episode_end", "n_days", "median_daily_dose")]
```

---

## Step-by-step pipeline

For more control over each stage, call the functions individually:

### 1. Connect / load data

Three connection modes are available:

```r
# --- Mode A: from a data frame (no database required) ---
con <- create_df_connector(drug_exp)

# --- Mode B: SQL Server via DatabaseConnector (original / on-premise) ---
# Requires: DatabaseConnector, SqlRender
# Populate .env with SQL_SERVER, SQL_DATABASE, USE_WINDOWS_AUTH, etc.
con <- create_connection_from_env(".env")
con <- detect_capabilities(con)   # auto-detect available columns

# --- Mode C: Databricks via SAFER / REACH HPC (rJava + RJDBC + DBI) ---
# Requires: rJava, RJDBC, DBI (no DatabaseConnector needed)
# Populate R.env with DATABRICKS_SERVER_HOSTNAME, DATABRICKS_HTTP_PATH,
# DATABRICKS_TOKEN, DATABRICKS_CDM_SCHEMA, and DATABRICKS_JDBC_JAR.
# See the SAFER connection setup section below for HPC prerequisites.
con <- create_connection_from_safer_env("R.env")
con <- detect_capabilities(con)
```

### 2. Fetch drug-exposure records

```r
# Load the bundled steroid concept allow-list (3,839 OMOP concept IDs)
steroid_ids <- as.integer(read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

drug_df <- with_connector(con, function(active) {
  fetch_drug_exposure(active,
    drug_concept_ids = steroid_ids,
    person_ids       = NULL,          # NULL = all patients
    start_date       = "2015-01-01",
    end_date         = "2025-12-31"
  )
})
drug_df <- dplyr::mutate(drug_df,
  drug_name_std = standardize_drug_name(drug_concept_name))
```

### 3. Compute daily doses

Choose one method, or run all three to compare:

```r
# Baseline: 4-step cascade on structured OMOP fields
baseline_df <- calc_daily_dose_baseline(drug_df, m2_sig_parse = "auto")

# NLP: parse free-text SIG instructions
nlp_df      <- calc_daily_dose_nlp(drug_df)

# Advanced NLP: handles tapers and extended frequency patterns
adv_nlp_df  <- calc_daily_dose_nlp_advanced(drug_df, expand_tapers = FALSE)
```

**Key diagnostic columns produced:**

| Column | Meaning |
|--------|---------|
| `daily_dose_mg_imputed` | Best estimated daily dose (Baseline) |
| `daily_dose_mg` | Estimated daily dose (NLP / Advanced NLP) |
| `imputation_method` | Which cascade step succeeded: `original`, `tablets_freq`, `actual_duration`, `supply_based`, `missing` (Baseline) |
| `parsed_status` | `"ok"`, `"no_parse"`, `"taper"` (NLP methods) |
| `sig` | Original free-text SIG string (useful for diagnosing parse failures) |

### 4. Convert to prednisone-equivalent

```r
# Baseline output uses daily_dose_mg_imputed
baseline_eq <- convert_pred_equiv(baseline_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg_imputed"
)

# NLP output uses daily_dose_mg
nlp_eq <- convert_pred_equiv(nlp_df,
  drug_col = "drug_name_std",
  dose_col = "daily_dose_mg"
)
```

Equivalency factors: prednisone = 1.0, prednisolone = 1.0, methylprednisolone = 1.25, dexamethasone = 7.5, hydrocortisone = 0.25, triamcinolone = 1.25. Output column: `pred_equiv_mg`.

### 5. Build episodes

```r
baseline_ep <- build_episodes(baseline_eq,
  end_col  = "drug_exposure_end_date",
  dose_col = "pred_equiv_mg",
  gap_days = 30L
)
```

Adjacent or overlapping prescriptions within 30 days are merged into a single episode. Output: one row per patient–drug episode with `episode_start`, `episode_end`, `n_days`, `n_records`, `median_daily_dose`, `mean_daily_dose`.

### 6. Evaluate against a gold standard

```r
gold_std <- read_csv(file.path(extdata, "synthetic_gold_standard.csv"),
                     show_col_types = FALSE)

# Gold standard doses are in native drug units — convert to pred-equiv first.
# See docs/pipeline.html#evaluation for the full drug-mapping step.

ev <- evaluate_against_gold(baseline_ep, gold_std, gold_id_col = "patient_id")

ev$summary    # coverage_pct, MAE, MBE, RMSE, MAPE, pearson_corr
ev$comparison # one row per gold episode: gold_dose, computed_dose, agreement_category
ev$stratified # metrics by dose range (Low / Medium / High / Very High)
```

---

## Visualization

### Interactive dose review dashboard

`launch_dose_dashboard()` opens a Shiny app in the browser with:

- **Timeline tab** — dose trajectory plot overlaying all algorithms and the gold standard in distinct colours
- **Episodes tab** — episode-level summary table, downloadable as CSV
- **Raw Records tab** — prescription-level diagnostic table (SIG strings, imputation method, calculated doses) colour-coded by algorithm

```r
library(shiny); library(ggplot2); library(DT)

nlp_ep <- build_episodes(nlp_eq,
  end_col  = "drug_exposure_end_date",
  dose_col = "pred_equiv_mg",
  gap_days = 30L
)

launch_dose_dashboard(
  episode_list = list(
    "Baseline"     = baseline_ep,
    "NLP"          = nlp_ep
  ),
  raw_list = list(
    "Baseline"     = baseline_eq,   # record-level, post convert_pred_equiv
    "NLP"          = nlp_eq
  ),
  gold_std = gold_std
)
```

`raw_list` is optional. When supplied, the Raw Records tab shows the original prescription rows including `sig`, `imputation_method`, `daily_dose_mg_imputed`, and `pred_equiv_mg` — making it easy to trace exactly why a particular dose was calculated.

### Static plot

```r
p <- plot_patient_episodes(
  episode_list = list(Baseline = baseline_ep, NLP = nlp_ep),
  patient_ids  = c(101L, 102L),
  gold_std     = gold_std
)
print(p)
ggplot2::ggsave("dose_review.pdf", p, width = 12, height = 8)
```

---

## Minimum required columns

| Column | Required by | Notes |
|--------|-------------|-------|
| `person_id` | All methods | Patient identifier |
| `drug_exposure_start_date` | All methods | |
| `drug_concept_name` | All methods | Used to standardize drug name |
| `route_concept_name` or `route_source_value` | All methods | Needed for oral filter (`filter_oral = TRUE`) |
| `amount_value` + `amount_unit_concept_id` | Baseline (M2/M3/M4), NLP | `amount_unit_concept_id = 8576` (mg) required |
| `quantity` | Baseline M3/M4 | |
| `days_supply` | Baseline M4 | |
| `drug_exposure_end_date` | Baseline M3 | Optional; falls back to `Sys.Date()` |
| `daily_dose` | Baseline M1 | Pre-computed dose; optional |
| `sig` | NLP methods | Free-text prescription instructions |

---

## Imputation methods

| Method | Function | Approach | Best when |
|--------|----------|----------|-----------|
| **Baseline** | `calc_daily_dose_baseline()` | 4-step cascade (M1 original dose → M2 tablets×freq×strength → M3 quantity/duration → M4 quantity/days_supply) | Structured OMOP fields well-populated; SIG absent or unreliable |
| **NLP** | `calc_daily_dose_nlp()` | Regex SIG parsing; frequency and tablet-count extraction; strength fallback chain | `sig` column consistently populated |
| **Advanced NLP** | `calc_daily_dose_nlp_advanced()` | NLP + word-form counts, weekly/monthly frequencies, taper decomposition | Taper SIGs that need per-step expansion |

All methods apply `filter_oral = TRUE` by default and cap doses at `max_daily_dose_mg = 2000`.

---

## SAFER / REACH Databricks connection (HPC)

This section covers connecting from the Johns Hopkins SAFER / REACH HPC cluster,
where the Databricks JDBC driver is accessed via `rJava` + `RJDBC` + `DBI`
(no `DatabaseConnector` needed).

### Prerequisites

```bash
# 1. Identify Java on the HPC
ls /programs/x86_64-linux/java/
export JAVA_HOME=/programs/x86_64-linux/java/jdk1.8.0_144

# 2. Build rJava from source in /tmp (WekaFS home dir has shell issues)
mkdir -p /tmp/$USER/rjava-build && cd /tmp/$USER/rjava-build
wget https://cran.r-project.org/src/contrib/rJava_1.0-11.tar.gz
tar xzf rJava_1.0-11.tar.gz && cd rJava
sed -i 's/ test / \/usr\/bin\/test /g' configure
R CMD INSTALL /tmp/$USER/rjava-build/rJava

# 3. Add dyn.load to ~/.Rprofile so the JVM loads automatically
echo 'dyn.load("/programs/x86_64-linux/java/jdk1.8.0_144/jre/lib/amd64/server/libjvm.so")' >> ~/.Rprofile

# 4. Download Databricks JDBC driver
mkdir -p ~/jdbc && cd ~/jdbc
wget https://repo1.maven.org/maven2/com/databricks/databricks-jdbc/2.6.36/databricks-jdbc-2.6.36.jar
```

```r
# 5. Install R packages
install.packages(c("RJDBC", "DBI"))
```

### R.env file (SAFER credential file)

```ini
DATABRICKS_SERVER_HOSTNAME=adb-1234567890123456.7.azuredatabricks.net
DATABRICKS_HTTP_PATH=/sql/1.0/warehouses/abcdef1234567890
DATABRICKS_TOKEN=dapi...your-token-here...
DATABRICKS_JDBC_JAR=~/jdbc/databricks-jdbc-2.6.36.jar
DATABRICKS_CDM_SCHEMA=deid.omop
DATABRICKS_VOCAB_SCHEMA=deid.omop
DATABRICKS_RESULTS_SCHEMA=reach_users.your_username
```

Restrict permissions: `chmod 600 R.env`. Never commit this file.

### Connect and run

```r
library(SteroidDoseR)
library(dplyr)

# Load credentials and connect (uses rJava + RJDBC + DBI)
con <- create_connection_from_safer_env("R.env")
con <- detect_capabilities(con)

# Fetch steroid drug_exposure rows
steroid_ids <- as.integer(read_csv(
  system.file("extdata", "steroid_concept_ids.csv", package = "SteroidDoseR"),
  col_names = FALSE, show_col_types = FALSE
)[[1L]])

drug_df <- with_connector(con, function(active) {
  fetch_drug_exposure(active,
    drug_concept_ids = steroid_ids,
    start_date = "2015-01-01",
    end_date   = "2025-12-31"
  )
})

# All downstream pipeline functions work identically to other connection modes
episodes <- run_pipeline(drug_df, method = "baseline")

disconnect_connector(con)
```

Note: `cdm_schema` uses three-part Databricks catalog.schema format (e.g. `deid.omop`),
so table references resolve to `deid.omop.drug_exposure` etc.

---

## Documentation

| Page | Contents |
|------|----------|
| [Overview & Quick Start](docs/index.html) | Installation, quick start, pipeline stages |
| [Connectors](docs/connectors.html) | Live OMOP database, env vars, capability detection, column contract |
| [Methods](docs/methods.html) | Baseline (M1–M4), NLP, Advanced NLP, taper parser, SIG vocabulary |
| [Pipeline](docs/pipeline.html) | Prednisone equivalency, episode building, run_pipeline(), evaluation, visualization |
| [Reference](docs/reference.html) | Full function list, data quality issues, troubleshooting, changelog |

---

## Citation

Xiong C et al. *AgentDose: Automated Steroid Dose Calculation from EHR Data.*
OHDSI Global Symposium 2025. Abstract 205.
