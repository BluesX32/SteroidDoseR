# data-raw/make_synthetic.R
# Generates synthetic (fully fabricated) fixtures for SteroidDoseR.
# Run this script to regenerate inst/extdata/*.csv whenever test data needs
# to change.  NO REAL PATIENT DATA is used here.

library(tibble)
library(readr)

out_dir <- here::here("inst", "extdata")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# Synthetic drug-exposure records (30 rows, 5 fictional patients)
# ---------------------------------------------------------------------------
# Columns mirror the OMOP drug_exposure extraction used in the legacy analysis.

synthetic_drug_exposure <- tribble(
  ~person_id, ~drug_concept_name,                    ~drug_source_value,
  ~drug_exposure_start_date, ~drug_exposure_end_date,
  ~quantity, ~days_supply, ~amount_value, ~amount_unit_concept_id,
  ~daily_dose, ~sig,

  # --- Patient 101: stable prednisone 10 mg/day ---------------------------
  101L, "prednisone 10 MG oral tablet", "PREDNISONE 10 MG TABLET",
  "2023-01-01", "2023-03-31", 90, 90, 10, 8576L, 10, "Take 1 tablet (10 mg) by mouth daily.",

  101L, "prednisone 10 MG oral tablet", "PREDNISONE 10 MG TABLET",
  "2023-04-01", "2023-06-30", 90, 90, 10, 8576L, 10, "Take 1 tablet (10 mg) by mouth daily.",

  # --- Patient 101: taper in second half ----------------------------------
  101L, "prednisone 5 MG oral tablet", "PREDNISONE 5 MG TABLET",
  "2023-07-01", "2023-09-30", 90, 90, 5, 8576L, NA, "Taper by 5 mg every 4 weeks to stop.",

  # --- Patient 102: methylprednisolone BID --------------------------------
  102L, "methylprednisolone 4 MG oral tablet", "METHYLPREDNISOLONE 4 MG TABLET",
  "2023-02-01", "2023-04-30", 180, 89, 4, 8576L, NA, "Take 3 tablets (12 mg total) twice daily.",

  102L, "methylprednisolone 4 MG oral tablet", "METHYLPREDNISOLONE 4 MG TABLET",
  "2023-05-01", "2023-07-31", 90, 91, 4, 8576L, NA, "Take 1 tablet (4 mg per dose) twice daily.",

  # --- Patient 102: supply_based only (no sig) ----------------------------
  102L, "methylprednisolone 4 MG oral tablet", "METHYLPREDNISOLONE 4 MG TABLET",
  "2023-08-01", "2023-10-31", 180, 92, 4, 8576L, NA, NA,

  # --- Patient 103: dexamethasone once daily ------------------------------
  103L, "dexamethasone 4 MG oral tablet", "DEXAMETHASONE 4 MG TABLET",
  "2023-03-01", "2023-05-31", 92, 92, 4, 8576L, NA, "Take 1 tablet (4 mg total) daily.",

  103L, "dexamethasone 4 MG oral tablet", "DEXAMETHASONE 4 MG TABLET",
  "2023-06-01", "2023-08-31", 46, 91, 4, 8576L, NA, "Take 0.5 tablet (2 mg total) daily.",

  # --- Patient 103: missing SIG + missing daily_dose → actual_duration (M3) or supply_based (M4) fallback ---
  103L, "dexamethasone 4 MG oral tablet", "DEXAMETHASONE 4 MG TABLET",
  "2023-09-01", "2023-11-30", 45, 90, 4, 8576L, NA, NA,

  # --- Patient 104: hydrocortisone TID ------------------------------------
  104L, "hydrocortisone 20 MG oral tablet", "HYDROCORTISONE 20 MG TABLET",
  "2023-01-15", "2023-04-14", 270, 90, 20, 8576L, 60, "Take 1 tablet (20 mg) three times daily.",

  104L, "hydrocortisone 20 MG oral tablet", "HYDROCORTISONE 20 MG TABLET",
  "2023-04-15", "2023-07-14", 180, 90, 20, 8576L, 40, "Take 1 tablet (20 mg) twice daily.",

  # --- Patient 104: "as directed" free-text SIG ---------------------------
  104L, "hydrocortisone 20 MG oral tablet", "HYDROCORTISONE 20 MG TABLET",
  "2023-07-15", "2023-10-13", 90, 90, 20, 8576L, NA, "Use as directed.",

  # --- Patient 105: prednisolone QOD (every other day) -------------------
  105L, "prednisolone 5 MG oral tablet", "PREDNISOLONE 5 MG TABLET",
  "2023-02-01", "2023-04-30", 45, 89, 5, 8576L, NA, "Take 2 tablets (10 mg per dose) every other day.",

  # --- Patient 105: overlapping records (same drug, different SIGs) ------
  105L, "prednisolone 5 MG oral tablet", "PREDNISOLONE 5 MG TABLET",
  "2023-04-15", "2023-06-30", 77, 77, 5, 8576L, NA, "Take 1 tablet (5 mg per dose) daily.",

  # --- Patient 105: gap > 30 days → new episode --------------------------
  105L, "prednisolone 5 MG oral tablet", "PREDNISOLONE 5 MG TABLET",
  "2023-09-01", "2023-11-30", 90, 91, 5, 8576L, 5, "Take 1 tablet (5 mg) daily.",

  # --- Patient 101 edge cases ---------------------------------------------
  101L, "prednisone 1 MG oral tablet", "PREDNISONE 1 MG TABLET",
  "2024-01-01", "2024-06-30", 180, 181, 1, 8576L, NA, "Take 5 tablets (5 mg total) daily.",

  101L, "prednisone 20 MG oral tablet", "PREDNISONE 20 MG TABLET",
  "2024-07-01", "2024-09-30", 90, 91, 20, 8576L, NA, "Take 1 tab (20 mg per dose) QD for 3 months.",

  # --- actual_duration (M3 Burkard formula, no days_supply) ---------------
  103L, "dexamethasone 2 MG oral tablet", "DEXAMETHASONE 2 MG TABLET",
  "2024-01-01", "2024-01-30", 30, NA, 2, 8576L, NA, NA,

  # --- PRN record ---------------------------------------------------------
  102L, "prednisone 5 MG oral tablet", "PREDNISONE 5 MG TABLET",
  "2024-01-01", "2024-01-30", 30, 30, 5, 8576L, NA, "Take 1 tablet as needed for flare.",

  # --- QID record ---------------------------------------------------------
  104L, "hydrocortisone 10 MG oral tablet", "HYDROCORTISONE 10 MG TABLET",
  "2024-01-01", "2024-01-31", 124, 31, 10, 8576L, NA, "Take 1 tablet (10 mg) four times daily.",

  # --- duration extraction ------------------------------------------------
  105L, "prednisolone 5 MG oral tablet", "PREDNISOLONE 5 MG TABLET",
  "2024-01-01", "2024-02-14", 42, 45, 5, 8576L, NA, "Take 1 tab (5 mg) daily for 6 weeks.",

  # --- three-times-daily --------------------------------------------------
  101L, "prednisone 5 MG oral tablet", "PREDNISONE 5 MG TABLET",
  "2024-02-01", "2024-02-28", 84, 28, 5, 8576L, NA, "Take 2 tablets (10 mg per dose) TID.",

  # --- 12-hourly (every 12 hours) -----------------------------------------
  102L, "methylprednisolone 8 MG oral tablet", "METHYLPREDNISOLONE 8 MG TABLET",
  "2024-03-01", "2024-04-30", 120, 61, 8, 8576L, NA, "Take 1 tablet (8 mg) every 12 hours.",

  # --- bare mg (no parens) ------------------------------------------------
  103L, "dexamethasone 4 MG oral tablet", "DEXAMETHASONE 4 MG TABLET",
  "2024-03-01", "2024-03-31", 31, 31, 4, 8576L, NA, "Take 1 tab 4 mg qd.",

  # --- mg from supply (no SIG, has days_supply) ---------------------------
  104L, "prednisone 5 MG oral tablet", "PREDNISONE 5 MG TABLET",
  "2024-04-01", "2024-06-30", 90, 91, 5, 8576L, NA, NA,

  # --- original daily_dose present ----------------------------------------
  105L, "prednisone 10 MG oral tablet", "PREDNISONE 10 MG TABLET",
  "2024-05-01", "2024-07-31", 92, 92, 10, 8576L, 10, "Take 1 tablet daily.",

  # --- negative duration (swap guard) -------------------------------------
  101L, "prednisone 5 MG oral tablet", "PREDNISONE 5 MG TABLET",
  "2024-06-30", "2024-06-01", 30, 30, 5, 8576L, NA, "Take 1 tablet (5 mg) daily.",

  # --- missing amount_value → strength from source_value -----------------
  102L, "prednisone 10 MG oral tablet", "PREDNISONE 10 MG TABLET",
  "2024-07-01", "2024-09-30", 92, 92, NA, NA, NA, "Take 1 tab (10 mg per dose) daily.",

  # --- unknown drug -------------------------------------------------------
  103L, "hydroxychloroquine 200 MG oral tablet", "HYDROXYCHLOROQUINE 200 MG TABLET",
  "2023-01-01", "2023-06-30", 180, 182, 200, 8576L, NA, "Take 1 tablet daily."
)

# Add route column
synthetic_drug_exposure$route_concept_name <- "Oral"
synthetic_drug_exposure$route_source_value <- "Oral"

readr::write_csv(synthetic_drug_exposure,
  file.path(out_dir, "synthetic_drug_exposure.csv"))

message("Written: synthetic_drug_exposure.csv (", nrow(synthetic_drug_exposure), " rows)")

# ---------------------------------------------------------------------------
# Synthetic gold standard (10 episodes, 4 patients)
# ---------------------------------------------------------------------------

synthetic_gold_standard <- tribble(
  ~patient_id, ~episode_start, ~episode_end, ~median_daily_dose,
  # patient 101: long prednisone course at 10 mg
  101L, "2023-01-01", "2023-06-30", 10,
  # patient 101: taper phase
  101L, "2023-07-01", "2023-09-30",  5,
  # patient 102: methylpred 24 mg/day (3 tabs BID × 4 mg)
  102L, "2023-02-01", "2023-04-30", 24,
  # patient 102: methylpred 8 mg/day
  102L, "2023-05-01", "2023-07-31",  8,
  # patient 103: dexamethasone 4 mg/day
  103L, "2023-03-01", "2023-05-31",  4,
  # patient 103: dexamethasone 2 mg/day
  103L, "2023-06-01", "2023-08-31",  2,
  # patient 104: hydrocortisone 60 mg/day
  104L, "2023-01-15", "2023-04-14", 60,
  # patient 104: hydrocortisone 40 mg/day
  104L, "2023-04-15", "2023-07-14", 40,
  # patient 105: prednisolone QOD
  105L, "2023-02-01", "2023-06-30",  5,
  # patient 105: gap episode
  105L, "2023-09-01", "2023-11-30",  5
)

readr::write_csv(synthetic_gold_standard,
  file.path(out_dir, "synthetic_gold_standard.csv"))

message("Written: synthetic_gold_standard.csv (", nrow(synthetic_gold_standard), " rows)")
