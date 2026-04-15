# EligibilityAnalysis.R
# Tracks how many patients/episodes survive each step of the pipeline,
# and identifies who was dropped and why.
#
# Prerequisite: run CodeToRun.R first so these objects are in session:
#   drug_df           — raw fetched drug_exposure records
#   baseline_df       — record-level baseline output (post oral filter)
#   baseline_episodes — built episodes (Baseline, pred-equiv)
#   nlp_episodes      — built episodes (NLP, pred-equiv)
#   adv_nlp_episodes  — built episodes (Advanced NLP, pred-equiv)
#   gold_std          — gold standard (with pred-equiv doses and drug_name_std)
#   ev_baseline / ev_nlp / ev_adv — evaluate_against_gold() results
#
# Usage: Source interactively in RStudio after CodeToRun.R.
#   source("extras/EligibilityAnalysis.R")

if (!interactive()) quit(status = 0L, save = "no")

library(SteroidDoseR)
library(dplyr)

# ---------------------------------------------------------------------------
# 0. Alias session objects
# ---------------------------------------------------------------------------
# baseline_df and nlp_df from run_analysis.R are already oral-filtered
# (filter_oral = TRUE is the default and is set explicitly in run_analysis.R).
# Use them directly to avoid re-running expensive queries.
baseline_df_oral <- baseline_df   # filter_oral = TRUE already applied in run_analysis.R
nlp_df_oral      <- nlp_df        # filter_oral = TRUE is the default

# ---------------------------------------------------------------------------
# Helper: compact funnel printer
# ---------------------------------------------------------------------------
funnel_row <- function(label, n_patients, n_episodes = NA_integer_) {
  ep_str <- if (!is.na(n_episodes)) sprintf(" | %d episodes", n_episodes) else ""
  cat(sprintf("  %-45s %d patients%s\n", label, n_patients, ep_str))
}

# ===========================================================================
# 1. DATABASE FUNNEL
# ===========================================================================
cat(strrep("=", 70), "\n")
cat("1. DATABASE FUNNEL — patients/episodes at each pipeline step\n")
cat(strrep("-", 70), "\n")

pts_raw      <- dplyr::n_distinct(drug_df$person_id)
pts_baseline <- dplyr::n_distinct(baseline_df_oral$person_id)
pts_nlp      <- dplyr::n_distinct(nlp_df_oral$person_id)

ep_baseline  <- dplyr::n_distinct(baseline_episodes$person_id)
ep_nlp       <- dplyr::n_distinct(nlp_episodes$person_id)
ep_adv       <- dplyr::n_distinct(adv_nlp_episodes$person_id)

funnel_row("1. Fetched from DB (all steroid concept IDs)", pts_raw)
funnel_row("2. After oral filter + drug standardise (Baseline)", pts_baseline,
           nrow(baseline_df_oral))
funnel_row("3. After oral filter + drug standardise (NLP)", pts_nlp,
           nrow(nlp_df_oral))
funnel_row("4. With ≥1 episode — Baseline", ep_baseline,
           nrow(baseline_episodes))
funnel_row("4. With ≥1 episode — NLP", ep_nlp,
           nrow(nlp_episodes))
funnel_row("4. With ≥1 episode — Adv NLP", ep_adv,
           nrow(adv_nlp_episodes))
cat("\n")

# Patients dropped at oral filter step (in raw but not in baseline_df_oral)
dropped_oral <- setdiff(
  unique(drug_df$person_id),
  unique(baseline_df_oral$person_id)
)
cat(sprintf(
  "Dropped by oral/drug filter: %d patients (%d → %d)\n",
  length(dropped_oral), pts_raw, pts_baseline
))
if (length(dropped_oral) > 0L) {
  cat("  Drugs seen for these patients (top 10):\n")
  drug_df |>
    dplyr::filter(person_id %in% dropped_oral) |>
    dplyr::count(drug_name_std, sort = TRUE) |>
    head(10) |>
    print()
}

# ===========================================================================
# 2. GOLD STANDARD FUNNEL
# ===========================================================================
cat(strrep("=", 70), "\n")
cat("2. GOLD STANDARD FUNNEL — patients/episodes at each step\n")
cat(strrep("-", 70), "\n")

gold_pts_all     <- dplyr::n_distinct(gold_std$patient_id)
gold_eps_all     <- nrow(gold_std)

gold_in_db       <- intersect(unique(gold_std$patient_id), unique(drug_df$person_id))
gold_in_baseline <- intersect(unique(gold_std$patient_id), unique(baseline_episodes$person_id))
gold_in_nlp      <- intersect(unique(gold_std$patient_id), unique(nlp_episodes$person_id))
gold_in_adv      <- intersect(unique(gold_std$patient_id), unique(adv_nlp_episodes$person_id))

funnel_row("1. Gold standard total", gold_pts_all, gold_eps_all)
funnel_row("2. Gold patients found in DB", length(gold_in_db))
funnel_row("3. Gold patients with Baseline episodes", length(gold_in_baseline))
funnel_row("3. Gold patients with NLP episodes", length(gold_in_nlp))
funnel_row("3. Gold patients with Adv NLP episodes", length(gold_in_adv))
cat("\n")

matched_baseline <- sum(!is.na(ev_baseline$comparison$computed_dose))
matched_nlp      <- sum(!is.na(ev_nlp$comparison$computed_dose))
matched_adv      <- sum(!is.na(ev_adv$comparison$computed_dose))

cat(sprintf(
  "Gold episodes matched (episode-level overlap):\n  Baseline: %d/%d  |  NLP: %d/%d  |  Adv NLP: %d/%d\n\n",
  matched_baseline, gold_eps_all,
  matched_nlp,      gold_eps_all,
  matched_adv,      gold_eps_all
))

# ===========================================================================
# 3. PATIENTS IN GOLD BUT NOT IN DB
# ===========================================================================
cat(strrep("=", 70), "\n")
cat("3. GOLD PATIENTS MISSING FROM DATABASE\n")
cat(strrep("-", 40), "\n")

gold_not_in_db <- setdiff(unique(gold_std$patient_id), unique(drug_df$person_id))
cat(sprintf("%d gold patients have no records in the extracted DB window.\n",
            length(gold_not_in_db)))

if (length(gold_not_in_db) > 0L) {
  cat("  Their gold episodes:\n")
  gold_std |>
    dplyr::filter(patient_id %in% gold_not_in_db) |>
    dplyr::select(patient_id, episode_start, episode_end,
                  drug_name_std, median_daily_dose) |>
    print(n = 50)
}

# ===========================================================================
# 4. PATIENTS IN GOLD, IN DB, BUT NOT MATCHED IN EVALUATION
# ===========================================================================
cat(strrep("=", 70), "\n")
cat("4. GOLD PATIENTS IN DB BUT NOT MATCHED IN EVALUATION (Baseline)\n")
cat(strrep("-", 40), "\n")

unmatched_baseline <- ev_baseline$comparison |>
  dplyr::filter(is.na(computed_dose)) |>
  dplyr::select(patient_id, episode_start, episode_end, gold_dose)

cat(sprintf(
  "%d gold episodes unmatched by Baseline (patient in DB but no overlapping episode).\n",
  nrow(unmatched_baseline)
))

if (nrow(unmatched_baseline) > 0L) {
  cat("\n  Unmatched gold episodes:\n")
  print(as.data.frame(unmatched_baseline), row.names = FALSE)

  cat("\n  Their raw drug_df records (first 30):\n")
  drug_df |>
    dplyr::filter(person_id %in% unique(unmatched_baseline$patient_id)) |>
    dplyr::arrange(person_id, drug_exposure_start_date) |>
    dplyr::select(person_id, drug_name_std, drug_exposure_start_date,
                  drug_exposure_end_date,
                  dplyr::any_of(c("imputation_method", "daily_dose_mg_imputed",
                                   "amount_value", "quantity", "days_supply"))) |>
    head(30) |>
    print()
}

# ===========================================================================
# 5. SUMMARY TABLE
# ===========================================================================
cat(strrep("=", 70), "\n")
cat("5. SUMMARY TABLE\n")
cat(strrep("-", 40), "\n")

summary_tbl <- tibble::tibble(
  Step = c(
    "DB: raw records (all steroids)",
    "DB: after oral/drug filter",
    "DB: with >=1 episode (Baseline)",
    "DB: with >=1 episode (NLP)",
    "DB: with >=1 episode (Adv NLP)",
    "Gold: total",
    "Gold: found in DB",
    "Gold: with Baseline episode",
    "Gold: with NLP episode",
    "Gold: with Adv NLP episode",
    "Gold eps matched (Baseline)",
    "Gold eps matched (NLP)",
    "Gold eps matched (Adv NLP)"
  ),
  Patients = c(
    pts_raw, pts_baseline,
    ep_baseline, ep_nlp, ep_adv,
    gold_pts_all, length(gold_in_db),
    length(gold_in_baseline), length(gold_in_nlp), length(gold_in_adv),
    NA_integer_, NA_integer_, NA_integer_
  ),
  Episodes = c(
    nrow(drug_df), nrow(baseline_df_oral),
    nrow(baseline_episodes), nrow(nlp_episodes), nrow(adv_nlp_episodes),
    gold_eps_all, NA_integer_,
    NA_integer_, NA_integer_, NA_integer_,
    matched_baseline, matched_nlp, matched_adv
  )
)

print(as.data.frame(summary_tbl), row.names = FALSE, na.print = "—")
cat(strrep("=", 70), "\n")
