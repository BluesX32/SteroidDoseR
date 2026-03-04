# ---- build_episodes tests ----------------------------------------

make_drug_df <- function(starts, ends, doses, person = 1L, drug = "prednisone") {
  tibble::tibble(
    person_id                = person,
    drug_name_std            = drug,
    drug_exposure_start_date = as.Date(starts),
    drug_exposure_end_date   = as.Date(ends),
    daily_dose_mg_imputed    = doses
  )
}

test_that("build_episodes: two adjacent records within gap are merged", {
  df <- make_drug_df(
    starts = c("2023-01-01", "2023-02-10"),
    ends   = c("2023-02-01", "2023-03-31"),
    doses  = c(10, 10)
  )
  out <- build_episodes(df, end_col = "drug_exposure_end_date", gap_days = 30L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$episode_start, as.Date("2023-01-01"))
  expect_equal(out$episode_end,   as.Date("2023-03-31"))
  expect_equal(out$n_records, 2L)
})

test_that("build_episodes: gap > gap_days creates a new episode", {
  df <- make_drug_df(
    starts = c("2023-01-01", "2023-09-01"),
    ends   = c("2023-03-31", "2023-11-30"),
    doses  = c(10, 5)
  )
  out <- build_episodes(df, end_col = "drug_exposure_end_date", gap_days = 30L)
  expect_equal(nrow(out), 2L)
})

test_that("build_episodes: overlapping records merged into one episode", {
  df <- make_drug_df(
    starts = c("2023-01-01", "2023-02-01"),
    ends   = c("2023-04-30", "2023-03-31"),
    doses  = c(20, 15)
  )
  out <- build_episodes(df, end_col = "drug_exposure_end_date", gap_days = 30L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$episode_end, as.Date("2023-04-30"))
})

test_that("build_episodes: different patients stay separate", {
  df <- tibble::tibble(
    person_id                = c(1L, 2L),
    drug_name_std            = "prednisone",
    drug_exposure_start_date = as.Date(c("2023-01-01", "2023-01-01")),
    drug_exposure_end_date   = as.Date(c("2023-06-30", "2023-06-30")),
    daily_dose_mg_imputed    = c(10, 20)
  )
  out <- build_episodes(df, end_col = "drug_exposure_end_date")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$person_id, c(1L, 2L))
})

test_that("build_episodes: n_days computed correctly", {
  df <- make_drug_df("2023-01-01", "2023-01-31", 10)
  out <- build_episodes(df, end_col = "drug_exposure_end_date")
  expect_equal(out$n_days, 31L)
})

test_that("build_episodes: median_daily_dose aggregated across records", {
  df <- make_drug_df(
    starts = c("2023-01-01", "2023-02-01"),
    ends   = c("2023-01-31", "2023-02-28"),
    doses  = c(20, 10)
  )
  out <- build_episodes(df, end_col = "drug_exposure_end_date", gap_days = 30L)
  expect_equal(out$median_daily_dose, 15)
})

# ---- evaluate_against_gold tests -----------------------------------------

make_eval_pair <- function(comp_dose = 10, gold_dose = 10,
                           start = "2023-01-01", end = "2023-06-30") {
  computed <- tibble::tibble(
    person_id         = 1L,
    episode_start     = as.Date(start),
    episode_end       = as.Date(end),
    median_daily_dose = comp_dose
  )
  gold <- tibble::tibble(
    patient_id        = 1L,
    episode_start     = as.Date(start),
    episode_end       = as.Date(end),
    median_daily_dose = gold_dose
  )
  list(computed = computed, gold = gold)
}

test_that("evaluate_against_gold: perfect match yields MAE=0, MBE=0", {
  pair <- make_eval_pair(10, 10)
  res  <- evaluate_against_gold(pair$computed, pair$gold)
  expect_equal(res$summary$MAE, 0)
  expect_equal(res$summary$MBE, 0)
  expect_equal(res$summary$coverage_pct, 100)
})

test_that("evaluate_against_gold: bias computed correctly", {
  pair <- make_eval_pair(comp_dose = 12, gold_dose = 10)
  res  <- evaluate_against_gold(pair$computed, pair$gold)
  expect_equal(res$summary$MBE, 2)
  expect_equal(res$summary$MAE, 2)
})

test_that("evaluate_against_gold: no overlap => 0 matched, NA metrics", {
  computed <- tibble::tibble(
    person_id = 1L,
    episode_start = as.Date("2024-01-01"),
    episode_end   = as.Date("2024-06-30"),
    median_daily_dose = 10
  )
  gold <- tibble::tibble(
    patient_id = 1L,
    episode_start = as.Date("2023-01-01"),
    episode_end   = as.Date("2023-06-30"),
    median_daily_dose = 10
  )
  res <- evaluate_against_gold(computed, gold)
  expect_equal(res$summary$n_matched_periods, 0L)
  expect_equal(res$summary$coverage_pct, 0)
})

test_that("evaluate_against_gold: agreement_category 'Exact' for zero error", {
  pair <- make_eval_pair(10, 10)
  res  <- evaluate_against_gold(pair$computed, pair$gold)
  expect_equal(res$comparison$agreement_category, "Exact (<=5%)")
})

test_that("evaluate_against_gold: stratified output by dose_range present", {
  pair <- make_eval_pair(10, 10)
  res  <- evaluate_against_gold(pair$computed, pair$gold)
  expect_true("by_dose_range" %in% names(res$stratified))
  expect_true("n" %in% names(res$stratified$by_dose_range))
})

test_that("evaluate_against_gold: $summary has expected column names", {
  pair <- make_eval_pair(10, 10)
  res  <- evaluate_against_gold(pair$computed, pair$gold)
  expected_cols <- c("n_gold_periods", "n_matched_periods", "coverage_pct",
                      "MAE", "MBE", "RMSE", "median_AE", "MAPE",
                      "mean_relative_bias_pct", "pearson_corr", "spearman_corr")
  expect_true(all(expected_cols %in% names(res$summary)))
})
