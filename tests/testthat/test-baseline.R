make_row <- function(daily_dose = NA, amount_value = 5, quantity = 90,
                     days_supply = 90, tablets = NA, freq_per_day = NA,
                     start = "2023-01-01", end = "2023-03-31",
                     drug_source_value = "PREDNISONE 5 MG TABLET") {
  tibble::tibble(
    person_id                = 1L,
    drug_source_value        = drug_source_value,
    amount_value             = amount_value,
    quantity                 = quantity,
    days_supply              = days_supply,
    tablets                  = tablets,
    freq_per_day             = freq_per_day,
    drug_exposure_start_date = start,
    drug_exposure_end_date   = end,
    daily_dose               = daily_dose
  )
}

test_that("M1 original: uses pre-computed daily_dose when present and > 0", {
  df  <- make_row(daily_dose = 10)
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 10)
  expect_equal(out$imputation_method, "original")
})

test_that("M1 skipped when daily_dose is NA; falls to M2", {
  df  <- make_row(daily_dose = NA, tablets = 2, freq_per_day = 1, amount_value = 5)
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 10)   # 2 * 1 * 5
  expect_equal(out$imputation_method, "tablets_freq")
})

test_that("M2 uses tablets × freq × strength correctly", {
  df  <- make_row(daily_dose = NA, tablets = 3, freq_per_day = 2, amount_value = 4)
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 24)   # 3 * 2 * 4
  expect_equal(out$imputation_method, "tablets_freq")
})

test_that("M3 supply_based: quantity × strength / days_supply", {
  df  <- make_row(daily_dose = NA, quantity = 90, days_supply = 90, amount_value = 10,
                  tablets = NA, freq_per_day = NA)
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 10)   # 90 * 10 / 90
  expect_equal(out$imputation_method, "supply_based")
})

test_that("M4 actual_duration: uses date diff when days_supply absent", {
  df  <- make_row(daily_dose = NA, quantity = 30, days_supply = NA, amount_value = 5,
                  tablets = NA, freq_per_day = NA,
                  start = "2023-01-01", end = "2023-01-30")
  out <- calc_daily_dose_baseline(df)
  # actual_duration = 30; 30 * 5 / 30 = 5
  expect_equal(out$daily_dose_mg_imputed, 5)
  expect_equal(out$imputation_method, "actual_duration")
})

test_that("'missing' when no method can provide a dose", {
  df  <- make_row(daily_dose = NA, quantity = NA, days_supply = NA, amount_value = NA,
                  tablets = NA, freq_per_day = NA)
  out <- calc_daily_dose_baseline(df)
  expect_true(is.na(out$daily_dose_mg_imputed))
  expect_equal(out$imputation_method, "missing")
})

test_that("strength_mg extracted from drug_source_value when amount_value is NA", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_source_value        = "PREDNISONE 10 MG TABLET",
    amount_value             = NA_real_,
    quantity                 = 90,
    days_supply              = 90,
    daily_dose               = NA_real_,
    drug_exposure_start_date = "2023-01-01",
    drug_exposure_end_date   = "2023-03-31"
  )
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$strength_mg, 10)
  expect_equal(out$daily_dose_mg_imputed, 10)   # 90*10/90
})

test_that("negative duration (start > end) still handled gracefully", {
  df <- make_row(daily_dose = NA, quantity = 30, days_supply = NA, amount_value = 5,
                 tablets = NA, freq_per_day = NA,
                 start = "2023-06-30", end = "2023-06-01")
  # actual_duration would be negative -> NA -> falls to "missing"
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$imputation_method, "missing")
})

test_that("imputation_method column always present in output", {
  df <- make_row(daily_dose = 5)
  out <- calc_daily_dose_baseline(df)
  expect_true("imputation_method" %in% names(out))
  expect_true("daily_dose_mg_imputed" %in% names(out))
  expect_true("strength_mg" %in% names(out))
})
