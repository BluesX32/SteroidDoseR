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

test_that("supply_based (M4 default): quantity × strength / days_supply", {
  df  <- make_row(daily_dose = NA, quantity = 90, days_supply = 90, amount_value = 10,
                  tablets = NA, freq_per_day = NA)
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 10)   # 90 * 10 / 90
  expect_equal(out$imputation_method, "supply_based")
})

test_that("actual_duration (M3 default, Burkard): uses date diff when days_supply absent", {
  df  <- make_row(daily_dose = NA, quantity = 30, days_supply = NA, amount_value = 5,
                  tablets = NA, freq_per_day = NA,
                  start = "2023-01-01", end = "2023-01-30")
  out <- calc_daily_dose_baseline(df)
  # actual_duration = 30; 30 * 5 / 30 = 5
  expect_equal(out$daily_dose_mg_imputed, 5)
  expect_equal(out$imputation_method, "actual_duration")
})

# Regression: all-NA tablets/freq_per_day columns must NOT suppress SIG auto-parse.
# Root cause: guard previously checked column *existence*, so an all-NA column
# suppressed parse_sig() exactly like a fully-populated one, silently killing M2.
test_that("M2 auto-parse fires when tablets/freq_per_day columns exist but are all NA", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_source_value        = "PREDNISONE 5 MG TABLET",
    amount_value             = 5,
    quantity                 = NA_real_,
    days_supply              = NA_real_,
    daily_dose               = NA_real_,
    tablets                  = NA_real_,   # column exists but all NA
    freq_per_day             = NA_real_,   # column exists but all NA
    sig                      = "Take 2 tablets (10 mg total) daily",
    drug_exposure_start_date = "2023-01-01",
    drug_exposure_end_date   = "2023-03-31"
  )
  out <- calc_daily_dose_baseline(df, m2_sig_parse = "auto")
  # SIG must be parsed: 2 tablets * 1/day * 5 mg = 10 mg
  expect_equal(out$daily_dose_mg_imputed, 10)
  expect_equal(out$imputation_method, "tablets_freq")
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

# BUG-3: M1 accepts daily_dose_mg (Version2 column name)
test_that("M1 fires on daily_dose_mg column (Version2 naming)", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_source_value        = "PREDNISONE 5 MG TABLET",
    amount_value             = 5,
    quantity                 = 90,
    days_supply              = 90,
    drug_exposure_start_date = "2023-01-01",
    drug_exposure_end_date   = "2023-03-31",
    daily_dose_mg            = 20
  )
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 20)
  expect_equal(out$imputation_method, "original")
})

test_that("daily_dose takes precedence over daily_dose_mg when both present", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_source_value        = "PREDNISONE 5 MG TABLET",
    amount_value             = 5,
    quantity                 = 90,
    days_supply              = 90,
    drug_exposure_start_date = "2023-01-01",
    drug_exposure_end_date   = "2023-03-31",
    daily_dose               = 10,
    daily_dose_mg            = 99
  )
  out <- calc_daily_dose_baseline(df)
  expect_equal(out$daily_dose_mg_imputed, 10)
})

# BUG-2: methods order controls cascade priority
test_that("methods order is respected: supply_based before tablets_freq", {
  # Both tablets_freq and supply_based are computable; supply_based listed first -> should win
  df <- make_row(
    daily_dose = NA, tablets = 2, freq_per_day = 1, amount_value = 5,
    quantity = 90, days_supply = 90
  )
  out <- calc_daily_dose_baseline(
    df,
    methods = c("supply_based", "tablets_freq")
  )
  # supply_based: 90 * 5 / 90 = 5  (not tablets_freq: 2 * 1 * 5 = 10)
  expect_equal(out$daily_dose_mg_imputed, 5)
  expect_equal(out$imputation_method, "supply_based")
})

test_that("methods = single element only runs that method", {
  df <- make_row(daily_dose = NA, tablets = 2, freq_per_day = 1,
                 amount_value = 5, quantity = 90, days_supply = 90)
  out <- calc_daily_dose_baseline(df, methods = "supply_based")
  expect_equal(out$imputation_method, "supply_based")   # tablets_freq excluded
})

test_that("methods order: actual_duration before supply_based", {
  df <- make_row(
    daily_dose = NA, quantity = 30, days_supply = 90, amount_value = 10,
    tablets = NA, freq_per_day = NA,
    start = "2023-01-01", end = "2023-01-30"
  )
  # actual_duration (M3): 30*10/30 = 10;  supply_based (M4): 30*10/90 = 3.33
  out_default <- calc_daily_dose_baseline(
    df, methods = c("supply_based", "actual_duration")
  )
  expect_equal(out_default$imputation_method, "supply_based")

  out_flipped <- calc_daily_dose_baseline(
    df, methods = c("actual_duration", "supply_based")
  )
  expect_equal(out_flipped$imputation_method, "actual_duration")
})
