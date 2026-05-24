## Tests for hierarchical.R
##
## Each test targets a specific branch of the decision tree.
## All fixtures include route_concept_name (required: filter_oral = TRUE default).

# ---------------------------------------------------------------------------
# Fixture helper
# ---------------------------------------------------------------------------

make_hier_row <- function(
    sig               = NA_character_,
    amount_value      = 5,
    amount_unit_concept_id = 8576L,   # mg
    quantity          = 90,
    days_supply       = 90,
    start             = "2023-01-01",
    end               = "2023-03-31",
    drug_concept_name = "prednisone 5 MG oral tablet",
    route_concept_name = "Oral",
    daily_dose        = NA_real_
) {
  tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = drug_concept_name,
    route_concept_name       = route_concept_name,
    sig                      = sig,
    drug_exposure_start_date = as.Date(start),
    drug_exposure_end_date   = as.Date(end),
    amount_value             = safe_as_numeric(amount_value),
    amount_unit_concept_id   = as.integer(amount_unit_concept_id),
    quantity                 = safe_as_numeric(quantity),
    days_supply              = safe_as_numeric(days_supply),
    daily_dose_mg            = safe_as_numeric(daily_dose)
  )
}

# ---------------------------------------------------------------------------
# Branch 1: both baseline and SIG complete
# ---------------------------------------------------------------------------

test_that("branch 1 cross_checked: baseline and SIG agree (|diff| <= match_tol)", {
  # SIG: 2 tabs × 5 mg × 1/day = 10 mg/day
  # Baseline M4: 60 qty × 5 mg / 30 days = 10 mg/day
  df <- make_hier_row(
    sig          = "Take 2 tablets (10 mg total) daily",
    quantity     = 60,
    days_supply  = 30,
    amount_value = 5,
    start        = "2023-01-01",
    end          = "2023-01-31"
  )
  result <- calc_daily_dose_hierarchical(df, match_tol = 0.5)
  expect_equal(result$hierarchical_method, "cross_checked")
  expect_equal(result$daily_dose_mg, 10)
})

test_that("branch 1 nlp_override: large discrepancy → trust SIG", {
  # SIG: 40 mg daily
  # Baseline M4: 90 qty × 5 mg / 90 days = 5 mg/day
  # |diff| = 35 > diff_threshold = 10 → nlp_override
  df <- make_hier_row(
    sig          = "Take 8 tablets (40 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5
  )
  result <- calc_daily_dose_hierarchical(df, diff_threshold = 10, match_tol = 0.01)
  expect_equal(result$hierarchical_method, "nlp_override")
  expect_equal(result$daily_dose_mg, 40)
})

test_that("branch 1 blended: small discrepancy → average", {
  # SIG: 10 mg daily (explicit)
  # Baseline M4: 90 × 5 / 90 = 5 mg/day
  # |diff| = 5, diff_threshold = 10, match_tol = 0.01 → blended
  df <- make_hier_row(
    sig          = "Take 2 tablets (10 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5
  )
  result <- calc_daily_dose_hierarchical(df, diff_threshold = 10, match_tol = 0.01)
  expect_equal(result$hierarchical_method, "blended")
  expect_equal(result$daily_dose_mg, (10 + 5) / 2)
})

test_that("branch 1 diff_threshold = 0 forces nlp_override for any discrepancy", {
  df <- make_hier_row(
    sig          = "Take 2 tablets (10 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5
  )
  result <- calc_daily_dose_hierarchical(df, diff_threshold = 0, match_tol = -1)
  expect_equal(result$hierarchical_method, "nlp_override")
  expect_equal(result$daily_dose_mg, 10)
})

# ---------------------------------------------------------------------------
# Branch 2: baseline complete, SIG absent or unparseable
# ---------------------------------------------------------------------------

test_that("branch 2 baseline_only: no SIG → use baseline", {
  # M4: 90 × 5 / 90 = 5 mg/day
  df <- make_hier_row(sig = NA_character_, quantity = 90, days_supply = 90)
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(result$hierarchical_method, "baseline_only")
  expect_equal(result$daily_dose_mg, 5)
})

test_that("branch 2 baseline_only: SIG exists but unparseable → use baseline", {
  df <- make_hier_row(
    sig      = "as directed",  # free_text, no_parse
    quantity = 90, days_supply = 90
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(result$hierarchical_method, "baseline_only")
  expect_equal(result$daily_dose_mg, 5)
})

test_that("branch 2 baseline_only: SIG PRN → use baseline (PRN is not steady dosing)", {
  df <- make_hier_row(
    sig      = "Take 1 tablet (5 mg) daily as needed",
    quantity = 90, days_supply = 90
  )
  result <- calc_daily_dose_hierarchical(df)
  # prn sig_status → sig_complete = FALSE → falls to baseline_only
  expect_equal(result$hierarchical_method, "baseline_only")
})

# ---------------------------------------------------------------------------
# Branch 3a: baseline incomplete, SIG complete, steady dosing
# ---------------------------------------------------------------------------

test_that("branch 3a nlp_fills_baseline: baseline NA, SIG has full dose", {
  # No quantity/days_supply → baseline cannot compute
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "prednisone 5 MG oral tablet",
    route_concept_name       = "Oral",
    sig                      = "Take 2 tablets (10 mg total) daily",
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-03-31"),
    amount_value             = NA_real_,
    quantity                 = NA_real_,
    days_supply              = NA_real_,
    daily_dose_mg            = NA_real_
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(result$hierarchical_method, "nlp_fills_baseline")
  expect_equal(result$daily_dose_mg, 10)
})

test_that("branch 3a nlp_fills_baseline: SIG has tablets+freq, strength from amount_value", {
  # SIG: "Take 1 tablet daily" — no mg in SIG
  # amount_value = 20 mg → nlp fills: 20 × 1 × 1 = 20 mg/day
  # No quantity/days_supply → baseline incomplete
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "prednisone 20 MG oral tablet",
    route_concept_name       = "Oral",
    sig                      = "Take 1 tablet daily",
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-03-31"),
    amount_value             = 20,
    amount_unit_concept_id   = 8576L,
    quantity                 = NA_real_,
    days_supply              = NA_real_,
    daily_dose_mg            = NA_real_
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(result$hierarchical_method, "nlp_fills_baseline")
  expect_equal(result$daily_dose_mg, 20)
})

# ---------------------------------------------------------------------------
# Branch 3b: baseline incomplete, SIG is taper
# ---------------------------------------------------------------------------

test_that("branch 3b nlp_taper: baseline NA, SIG is taper", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "prednisone 10 MG oral tablet",
    route_concept_name       = "Oral",
    sig                      = "Taper by 10 mg every week starting at 60 mg",
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-06-01"),
    amount_value             = NA_real_,
    quantity                 = NA_real_,
    days_supply              = NA_real_,
    daily_dose_mg            = NA_real_
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(result$hierarchical_method, "nlp_taper")
  # sig_dose for a taper record will be NA (no computable steady dose from SIG alone)
  # so daily_dose_mg ends up NA — that is correct; taper expansion handles per-step doses
  expect_true(result$sig_taper_flag)
})

# ---------------------------------------------------------------------------
# Branch 4: baseline incomplete, no usable SIG
# ---------------------------------------------------------------------------

test_that("branch 4 missing: no baseline and no SIG → NA", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "prednisone 5 MG oral tablet",
    route_concept_name       = "Oral",
    sig                      = NA_character_,
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-03-31"),
    amount_value             = NA_real_,
    quantity                 = NA_real_,
    days_supply              = NA_real_,
    daily_dose_mg            = NA_real_
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(result$hierarchical_method, "missing")
  expect_true(is.na(result$daily_dose_mg))
})

# ---------------------------------------------------------------------------
# diff_threshold parameter sensitivity
# ---------------------------------------------------------------------------

test_that("diff_threshold = Inf always produces cross_checked or blended, never nlp_override", {
  df <- make_hier_row(
    sig          = "Take 8 tablets (40 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5
  )
  result <- calc_daily_dose_hierarchical(df, diff_threshold = Inf, match_tol = 0.01)
  expect_true(result$hierarchical_method %in% c("cross_checked", "blended"))
})

test_that("diff_threshold = 0 always nlp_override when doses differ", {
  df <- make_hier_row(
    sig          = "Take 2 tablets (10 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5    # baseline 5 vs NLP 10 → diff = 5 > 0
  )
  result <- calc_daily_dose_hierarchical(df, diff_threshold = 0, match_tol = -1)
  expect_equal(result$hierarchical_method, "nlp_override")
})

# ---------------------------------------------------------------------------
# Output schema
# ---------------------------------------------------------------------------

test_that("output contains all expected columns", {
  df <- make_hier_row(sig = "Take 1 tablet (5 mg) daily")
  result <- calc_daily_dose_hierarchical(df)
  expected_cols <- c(
    "bl_dose", "bl_method",
    "sig_dose", "sig_status", "sig_tablets", "sig_freq_per_day",
    "sig_mg_per_admin", "sig_taper_flag", "sig_duration_days",
    "daily_dose_mg", "hierarchical_method"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("bl_dose and sig_dose are preserved separately in output", {
  df <- make_hier_row(
    sig          = "Take 2 tablets (10 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_false(is.na(result$bl_dose))
  expect_false(is.na(result$sig_dose))
  # bl_dose and sig_dose should differ for this fixture
  expect_true(result$bl_dose != result$sig_dose || result$hierarchical_method == "cross_checked")
})

# ---------------------------------------------------------------------------
# Plausibility cap
# ---------------------------------------------------------------------------

test_that("plausibility cap fires for implausible doses", {
  df <- make_hier_row(
    sig          = "Take 100 tablets (500 mg total) daily",
    quantity     = 90,
    days_supply  = 90,
    amount_value = 5
  )
  expect_warning(
    result <- calc_daily_dose_hierarchical(df, max_daily_dose_mg = 200),
    regexp = "implausible|max_daily_dose"
  )
  expect_true(is.na(result$daily_dose_mg))
  expect_equal(result$hierarchical_method, "implausible")
})

# ---------------------------------------------------------------------------
# Route filter
# ---------------------------------------------------------------------------

test_that("non-oral records are excluded when filter_oral = TRUE", {
  df <- make_hier_row(
    sig                = "Take 1 tablet (5 mg) daily",
    route_concept_name = "Intravenous"
  )
  result <- calc_daily_dose_hierarchical(df, filter_oral = TRUE)
  expect_equal(nrow(result), 0L)
})

test_that("filter_oral = FALSE retains all routes", {
  df <- make_hier_row(
    sig                = "Take 1 tablet (5 mg) daily",
    route_concept_name = "Intravenous"
  )
  result <- calc_daily_dose_hierarchical(df, filter_oral = FALSE)
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# Multi-row vectorised behaviour
# ---------------------------------------------------------------------------

test_that("multi-row data frame produces one row per input row", {
  df <- dplyr::bind_rows(
    make_hier_row(sig = "Take 1 tablet (5 mg) daily", quantity = 90, days_supply = 90),
    make_hier_row(sig = NA_character_,                 quantity = 90, days_supply = 90),
    make_hier_row(sig = "Take 2 tablets (10 mg total) daily",
                  amount_value = NA_real_, quantity = NA_real_, days_supply = NA_real_)
  )
  result <- calc_daily_dose_hierarchical(df)
  expect_equal(nrow(result), 3L)
  # Each result method must be one of the valid hierarchy labels
  valid_methods <- c("cross_checked", "blended", "nlp_override",
                     "baseline_only", "nlp_fills_baseline", "nlp_taper", "missing")
  expect_true(all(result$hierarchical_method %in% valid_methods))
})
