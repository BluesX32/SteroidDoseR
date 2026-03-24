test_that("parse_sig_one: explicit mg total sets daily_dose directly", {
  out <- parse_sig_one("Take 2 tablets (10 mg total) daily.")
  expect_equal(out$daily_dose_mg, 10)
  expect_true(out$mg_total_flag)
})

test_that("parse_sig_one: (X mg per dose) is the total per-administration amount", {
  # "(20 mg per dose)" means 20 mg total each time — tablets are the delivery
  # mechanism, not a multiplier. Clinical standard: dose stated in parens is
  # the per-administration total.
  out <- parse_sig_one("Take 4 tablets (20 mg per dose) by mouth daily.")
  expect_equal(out$mg_per_admin, 20)
  expect_equal(out$daily_dose_mg, 20)
})

test_that("parse_sig_one: BID gives freq_per_day = 2", {
  out <- parse_sig_one("Take 1 tablet (5 mg per dose) BID.")
  expect_equal(out$freq_per_day, 2)
  expect_equal(out$daily_dose_mg, 10)
})

test_that("parse_sig_one: TID gives freq_per_day = 3", {
  out <- parse_sig_one("Take 1 tablet (10 mg) three times daily.")
  expect_equal(out$freq_per_day, 3)
  expect_equal(out$daily_dose_mg, 30)
})

test_that("parse_sig_one: QID gives freq_per_day = 4", {
  out <- parse_sig_one("Take 1 tablet (10 mg) four times daily.")
  expect_equal(out$freq_per_day, 4)
  expect_equal(out$daily_dose_mg, 40)
})

test_that("parse_sig_one: every other day gives freq_per_day = 0.5", {
  out <- parse_sig_one("Take 2 tablets (10 mg per dose) every other day.")
  expect_equal(out$freq_per_day, 0.5)
  expect_equal(out$daily_dose_mg, 5)
})

test_that("parse_sig_one: every 12 hours gives freq = 2", {
  out <- parse_sig_one("Take 1 tablet (8 mg) every 12 hours.")
  expect_equal(out$freq_per_day, 2)
  expect_equal(out$daily_dose_mg, 16)
})

test_that("parse_sig_one: duration in days", {
  out <- parse_sig_one("Take 1 tablet (5 mg) daily for 14 days.")
  expect_equal(out$duration_days, 14)
})

test_that("parse_sig_one: duration in weeks converts to days", {
  out <- parse_sig_one("Take 1 tablet (5 mg) daily for 2 weeks.")
  expect_equal(out$duration_days, 14)
})

test_that("parse_sig_one: duration in months converts to days", {
  out <- parse_sig_one("Take 1 tablet (5 mg) daily for 3 months.")
  expect_equal(out$duration_days, 90)
})

test_that("parse_sig_one: taper_flag set for taper language", {
  out <- parse_sig_one("Taper by 1 mg every 4 weeks to stop.")
  expect_true(out$taper_flag)
  expect_equal(out$parsed_status, "taper")
})

test_that("parse_sig_one: free_text_flag for 'as directed'", {
  out <- parse_sig_one("Use as directed.")
  expect_true(out$free_text_flag)
  expect_equal(out$parsed_status, "free_text")
  expect_true(is.na(out$daily_dose_mg))
})

test_that("parse_sig_one: prn_flag for 'as needed'", {
  out <- parse_sig_one("Take 1 tablet as needed for pain.")
  expect_true(out$prn_flag)
  expect_equal(out$parsed_status, "prn")
})

test_that("parse_sig_one: bare mg without parens", {
  out <- parse_sig_one("Take 1 tab 4 mg qd.")
  expect_equal(out$daily_dose_mg, 4)
})

test_that("parse_sig_one: NA input returns empty row without error", {
  out <- parse_sig_one(NA_character_)
  expect_equal(out$parsed_status, "empty")
  expect_true(is.na(out$daily_dose_mg))
  expect_true(is.na(out$freq_per_day))
})

test_that("parse_sig_one: empty string returns empty status without error", {
  out <- parse_sig_one("")
  expect_equal(out$parsed_status, "empty")
})

test_that("parse_sig: vectorised wrapper preserves input rows", {
  df <- tibble::tibble(
    person_id = 1:3,
    sig = c("Take 1 tablet (5 mg) daily.",
            "Take 2 tabs BID",
            NA_character_)
  )
  out <- parse_sig(df)
  expect_equal(nrow(out), 3L)
  expect_true("daily_dose_mg" %in% names(out))
  expect_true("person_id"     %in% names(out))
})

test_that("calc_daily_dose_nlp: filters out non-oral routes", {
  df <- tibble::tibble(
    person_id          = 1:2,
    drug_concept_name  = c("prednisone 5 MG oral tablet", "prednisone 5 MG inhalation"),
    route_concept_name = c("Oral", "Inhalation"),
    sig                = c("Take 1 tablet (5 mg) daily.", "Inhale 2 puffs daily."),
    drug_exposure_start_date = as.Date(c("2023-01-01", "2023-01-01")),
    drug_exposure_end_date   = as.Date(c("2023-06-01", "2023-06-01"))
  )
  out <- calc_daily_dose_nlp(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$person_id, 1L)
})

test_that("calc_daily_dose_nlp: computes dose for simple oral SIG", {
  df <- tibble::tibble(
    person_id          = 1L,
    drug_concept_name  = "prednisone 5 MG oral tablet",
    route_concept_name = "Oral",
    sig                = "Take 2 tablets (10 mg total) daily.",
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-03-01")
  )
  out <- calc_daily_dose_nlp(df)
  expect_equal(out$daily_dose_mg, 10)
})

# New frequency patterns: "Once Oral", "Every evening Oral", "Nightly Oral" → freq = 1
# Tablets default to 1 when not specified in SIG.
test_that("parse_sig_one: 'Once Oral' parses as freq = 1, tablets default = 1", {
  out <- parse_sig_one("    Once Oral")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$tablets, 1)
})

test_that("parse_sig_one: 'Every evening Oral' parses as freq = 1", {
  out <- parse_sig_one("    Every evening Oral")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$tablets, 1)
})

test_that("parse_sig_one: 'Nightly Oral' parses as freq = 1", {
  out <- parse_sig_one("    Nightly Oral")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$tablets, 1)
})

test_that("parse_sig_one: tablets default = 1 when SIG gives no count (bare mg + freq)", {
  out <- parse_sig_one("5 mg daily")
  expect_equal(out$tablets, 1)
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$daily_dose_mg, 5)
})

# ── New SIG patterns added in v0.2.1 ──────────────────────────────────────────

test_that("parse_sig_one: 'in am' parses as freq = 1", {
  out <- parse_sig_one("3 tabs in am for 5 days.")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$tablets, 3)
})

test_that("parse_sig_one: 'once for 1 dose' parses as freq = 1", {
  out <- parse_sig_one("Take 1 tablet (1 mg per dose) by mouth once for 1 dose.")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$mg_per_admin, 1)
})

test_that("parse_sig_one: 'every 12 (twelve) hours' strips word parenthetical and gives freq = 2", {
  out <- parse_sig_one("Take 1 tablet (10 mg per dose) by mouth every 12 (twelve) hours for 14 days.")
  expect_equal(out$freq_per_day, 2)
  expect_equal(out$mg_per_admin, 10)
  expect_equal(out$daily_dose_mg, 20)
})

test_that("parse_sig_one: bare 'X mg.' gives freq = 1", {
  out <- parse_sig_one("10 mg.")
  expect_equal(out$freq_per_day, 1)
})

test_that("parse_sig_one: 'Take X mg by mouth' without time qualifier gives freq = 1", {
  out <- parse_sig_one("Take 40 mg by mouth.")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$daily_dose_mg, 40)
})

test_that("parse_sig_one: Spanish SIG translated before parsing", {
  # CUATRO→4 tablets, DIARIO→daily, TABLETAS→tablet
  out <- parse_sig_one("TOME CUATRO TABLETAS POR VIA ORAL A DIARIO")
  expect_equal(out$freq_per_day, 1)
  expect_equal(out$tablets, 4)
})

# ── Baseline cascade fallback in NLP ──────────────────────────────────────────

test_that("calc_daily_dose_nlp: baseline fallback fills NA when SIG is empty but quantity present", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "prednisone 5 MG oral tablet",
    route_concept_name       = "Oral",
    sig                      = NA_character_,
    quantity                 = 28,
    amount_value             = 5,
    amount_unit_concept_id   = 8576L,
    days_supply              = 28,
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-01-28")
  )
  out <- calc_daily_dose_nlp(df)
  expect_false(is.na(out$daily_dose_mg))
  expect_true(grepl("^fallback_", out$parsed_status))
})

test_that("calc_daily_dose_nlp: baseline fallback uses actual_duration when available", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "prednisone 5 MG oral tablet",
    route_concept_name       = "Oral",
    sig                      = NA_character_,
    quantity                 = 28,
    amount_value             = 5,
    amount_unit_concept_id   = 8576L,
    days_supply              = 28,
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-01-28")
  )
  out <- calc_daily_dose_nlp(df)
  # 28 tablets * 5 mg / 28 days = 5 mg/day
  expect_equal(out$daily_dose_mg, 5)
})

# ── Generalised every-N-hours (v0.2.3) ────────────────────────────────────────

test_that("parse_sig_one: every 5 hours → freq = 24/5", {
  out <- parse_sig_one("Take 1 tablet (10 mg) every 5 hours.")
  expect_equal(out$freq_per_day, 24 / 5)
  expect_equal(out$daily_dose_mg, 10 * 24 / 5)
})

test_that("parse_sig_one: every 4 hours → freq = 6 (regression)", {
  out <- parse_sig_one("Take 1 tablet (5 mg) every 4 hours.")
  expect_equal(out$freq_per_day, 6)
  expect_equal(out$daily_dose_mg, 30)
})

test_that("parse_sig_one: q5h → freq = 24/5", {
  out <- parse_sig_one("Take 10 mg q5h.")
  expect_equal(out$freq_per_day, 24 / 5)
})

# ── Generalised N-times-daily (v0.2.3) ────────────────────────────────────────

test_that("parse_sig_one: 5 times a day → freq = 5", {
  out <- parse_sig_one("Take 1 tab (4 mg) 5 times a day.")
  expect_equal(out$freq_per_day, 5)
  expect_equal(out$daily_dose_mg, 20)
})

test_that("parse_sig_one: 6 times daily → freq = 6", {
  out <- parse_sig_one("Take 1 tab (5 mg) 6 times daily.")
  expect_equal(out$freq_per_day, 6)
  expect_equal(out$daily_dose_mg, 30)
})

# ── Generalised every-N-days (v0.2.3) ─────────────────────────────────────────

test_that("parse_sig_one: every 10 days → freq = 0.1", {
  out <- parse_sig_one("Take 2 tablets (20 mg per dose) every 10 days.")
  expect_equal(out$freq_per_day, 1 / 10)
})

test_that("parse_sig_one: every 5 days → freq = 0.2", {
  out <- parse_sig_one("Take 1 tab (5 mg) every 5 days.")
  expect_equal(out$freq_per_day, 1 / 5)
})

test_that("parse_sig_one: q3d → freq = 1/3 (regression)", {
  out <- parse_sig_one("Take 10 mg q3d.")
  expect_equal(out$freq_per_day, 1 / 3)
})

# ── Generalised N-times-weekly (v0.2.3) ───────────────────────────────────────

test_that("parse_sig_one: 4 times a week → freq = 4/7", {
  out <- parse_sig_one("Take 1 tab (5 mg) 4 times a week.")
  expect_equal(out$freq_per_day, 4 / 7)
})

test_that("parse_sig_one: 5 times per week → freq = 5/7", {
  out <- parse_sig_one("Take 1 tab (10 mg) 5 times per week.")
  expect_equal(out$freq_per_day, 5 / 7)
})

# ── Generalised every-N-weeks (v0.2.3) ────────────────────────────────────────

test_that("parse_sig_one: every 2 weeks → freq = 1/14", {
  out <- parse_sig_one("Take 4 tablets (20 mg per dose) every 2 weeks.")
  expect_equal(out$freq_per_day, 1 / 14)
})

test_that("parse_sig_one: every 3 weeks → freq = 1/21", {
  out <- parse_sig_one("Take 1 tab (10 mg) every 3 weeks.")
  expect_equal(out$freq_per_day, 1 / 21)
})

# ── mg_per_day tier (v0.2.3) ──────────────────────────────────────────────────

test_that("parse_sig_one: 'X mg/day' treats as daily total, not per-tablet", {
  out <- parse_sig_one("Take 2 tablets 60 mg/day.")
  expect_equal(out$daily_dose_mg, 60)
  expect_true(out$mg_total_flag)
})

test_that("parse_sig_one: 'X mg per day' treats as daily total", {
  out <- parse_sig_one("Take 2 tablets 60 mg per day.")
  expect_equal(out$daily_dose_mg, 60)
})

test_that("parse_sig_one: '60 mg a day by mouth' parses as daily total", {
  out <- parse_sig_one("60 mg a day by mouth.")
  expect_equal(out$daily_dose_mg, 60)
})

test_that("parse_sig_one: bare mg * tablets unchanged when no /day suffix (regression)", {
  out <- parse_sig_one("Take 2 tabs 10 mg daily.")
  expect_equal(out$daily_dose_mg, 20)
})

# ── preprocess_sig: six–ten number words (v0.2.3) ────────────────────────────

test_that("parse_sig_one: 'six tablets daily' → tablets = 6", {
  out <- parse_sig_one("Take six tablets (5 mg) daily.")
  expect_equal(out$tablets, 6)
})

test_that("parse_sig_one: 'seven times a day' → freq = 7", {
  out <- parse_sig_one("Take 1 tab (1 mg) seven times a day.")
  expect_equal(out$freq_per_day, 7)
})

test_that("parse_sig_one: 'eight tablets daily' → tablets = 8", {
  out <- parse_sig_one("Take eight tablets (5 mg) daily.")
  expect_equal(out$tablets, 8)
})

# ── non-steroid exclusion regression (v0.2.4) ─────────────────────────────────

test_that("calc_daily_dose_nlp: non-steroid excluded even when filter_oral = FALSE", {
  df <- tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = "levetiracetam 500 mg oral tablet",
    route_concept_name       = "Oral",
    sig                      = "Take 1 tablet daily",
    drug_exposure_start_date = as.Date("2023-01-01"),
    drug_exposure_end_date   = as.Date("2023-03-01")
  )
  expect_warning(
    expect_equal(nrow(calc_daily_dose_nlp(df, filter_oral = FALSE)), 0L),
    "No corticosteroid records found after filtering\\."
  )
})
