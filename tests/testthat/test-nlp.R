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
