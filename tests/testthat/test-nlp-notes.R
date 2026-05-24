## Tests for nlp_notes.R (medspaCy clinical note parser)
##
## Integration tests that need live Python are skipped automatically in CI via
## skip_if_not(reticulate::py_module_available("medspacy")).
## Pure-logic tests mock .call_medspacy() so no Python is required.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Returns a minimal drug_df row with all required columns.
make_note_row <- function(
    daily_dose        = NA,
    amount_value      = 5,
    quantity          = 90,
    days_supply       = 90,
    start             = "2023-01-01",
    end               = "2023-03-31",
    drug_concept_name = "prednisone 5 MG oral tablet",
    route_concept_name = "Oral",   # required: filter_oral = TRUE default
    sig               = NA_character_,
    clinical_note     = NA_character_
) {
  tibble::tibble(
    person_id                = 1L,
    drug_concept_name        = drug_concept_name,
    route_concept_name       = route_concept_name,
    sig                      = sig,
    clinical_note            = clinical_note,
    drug_exposure_start_date = as.Date(start),
    drug_exposure_end_date   = as.Date(end),
    daily_dose_mg            = safe_as_numeric(daily_dose),
    amount_value             = safe_as_numeric(amount_value),
    quantity                 = safe_as_numeric(quantity),
    days_supply              = safe_as_numeric(days_supply)
  )
}

# A mock entity list returned by .call_medspacy for a positive result
entity_prednisone_40mg <- list(list(
  drug          = "prednisone",
  dose_mg       = 40,
  freq_per_day  = 1,
  duration_days = NULL,
  taper_flag    = FALSE,
  negated       = FALSE,
  uncertain     = FALSE,
  historical    = FALSE,
  section       = "assessment",
  raw_span      = "prednisone"
))

entity_negated <- list(list(
  drug          = "prednisone",
  dose_mg       = NULL,
  freq_per_day  = NULL,
  duration_days = NULL,
  taper_flag    = FALSE,
  negated       = TRUE,
  uncertain     = FALSE,
  historical    = FALSE,
  section       = "assessment",
  raw_span      = "prednisone"
))

entity_uncertain <- list(list(
  drug          = "prednisone",
  dose_mg       = 10,
  freq_per_day  = 1,
  duration_days = NULL,
  taper_flag    = FALSE,
  negated       = FALSE,
  uncertain     = TRUE,
  historical    = FALSE,
  section       = "plan",
  raw_span      = "prednisone"
))

entity_historical <- list(list(
  drug          = "prednisone",
  dose_mg       = 20,
  freq_per_day  = 1,
  duration_days = NULL,
  taper_flag    = FALSE,
  negated       = FALSE,
  uncertain     = FALSE,
  historical    = TRUE,
  section       = "history",
  raw_span      = "prednisone"
))

entity_taper <- list(list(
  drug          = "prednisone",
  dose_mg       = 40,
  freq_per_day  = 1,
  duration_days = 14,
  taper_flag    = TRUE,
  negated       = FALSE,
  uncertain     = FALSE,
  historical    = FALSE,
  section       = "plan",
  raw_span      = "prednisone"
))

entity_no_dose <- list(list(
  drug          = "prednisone",
  dose_mg       = NULL,
  freq_per_day  = NULL,
  duration_days = NULL,
  taper_flag    = FALSE,
  negated       = FALSE,
  uncertain     = FALSE,
  historical    = FALSE,
  section       = NA,
  raw_span      = "prednisone"
))

# ---------------------------------------------------------------------------
# .entities_to_row() (pure R — no Python)
# ---------------------------------------------------------------------------

test_that(".entities_to_row returns notes_ok with correct dose", {
  row <- SteroidDoseR:::.entities_to_row(entity_prednisone_40mg, "note text")
  expect_equal(row$parsed_status,  "notes_ok")
  expect_equal(row$daily_dose_mg,  40)
  expect_equal(row$freq_per_day,   1)
  expect_equal(row$note_entity_count, 1L)
  expect_false(row$note_negation_flag)
  expect_false(row$note_uncertainty_flag)
  expect_equal(row$note_section,   "assessment")
})

test_that(".entities_to_row returns notes_negated for negated entity", {
  row <- SteroidDoseR:::.entities_to_row(entity_negated, "not on prednisone")
  expect_equal(row$parsed_status,      "notes_negated")
  expect_true(row$note_negation_flag)
  expect_true(is.na(row$daily_dose_mg))
})

test_that(".entities_to_row returns notes_uncertain for hedged entity", {
  row <- SteroidDoseR:::.entities_to_row(entity_uncertain, "may be on prednisone")
  expect_equal(row$parsed_status,       "notes_uncertain")
  expect_true(row$note_uncertainty_flag)
})

test_that(".entities_to_row returns notes_historical for past-tense entity", {
  row <- SteroidDoseR:::.entities_to_row(entity_historical, "was on prednisone")
  expect_equal(row$parsed_status, "notes_historical")
  expect_true(is.na(row$daily_dose_mg))
})

test_that(".entities_to_row returns notes_no_dose when dose is absent", {
  row <- SteroidDoseR:::.entities_to_row(entity_no_dose, "on prednisone")
  expect_equal(row$parsed_status, "notes_no_dose")
  expect_true(is.na(row$daily_dose_mg))
})

test_that(".entities_to_row returns notes_no_drug for empty entity list", {
  row <- SteroidDoseR:::.entities_to_row(list(), "no steroids")
  expect_equal(row$parsed_status,   "notes_no_drug")
  expect_equal(row$note_entity_count, 0L)
  expect_true(is.na(row$daily_dose_mg))
})

test_that(".entities_to_row sets taper_flag correctly", {
  row <- SteroidDoseR:::.entities_to_row(entity_taper, "taper prednisone")
  expect_true(row$taper_flag)
  expect_equal(row$duration_days, 14)
})

test_that(".entities_to_row computes daily_dose_mg from dose * freq", {
  entities <- list(list(
    drug = "prednisone", dose_mg = 10, freq_per_day = 2,
    duration_days = NULL, taper_flag = FALSE,
    negated = FALSE, uncertain = FALSE, historical = FALSE,
    section = NA, raw_span = "prednisone"
  ))
  row <- SteroidDoseR:::.entities_to_row(entities, "prednisone 10 mg twice daily")
  expect_equal(row$daily_dose_mg, 20)
})

test_that(".entities_to_row assumes daily when freq is NULL but dose is present", {
  entities <- list(list(
    drug = "prednisone", dose_mg = 15, freq_per_day = NULL,
    duration_days = NULL, taper_flag = FALSE,
    negated = FALSE, uncertain = FALSE, historical = FALSE,
    section = NA, raw_span = "prednisone"
  ))
  row <- SteroidDoseR:::.entities_to_row(entities, "prednisone 15 mg")
  expect_equal(row$daily_dose_mg, 15)  # dose assumed daily
})

# ---------------------------------------------------------------------------
# parse_note_one() (mocked .call_medspacy)
# ---------------------------------------------------------------------------

test_that("parse_note_one returns empty row for NA input", {
  local_mocked_bindings(.call_medspacy = function(text) list(), .package = "SteroidDoseR")
  row <- parse_note_one(NA_character_)
  expect_equal(row$parsed_status, "empty")
  expect_equal(nrow(row), 1L)
  expect_true(is.na(row$daily_dose_mg))
})

test_that("parse_note_one returns empty row for blank string", {
  local_mocked_bindings(.call_medspacy = function(text) list(), .package = "SteroidDoseR")
  row <- parse_note_one("   ")
  expect_equal(row$parsed_status, "empty")
})

test_that("parse_note_one returns notes_ok with correct dose (mocked)", {
  local_mocked_bindings(
    .call_medspacy = function(text) entity_prednisone_40mg,
    .package = "SteroidDoseR"
  )
  row <- parse_note_one("Patient on prednisone 40 mg daily.")
  expect_equal(row$parsed_status, "notes_ok")
  expect_equal(row$daily_dose_mg, 40)
  expect_false(row$note_negation_flag)
})

test_that("parse_note_one never throws on Python error", {
  local_mocked_bindings(
    .call_medspacy = function(text) stop("simulated Python crash"),
    .package = "SteroidDoseR"
  )
  row <- parse_note_one("patient on prednisone")
  # tryCatch in parse_note_one catches the error → returns empty row
  expect_equal(row$parsed_status, "error")
  expect_true(is.na(row$daily_dose_mg))
})

test_that("parse_note_one output has all expected columns", {
  local_mocked_bindings(
    .call_medspacy = function(text) entity_prednisone_40mg,
    .package = "SteroidDoseR"
  )
  row <- parse_note_one("prednisone 40 mg daily")
  expected_cols <- c(
    "sig_raw", "tablets", "freq_per_day", "mg_per_admin", "mg_total_flag",
    "duration_days", "taper_flag", "prn_flag", "free_text_flag",
    "daily_dose_mg", "parsed_status",
    "note_entity_count", "note_negation_flag", "note_uncertainty_flag", "note_section"
  )
  expect_true(all(expected_cols %in% names(row)))
})

# ---------------------------------------------------------------------------
# parse_notes() vectorised wrapper (mocked)
# ---------------------------------------------------------------------------

test_that("parse_notes appends columns without duplicating note column", {
  df <- tibble::tibble(
    person_id     = 1:2,
    clinical_note = c("prednisone 40 mg daily", "no steroids"),
    route_concept_name = "Oral"
  )
  local_mocked_bindings(
    .call_medspacy = function(text) {
      if (grepl("prednisone", text)) entity_prednisone_40mg else list()
    },
    .package = "SteroidDoseR"
  )
  result <- parse_notes(df, note_col = "clinical_note")
  expect_equal(nrow(result), 2L)
  expect_true("daily_dose_mg" %in% names(result))
  expect_true("clinical_note" %in% names(result))  # original column preserved
  expect_false("sig_raw" %in% names(result))        # duplicate dropped
})

# ---------------------------------------------------------------------------
# calc_daily_dose_nlp_notes() integration (mocked)
# ---------------------------------------------------------------------------

test_that("calc_daily_dose_nlp_notes resolves SIG via regex (no medspaCy needed)", {
  df <- make_note_row(
    sig           = "Take 2 tablets (10 mg) daily",
    clinical_note = NA_character_
  )
  # medspaCy should NOT be called when regex succeeds
  mock_called <- FALSE
  local_mocked_bindings(
    .call_medspacy = function(text) {
      mock_called <<- TRUE
      list()
    },
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df)
  expect_false(mock_called)
  expect_equal(result$daily_dose_mg, 10)
  expect_equal(result$parsed_status, "ok")
})

test_that("calc_daily_dose_nlp_notes falls back to medspaCy for no_parse rows", {
  df <- make_note_row(
    sig           = NA_character_,
    clinical_note = "Patient continued on prednisone 40 mg daily."
  )
  local_mocked_bindings(
    .call_medspacy = function(text) entity_prednisone_40mg,
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df)
  expect_equal(result$daily_dose_mg, 40)
  expect_equal(result$parsed_status, "notes_ok")
})

test_that("calc_daily_dose_nlp_notes applies plausibility cap (mocked)", {
  df <- make_note_row(clinical_note = "prednisone 5000 mg daily")
  entities_huge <- list(list(
    drug = "prednisone", dose_mg = 5000, freq_per_day = 1,
    duration_days = NULL, taper_flag = FALSE,
    negated = FALSE, uncertain = FALSE, historical = FALSE,
    section = NA, raw_span = "prednisone"
  ))
  local_mocked_bindings(
    .call_medspacy = function(text) entities_huge,
    .package = "SteroidDoseR"
  )
  expect_warning(
    result <- calc_daily_dose_nlp_notes(df, max_daily_dose_mg = 2000),
    regexp = "implausible|max_daily_dose"
  )
  expect_true(is.na(result$daily_dose_mg))
  expect_equal(result$parsed_status, "implausible")
})

test_that("calc_daily_dose_nlp_notes fires baseline fallback when both SIG and note fail", {
  df <- make_note_row(
    sig           = NA_character_,
    clinical_note = "No steroids documented.",
    quantity      = 90,
    days_supply   = 90,
    amount_value  = 5
  )
  local_mocked_bindings(
    .call_medspacy = function(text) list(),
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df)
  # baseline supply_based: 90 qty * 5 mg / 90 days_supply = 5 mg/day
  expect_equal(result$daily_dose_mg, 5)
  expect_true(grepl("^fallback_", result$parsed_status))
})

test_that("calc_daily_dose_nlp_notes excludes non-oral routes (filter_oral = TRUE)", {
  df <- make_note_row(
    clinical_note      = "methylprednisolone IV",
    route_concept_name = "Intravenous"
  )
  local_mocked_bindings(
    .call_medspacy = function(text) entity_prednisone_40mg,
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df, filter_oral = TRUE)
  expect_equal(nrow(result), 0L)
})

test_that("calc_daily_dose_nlp_notes note note_section propagates correctly", {
  df <- make_note_row(clinical_note = "Assessment: prednisone 40 mg daily.")
  local_mocked_bindings(
    .call_medspacy = function(text) entity_prednisone_40mg,
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df)
  expect_equal(result$note_section, "assessment")
})

test_that("calc_daily_dose_nlp_notes returns negation flag correctly", {
  df <- make_note_row(clinical_note = "Patient is NOT on any steroids.")
  local_mocked_bindings(
    .call_medspacy = function(text) entity_negated,
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df)
  # negated → no dose from notes; will fall to baseline
  expect_true(result$note_negation_flag %in% c(TRUE, NA))
})

test_that("calc_daily_dose_nlp_notes missing note column is handled gracefully", {
  df <- make_note_row(sig = "Take 1 tablet daily")
  df$clinical_note <- NULL  # remove note column entirely
  local_mocked_bindings(
    .call_medspacy = function(text) stop("should not be called"),
    .package = "SteroidDoseR"
  )
  result <- calc_daily_dose_nlp_notes(df, note_col = "clinical_note")
  # SIG regex should handle this without needing notes
  expect_equal(nrow(result), 1L)
  expect_true("daily_dose_mg" %in% names(result))
})

# ---------------------------------------------------------------------------
# Integration tests (skipped without medspaCy)
# ---------------------------------------------------------------------------

test_that("parse_note_one extracts dose from real clinical text (integration)", {
  skip_if_not(
    requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_module_available("medspacy"),
    "medspaCy not available"
  )
  row <- parse_note_one("Patient continued on prednisone 40 mg daily.")
  expect_equal(row$parsed_status, "notes_ok")
  expect_equal(row$daily_dose_mg, 40)
})

test_that("parse_note_one detects negation in real clinical text (integration)", {
  skip_if_not(
    requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_module_available("medspacy"),
    "medspaCy not available"
  )
  row <- parse_note_one("Patient is not on prednisone or any other steroids.")
  expect_equal(row$parsed_status, "notes_negated")
})
