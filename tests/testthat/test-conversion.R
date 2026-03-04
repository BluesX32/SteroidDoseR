test_that("convert_pred_equiv: prednisone factor is 1.0", {
  df <- tibble::tibble(drug_name_std = "prednisone", daily_dose_mg = 10)
  out <- convert_pred_equiv(df)
  expect_equal(out$pred_equiv_mg, 10)
  expect_equal(out$equiv_factor,  1.0)
  expect_equal(out$pred_equiv_status, "ok")
})

test_that("convert_pred_equiv: methylprednisolone 8 mg -> 10 mg equiv", {
  df <- tibble::tibble(drug_name_std = "methylprednisolone", daily_dose_mg = 8)
  out <- convert_pred_equiv(df)
  expect_equal(out$pred_equiv_mg, 10)
})

test_that("convert_pred_equiv: dexamethasone 4 mg -> 30 mg equiv", {
  df <- tibble::tibble(drug_name_std = "dexamethasone", daily_dose_mg = 4)
  out <- convert_pred_equiv(df)
  expect_equal(out$pred_equiv_mg, 30)
})

test_that("convert_pred_equiv: hydrocortisone factor is 0.25", {
  df <- tibble::tibble(drug_name_std = "hydrocortisone", daily_dose_mg = 20)
  out <- convert_pred_equiv(df)
  expect_equal(out$pred_equiv_mg, 5)
})

test_that("convert_pred_equiv: prednisolone factor is 1.0", {
  df <- tibble::tibble(drug_name_std = "prednisolone", daily_dose_mg = 7.5)
  out <- convert_pred_equiv(df)
  expect_equal(out$pred_equiv_mg, 7.5)
})

test_that("convert_pred_equiv: budesonide has NA factor and missing_factor status", {
  df <- tibble::tibble(drug_name_std = "budesonide", daily_dose_mg = 3)
  out <- convert_pred_equiv(df)
  expect_true(is.na(out$pred_equiv_mg))
  expect_equal(out$pred_equiv_status, "missing_factor")
})

test_that("convert_pred_equiv: unknown drug gets unknown_drug status and NA", {
  df <- tibble::tibble(drug_name_std = "hydroxychloroquine", daily_dose_mg = 400)
  out <- convert_pred_equiv(df)
  expect_true(is.na(out$pred_equiv_mg))
  expect_equal(out$pred_equiv_status, "unknown_drug")
})

test_that("convert_pred_equiv: custom equiv_table overrides built-in", {
  custom <- tibble::tibble(drug_name_std = "prednisone", equiv_factor = 2.0)
  df <- tibble::tibble(drug_name_std = "prednisone", daily_dose_mg = 10)
  out <- convert_pred_equiv(df, equiv_table = custom)
  expect_equal(out$pred_equiv_mg, 20)
})

test_that("convert_pred_equiv: accepts dmard_name column as fallback", {
  df <- tibble::tibble(dmard_name = "prednisone", daily_dose_mg = 5)
  expect_message(out <- convert_pred_equiv(df, drug_col = "drug_name_std"),
                 regexp = "dmard_name")
  expect_equal(out$pred_equiv_mg, 5)
})

test_that("convert_pred_equiv: standardises raw drug names before joining", {
  df <- tibble::tibble(drug_name_std = "PREDNISONE 5 MG TABLET", daily_dose_mg = 10)
  out <- convert_pred_equiv(df)
  expect_equal(out$pred_equiv_mg, 10)
})
