test_that("assert_required_cols passes silently when all columns present", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_invisible(assert_required_cols(df, c("a", "b")))
})

test_that("assert_required_cols errors with informative message on missing cols", {
  df <- tibble::tibble(a = 1)
  expect_error(
    assert_required_cols(df, c("a", "b", "c"), "test_df"),
    regexp = "test_df.*missing.*b.*c"
  )
})

test_that("safe_as_date handles Date input unchanged", {
  d <- as.Date("2023-01-15")
  expect_equal(safe_as_date(d), d)
})

test_that("safe_as_date parses YYYY-MM-DD strings", {
  expect_equal(safe_as_date("2023-06-01"), as.Date("2023-06-01"))
})

test_that("safe_as_date parses MM/DD/YYYY strings", {
  expect_equal(safe_as_date("06/01/2023"), as.Date("2023-06-01"))
})

test_that("safe_as_date returns NA for unparseable string with warning", {
  expect_warning(result <- safe_as_date("not-a-date"), regexp = "NA")
  expect_true(is.na(result))
})

test_that("safe_as_date handles NA input without warning", {
  expect_no_warning(result <- safe_as_date(NA_character_))
  expect_true(is.na(result))
})

test_that("standardize_drug_name maps prednisone variants", {
  expect_equal(standardize_drug_name("PREDNISONE 5 MG"), "prednisone")
  expect_equal(standardize_drug_name("Prednisone Oral Tablet 10mg"), "prednisone")
})

test_that("standardize_drug_name maps methylprednisolone correctly", {
  expect_equal(standardize_drug_name("methylprednisolone 4mg"), "methylprednisolone")
  expect_equal(standardize_drug_name("Medrol 8 MG"), "methylprednisolone")
  expect_equal(standardize_drug_name("Solu-Medrol"), "methylprednisolone")
})

test_that("standardize_drug_name does not map prednisolone to prednisone", {
  expect_equal(standardize_drug_name("prednisolone 5 mg"), "prednisolone")
})

test_that("standardize_drug_name maps dexamethasone", {
  expect_equal(standardize_drug_name("dexamethasone 4mg oral"), "dexamethasone")
  expect_equal(standardize_drug_name("Decadron"), "dexamethasone")
})

test_that("classify_route identifies oral correctly", {
  expect_equal(classify_route("Oral", NA), "oral")
  expect_equal(classify_route(NA, "tablet"), "oral")
})

test_that("classify_route identifies inhaled correctly", {
  expect_equal(classify_route("Inhalation"), "inhaled")
})

test_that("classify_route identifies injection correctly", {
  expect_equal(classify_route("Intramuscular"), "injection")
  expect_equal(classify_route(NA, "IV"), "injection")
})
