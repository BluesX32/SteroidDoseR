# test-connector.R
# Tests for the connector abstraction layer.
# All tests use df_connector — no live database required.

# ---------------------------------------------------------------------------
# Synthetic fixture
# ---------------------------------------------------------------------------

.make_test_df <- function() {
  tibble::tibble(
    person_id                = c(1L, 1L, 2L, 2L, 3L),
    drug_exposure_id         = 101:105,
    drug_concept_id          = c(1518254L, 1518254L, 1518254L, 40224131L, 1518254L),
    drug_concept_name        = c(
      "prednisone 5 MG oral tablet",
      "prednisone 5 MG oral tablet",
      "prednisone 10 MG oral tablet",
      "methylprednisolone 4 MG oral tablet",
      "prednisone 5 MG oral tablet"
    ),
    drug_source_value        = c(
      "PREDNISONE 5 MG TAB",
      "PREDNISONE 5 MG TAB",
      "PREDNISONE 10 MG TAB",
      "METHYLPREDNISOLONE 4 MG TAB",
      "PREDNISONE 5 MG TAB"
    ),
    drug_exposure_start_date = as.Date(c(
      "2022-01-01", "2022-03-01", "2021-06-01", "2022-01-01", "2023-01-01"
    )),
    drug_exposure_end_date   = as.Date(c(
      "2022-02-28", "2022-04-30", "2021-09-30", "2022-03-31", "2023-06-30"
    )),
    quantity                 = c(56, 56, 90, 90, 180),
    days_supply              = c(28, 28, 90, 90, 180),
    amount_value             = c(5, 5, 10, 4, 5),
    sig                      = c(
      "Take 2 tabs (10 mg total) daily",
      "Take 1 tab daily",
      "Take 1 tab BID",
      "Take 1 tab TID",
      NA_character_
    ),
    route_concept_id         = rep(4132161L, 5),
    route_concept_name       = rep("oral", 5),
    dose_unit_source_value   = rep("MG", 5)
  )
}

# ---------------------------------------------------------------------------
# create_df_connector
# ---------------------------------------------------------------------------

test_that("create_df_connector returns correct S3 class", {
  con <- create_df_connector(.make_test_df())
  expect_s3_class(con, "df_connector")
  expect_s3_class(con, "steroid_connector")
  expect_equal(con$type, "df")
})

test_that("create_df_connector stores the data frame", {
  df  <- .make_test_df()
  con <- create_df_connector(df)
  expect_equal(nrow(con$drug_df), nrow(df))
})

test_that("create_df_connector detects capabilities from columns", {
  con <- create_df_connector(.make_test_df())
  caps <- con$capabilities
  expect_true(caps$has_sig)
  expect_true(caps$has_days_supply)
  expect_true(caps$has_quantity)
  expect_true(caps$has_route)
  expect_true(caps$has_drug_source)
  expect_true(caps$has_drug_concept)
})

test_that("create_df_connector errors when person_id is missing", {
  df <- tibble::tibble(drug_exposure_start_date = Sys.Date())
  expect_error(create_df_connector(df), regexp = "person_id")
})

test_that("create_df_connector errors when start date is missing", {
  df <- tibble::tibble(person_id = 1L)
  expect_error(create_df_connector(df), regexp = "drug_exposure_start_date")
})

test_that("print.df_connector runs without error", {
  con <- create_df_connector(.make_test_df())
  expect_output(print(con), "df_connector")
})

# ---------------------------------------------------------------------------
# create_omop_connector (construction only — no DB connection)
# ---------------------------------------------------------------------------

test_that("create_omop_connector returns correct S3 class", {
  # Pass a named list as a stand-in for connectionDetails (structure check only)
  cd  <- list(dbms = "postgresql", server = "fake/db")
  con <- create_omop_connector(cd, cdm_schema = "omop")
  expect_s3_class(con, "omop_connector")
  expect_s3_class(con, "steroid_connector")
  expect_equal(con$type, "omop")
  expect_equal(con$cdm_schema, "omop")
  expect_equal(con$vocab_schema, "omop")   # defaults to cdm_schema
})

test_that("create_omop_connector errors on missing cdm_schema", {
  cd <- list(dbms = "postgresql")
  expect_error(create_omop_connector(cd, cdm_schema = ""), regexp = "cdm_schema")
})

test_that("print.omop_connector runs without error", {
  cd  <- list(dbms = "postgresql")
  con <- create_omop_connector(cd, cdm_schema = "cdm")
  expect_output(print(con), "omop_connector")
})

# ---------------------------------------------------------------------------
# detect_capabilities (df_connector)
# ---------------------------------------------------------------------------

test_that("detect_capabilities.df_connector updates connector in place", {
  df  <- .make_test_df()
  con <- create_df_connector(df)
  con2 <- detect_capabilities(con)
  expect_true(con2$capabilities$has_sig)
  expect_true(con2$capabilities$has_quantity)
})

test_that("detect_capabilities.df_connector reports absent columns correctly", {
  df  <- tibble::tibble(
    person_id                = 1L,
    drug_exposure_start_date = Sys.Date()
  )
  con  <- create_df_connector(df)
  con2 <- detect_capabilities(con)
  expect_false(con2$capabilities$has_sig)
  expect_false(con2$capabilities$has_days_supply)
  expect_false(con2$capabilities$has_quantity)
})

# ---------------------------------------------------------------------------
# with_connector (df_connector path — no DB needed)
# ---------------------------------------------------------------------------

test_that("with_connector.df_connector passes connector to fn", {
  con <- create_df_connector(.make_test_df())
  result <- with_connector(con, function(c) nrow(c$drug_df))
  expect_equal(result, 5L)
})

# ---------------------------------------------------------------------------
# fetch_drug_exposure (df_connector path)
# ---------------------------------------------------------------------------

test_that("fetch_drug_exposure.df_connector returns all rows by default", {
  con <- create_df_connector(.make_test_df())
  out <- fetch_drug_exposure(con)
  expect_equal(nrow(out), 5L)
})

test_that("fetch_drug_exposure filters by person_ids", {
  con <- create_df_connector(.make_test_df())
  out <- fetch_drug_exposure(con, person_ids = 1L)
  expect_equal(unique(out$person_id), 1L)
  expect_equal(nrow(out), 2L)
})

test_that("fetch_drug_exposure filters by drug_concept_ids", {
  con <- create_df_connector(.make_test_df())
  out <- fetch_drug_exposure(con, drug_concept_ids = 40224131L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$drug_concept_id, 40224131L)
})

test_that("fetch_drug_exposure filters by start_date", {
  con <- create_df_connector(.make_test_df())
  out <- fetch_drug_exposure(con, start_date = "2022-01-01")
  expect_true(all(out$drug_exposure_start_date >= as.Date("2022-01-01")))
})

test_that("fetch_drug_exposure filters by end_date", {
  con <- create_df_connector(.make_test_df())
  out <- fetch_drug_exposure(con, end_date = "2022-01-01")
  expect_true(all(out$drug_exposure_start_date <= as.Date("2022-01-01")))
})

test_that("fetch_drug_exposure sig_source aliases drug_source_value when sig absent", {
  df <- .make_test_df()
  df$sig <- NA_character_
  con <- create_df_connector(df)
  out <- fetch_drug_exposure(con, sig_source = "drug_source_value")
  expect_false(all(is.na(out$sig)))
  expect_equal(out$sig[[1L]], df$drug_source_value[[1L]])
})

# ---------------------------------------------------------------------------
# .resolve_drug_df — dispatch logic
# ---------------------------------------------------------------------------

test_that(".resolve_drug_df passes data.frame through unchanged", {
  df  <- .make_test_df()
  out <- SteroidDoseR:::.resolve_drug_df(df)
  expect_equal(nrow(out), nrow(df))
})

test_that(".resolve_drug_df dispatches df_connector correctly", {
  con <- create_df_connector(.make_test_df())
  out <- SteroidDoseR:::.resolve_drug_df(con)
  expect_equal(nrow(out), 5L)
})

test_that(".resolve_drug_df errors on invalid input type", {
  expect_error(
    SteroidDoseR:::.resolve_drug_df("not_a_df"),
    regexp = "data.frame|connector"
  )
})

# ---------------------------------------------------------------------------
# SQL rendering (no DB required — tests parameter substitution logic)
# ---------------------------------------------------------------------------

test_that("render_translate_sql substitutes @cdm_schema correctly", {
  skip_if_not_installed("SqlRender")
  sql_path <- system.file("sql", "extract_drug_exposure.sql",
                           package = "SteroidDoseR")
  result <- SteroidDoseR:::render_translate_sql(
    sql_path,
    params = list(
      cdm_schema     = "my_cdm",
      start_date     = "2020-01-01",
      end_date       = "2023-12-31",
      concept_filter = "",
      person_filter  = ""
    ),
    dbms = "postgresql"
  )
  expect_true(grepl("my_cdm.drug_exposure", result, fixed = TRUE))
  expect_true(grepl("2020-01-01", result, fixed = TRUE))
})

test_that("render_translate_sql includes concept filter when non-empty", {
  skip_if_not_installed("SqlRender")
  sql_path <- system.file("sql", "extract_drug_exposure.sql",
                           package = "SteroidDoseR")
  result <- SteroidDoseR:::render_translate_sql(
    sql_path,
    params = list(
      cdm_schema     = "cdm",
      start_date     = "2020-01-01",
      end_date       = "2023-12-31",
      concept_filter = "1518254,40224131",
      person_filter  = ""
    ),
    dbms = "postgresql"
  )
  expect_true(grepl("1518254", result, fixed = TRUE))
  expect_true(grepl("drug_concept_id", result, fixed = TRUE))
})

test_that("render_translate_sql omits concept filter when empty string", {
  skip_if_not_installed("SqlRender")
  sql_path <- system.file("sql", "extract_drug_exposure.sql",
                           package = "SteroidDoseR")
  result <- SteroidDoseR:::render_translate_sql(
    sql_path,
    params = list(
      cdm_schema     = "cdm",
      start_date     = "2020-01-01",
      end_date       = "2023-12-31",
      concept_filter = "",
      person_filter  = ""
    ),
    dbms = "sql server"
  )
  # When concept_filter is empty the conditional block is omitted
  expect_false(grepl("drug_concept_id IN", result, fixed = TRUE))
})

test_that("SQL includes amount_value from drug_strength join", {
  skip_if_not_installed("SqlRender")
  sql_path <- system.file("sql", "extract_drug_exposure.sql",
                           package = "SteroidDoseR")
  result <- SteroidDoseR:::render_translate_sql(
    sql_path,
    params = list(
      cdm_schema     = "cdm",
      start_date     = "2020-01-01",
      end_date       = "2023-12-31",
      concept_filter = "",
      person_filter  = ""
    ),
    dbms = "postgresql"
  )
  expect_true(grepl("amount_value",           result, fixed = TRUE))
  expect_true(grepl("drug_strength",          result, fixed = TRUE))
  expect_true(grepl("amount_unit_concept_id", result, fixed = TRUE))
  # Subquery groups by drug_concept_id to prevent duplicate rows
  expect_true(grepl("GROUP BY", result, ignore.case = TRUE))
})

# ---------------------------------------------------------------------------
# Public API connector dispatch: calc_daily_dose_baseline
# ---------------------------------------------------------------------------

test_that("calc_daily_dose_baseline accepts df_connector", {
  con <- create_df_connector(.make_test_df())
  out <- calc_daily_dose_baseline(con, m2_sig_parse = "none")
  expect_true("daily_dose_mg_imputed" %in% names(out))
  expect_true("imputation_method"     %in% names(out))
  expect_equal(nrow(out), 5L)
})

test_that("calc_daily_dose_baseline connector result equals data.frame result", {
  df  <- .make_test_df()
  con <- create_df_connector(df)
  expect_equal(
    calc_daily_dose_baseline(con, m2_sig_parse = "none"),
    calc_daily_dose_baseline(df,  m2_sig_parse = "none")
  )
})

test_that("calc_daily_dose_baseline connector respects person_ids filter", {
  con <- create_df_connector(.make_test_df())
  out <- calc_daily_dose_baseline(con, person_ids = 1L, m2_sig_parse = "none")
  expect_equal(nrow(out), 2L)
  expect_true(all(out$person_id == 1L))
})

# ---------------------------------------------------------------------------
# Public API connector dispatch: calc_daily_dose_nlp
# ---------------------------------------------------------------------------

test_that("calc_daily_dose_nlp accepts df_connector", {
  con <- create_df_connector(.make_test_df())
  out <- calc_daily_dose_nlp(con)
  expect_true("daily_dose_mg" %in% names(out))
  expect_true("parsed_status" %in% names(out))
})

test_that("calc_daily_dose_nlp connector result equals data.frame result", {
  df  <- .make_test_df()
  con <- create_df_connector(df)
  expect_equal(
    calc_daily_dose_nlp(con),
    calc_daily_dose_nlp(df)
  )
})

# ---------------------------------------------------------------------------
# Public API connector dispatch: build_episodes
# ---------------------------------------------------------------------------

test_that("build_episodes accepts df_connector", {
  df  <- .make_test_df() |>
    dplyr::mutate(
      drug_name_std         = standardize_drug_name(drug_concept_name),
      daily_dose_mg_imputed = amount_value * 2
    )
  con <- create_df_connector(df)
  out <- build_episodes(con, end_col = "drug_exposure_end_date")
  expect_s3_class(out, "data.frame")
  expect_true("episode_start" %in% names(out))
})

# ---------------------------------------------------------------------------
# run_pipeline
# ---------------------------------------------------------------------------

test_that("run_pipeline baseline returns episodes by default", {
  con <- create_df_connector(.make_test_df())
  out <- run_pipeline(con, method = "baseline", m2_sig_parse = "none")
  expect_s3_class(out, "data.frame")
  expect_true("episode_start" %in% names(out))
  expect_true("median_daily_dose" %in% names(out))
})

test_that("run_pipeline return_level='exposure' returns drug-exposure rows", {
  con <- create_df_connector(.make_test_df())
  out <- run_pipeline(con, method = "baseline", return_level = "exposure",
                      m2_sig_parse = "none")
  expect_equal(nrow(out), 5L)
  expect_true("daily_dose_mg_imputed" %in% names(out))
  expect_true("pred_equiv_mg"         %in% names(out))
})

test_that("run_pipeline nlp method returns episodes", {
  con <- create_df_connector(.make_test_df())
  out <- run_pipeline(con, method = "nlp")
  expect_s3_class(out, "data.frame")
  expect_true("episode_start" %in% names(out))
})

test_that("run_pipeline filters by person_ids", {
  con <- create_df_connector(.make_test_df())
  out <- run_pipeline(con, method = "baseline", m2_sig_parse = "none",
                      person_ids = 1L, return_level = "exposure")
  expect_true(all(out$person_id == 1L))
})