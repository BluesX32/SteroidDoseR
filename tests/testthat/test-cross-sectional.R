# ---- dose_at_visits() tests ---------------------------------------

make_episodes <- function(starts, ends, doses, person = 1L) {
  tibble::tibble(
    person_id       = person,
    episode_start   = as.Date(starts),
    episode_end     = as.Date(ends),
    mean_daily_dose = doses
  )
}

make_visits <- function(dates, person = 1L) {
  tibble::tibble(
    person_id  = person,
    visit_date = as.Date(dates)
  )
}

test_that("dose_at_visits: visit inside one episode returns that dose", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10)
  vs <- make_visits("2023-02-01")
  out <- dose_at_visits(ep, vs)
  expect_equal(out$dose_mg, 10)
  expect_true(out$has_coverage)
})

test_that("dose_at_visits: visit outside any episode gets no_coverage_value", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10)
  vs <- make_visits("2023-06-01")
  out <- dose_at_visits(ep, vs)
  expect_equal(out$dose_mg, 0)
  expect_false(out$has_coverage)
})

test_that("dose_at_visits: no_coverage_value can be set to NA (gold-standard use)", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10)
  vs <- make_visits("2023-06-01")
  out <- dose_at_visits(ep, vs, no_coverage_value = NA_real_)
  expect_true(is.na(out$dose_mg))
  expect_false(out$has_coverage)
})

test_that("dose_at_visits: visit exactly on episode start/end boundary is covered", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10)
  vs <- make_visits(c("2023-01-01", "2023-03-31"))
  out <- dose_at_visits(ep, vs)
  expect_equal(out$dose_mg, c(10, 10))
  expect_true(all(out$has_coverage))
})

test_that("dose_at_visits: visit one day past episode end is uncovered", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10)
  vs <- make_visits("2023-04-01")
  out <- dose_at_visits(ep, vs)
  expect_equal(out$dose_mg, 0)
  expect_false(out$has_coverage)
})

test_that("dose_at_visits: two overlapping episodes on the same date sum doses", {
  ep <- tibble::tibble(
    person_id       = 1L,
    episode_start   = as.Date(c("2023-01-01", "2023-02-01")),
    episode_end     = as.Date(c("2023-03-31", "2023-02-28")),
    mean_daily_dose = c(10, 4)
  )
  vs <- make_visits("2023-02-15")
  out <- dose_at_visits(ep, vs)
  expect_equal(out$dose_mg, 14)
  expect_true(out$has_coverage)
})

test_that("dose_at_visits: covering episode with unknown dose returns NA, not 0", {
  ep <- make_episodes("2023-01-01", "2023-03-31", NA_real_)
  vs <- make_visits("2023-02-01")
  out <- dose_at_visits(ep, vs)
  expect_true(is.na(out$dose_mg))
  expect_true(out$has_coverage)
})

test_that("dose_at_visits: different patients are not cross-matched", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10, person = 1L)
  vs <- make_visits("2023-02-01", person = 2L)
  out <- dose_at_visits(ep, vs)
  expect_equal(out$dose_mg, 0)
  expect_false(out$has_coverage)
})

test_that("dose_at_visits: preserves extra visits_df columns and row order", {
  ep <- make_episodes("2023-01-01", "2023-03-31", 10)
  vs <- tibble::tibble(
    person_id  = 1L,
    visit_date = as.Date(c("2023-06-01", "2023-02-01")),
    visit_occurrence_id = c(9001L, 9002L)
  )
  out <- dose_at_visits(ep, vs)
  expect_equal(out$visit_occurrence_id, c(9001L, 9002L))
  expect_equal(out$dose_mg, c(0, 10))
})

# ---- fetch_visit_occurrence() tests --------------------------------

visit_fixture <- tibble::tibble(
  person_id         = c(1L, 1L, 2L),
  visit_occurrence_id = c(1L, 2L, 3L),
  visit_start_date  = as.Date(c("2022-01-01", "2022-06-01", "2022-01-01")),
  visit_concept_id  = c(9202L, 9202L, 581477L)
)

test_that("fetch_visit_occurrence.data.frame returns all rows by default", {
  out <- fetch_visit_occurrence(visit_fixture)
  expect_equal(nrow(out), 3L)
})

test_that("fetch_visit_occurrence filters by visit_concept_ids", {
  out <- fetch_visit_occurrence(visit_fixture, visit_concept_ids = 9202L)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$visit_concept_id == 9202L))
})

test_that("fetch_visit_occurrence filters by person_ids", {
  out <- fetch_visit_occurrence(visit_fixture, person_ids = 2L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$person_id, 2L)
})

test_that("fetch_visit_occurrence filters by start_date and end_date", {
  out <- fetch_visit_occurrence(visit_fixture, start_date = "2022-02-01")
  expect_equal(nrow(out), 1L)
  expect_equal(out$visit_start_date, as.Date("2022-06-01"))
})

test_that("fetch_visit_occurrence errors on an unsupported input type", {
  expect_error(fetch_visit_occurrence(list(a = 1)), "data.frame or omop_connector")
})

# ---- dose_agreement_metrics() tests --------------------------------

test_that("dose_agreement_metrics: identical vectors give zero error and perfect correlation", {
  out <- dose_agreement_metrics(c(10, 20, 30, 5), c(10, 20, 30, 5))
  expect_equal(out$MAE, 0)
  expect_equal(out$MBE, 0)
  expect_equal(out$RMSE, 0)
  expect_equal(out$pearson_corr, 1)
})

test_that("dose_agreement_metrics: known bias is captured by MBE and MAE", {
  out <- dose_agreement_metrics(c(15, 25), c(10, 20))
  expect_equal(out$MAE, 5)
  expect_equal(out$MBE, 5)
})

test_that("dose_agreement_metrics: fewer than 3 pairs returns NA correlations", {
  out <- dose_agreement_metrics(c(10, 20), c(10, 15))
  expect_true(is.na(out$pearson_corr))
  expect_true(is.na(out$spearman_corr))
})

test_that("dose_agreement_metrics: NA pairs are dropped before computing metrics", {
  out <- dose_agreement_metrics(c(10, NA, 30), c(10, 20, 30))
  expect_equal(out$n, 2L)
  expect_equal(out$MAE, 0)
})
