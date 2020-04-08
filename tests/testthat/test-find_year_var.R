context("find_year_var")

test_that("stop if not found", {

  expect_error(
    find_year_var(
      select(test_data, -year)),
    "matching")

})

test_that("return if found", {

  expect_equal(
    find_year_var(test_data),
    "year")

})

test_that('finds plural form ("years")', {

  expect_equal(
    find_year_var(
      rename(test_data, years = year)),
    "years")

})
