context("find_qty_var")

test_that("stop if not found", {

  expect_error(
    find_year_var(
      select(test_data, -year)),
    "No columns matching")

})

test_that("return if found", {

  expect_equal(
    find_year_var(test_data),
    "year")

})