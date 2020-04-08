context("find_qty_var")

test_that("stop if not found", {

  expect_error(
    find_qty_var(
      select(test_data, -foo_qty)),
    "matching")

})

test_that("return if found", {

  expect_equal(
    find_qty_var(test_data),
    "foo_qty")

})
