context("find_var")

test_that("stop if not found", {

  expect_error(
    find_var(test_data, pattern = "_baz"),
    "No columns matching")

})

test_that("return if found", {

  expect_equal(
    find_var(test_data, pattern = "_qty"),
    "foo_qty")

  expect_equal(
    find_var(test_data, pattern = "_unit"),
    "foo_unit")

})

test_that("stop if more than one found", {

  expect_error(
    find_var(test_data, pattern = "_bap"),
    "baz_bap and foo_bap")

})
