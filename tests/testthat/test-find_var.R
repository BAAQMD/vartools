context("find_var")

requireNamespace("tidyselect")

test_that("stop if not found", {

  expect_error(
    find_var(test_data, pattern = "_baz"),
    "matching")

})

test_that("return if found", {

  expect_equal(
    find_var(test_data, tidyselect::matches("_qty")),
    "foo_qty")

  expect_equal(
    find_var(test_data, tidyselect::matches("_unit")),
    "foo_unit")

})

test_that("stop if more than one found", {

  expect_error(
    find_var(test_data, tidyselect::matches("_bap")),
    "baz_bap and foo_bap")

})

test_that("backwards-compatible support for `suffix = ...`", {

  expect_equal(
    find_var(test_data, suffix = "_id"),
    "foo_id")

  expect_equal(
    find_var(test_data, suffix = "_id$"), # with "$" on end
    "foo_id")

})
