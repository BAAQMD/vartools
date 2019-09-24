context("find_var")

test_input <-
  tibble(
    foo_qty = 0,
    foo_unit = "lbs/day",
    baz_bap = 99,
    foo_bap = 88)

test_that("not found", {
  expect_error(find_var(test_input, suffix = "_baz"), "_baz in")
})

test_that("found", {
  expect_equal(find_var(test_input, suffix = "_qty"), c("foo_qty" = "foo_qty"))
  expect_equal(find_var(test_input, suffix = "_unit"), c("foo_unit" = "foo_unit"))
})

test_that("more than one (ambiguous)", {
  expect_error(find_var(test_input, suffix = "_bap"), "baz_bap, foo_bap")
})
