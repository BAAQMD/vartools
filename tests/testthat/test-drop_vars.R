context("drop_vars")

test_that("single var name, present in data", {

  dropped <-
    drop_vars(mtcars, "mpg")

  expect_false(
    "mpg" %in% names(dropped))

  expect_equal(
    setdiff(names(mtcars), names(dropped)),
    "mpg")

})

test_that("multiple var names, present in data", {

  dropped <-
    drop_vars(mtcars, "mpg", "cyl")

  expect_equal(
    setdiff(names(mtcars), names(dropped)),
    c("mpg", "cyl"))

})

test_that("multiple var symbols, present in data", {

  dropped <-
    drop_vars(mtcars, mpg, cyl)

  expect_equal(
    setdiff(names(mtcars), names(dropped)),
    c("mpg", "cyl"))

})

test_that("multiple var symbols, none present in data", {

  dropped <-
    drop_vars(mtcars, foo, bar)

  expect_equal(
    names(mtcars), names(dropped))

})

test_that("multiple var symbols, some present in data", {

  dropped <-
    drop_vars(mtcars, cyl, foo)

  expect_equal(
    setdiff(names(mtcars), names(dropped)),
    "cyl")

})

test_that("vector of names, present in data", {

  vars_to_drop <- c("mpg", "cyl")

  dropped <-
    drop_vars(mtcars, !!!vars_to_drop)

  expect_equal(
    setdiff(names(mtcars), names(dropped)),
    vars_to_drop)

})

test_that("vector of names, none present in data", {

  vars_to_drop <- c("foo", "bar")

  dropped <-
    drop_vars(
      mtcars,
      !!!vars_to_drop,
      verbose = TRUE)

  expect_equal(
    names(mtcars), names(dropped))

})

