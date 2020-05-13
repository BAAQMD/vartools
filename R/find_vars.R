#' @name find_vars
#' @title Find matching column names
#'
#' @description Find the names of variable(s) matching a given pattern. If there are no matches,
#' display an error message and stop.
#'
#' @rdname find_vars
#'
#' @param input_data (tabular data) must have one or more columns
#' @param pattern (character) regular expression (also see [glob2rx()])
#' @param verbose (logical) display messages
#'
#' @return (character) name of variable(s) matching `pattern`.
#'
NULL

#' @describeIn find_vars Find one or more
#'
#' @importFrom purrr keep
#' @importFrom stringr str_c str_detect str_remove
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#'
#' @details `find_var()` stops if more than one variable in `input_data` matches `pattern`.
#'
#' @export
find_all_vars <- function (
  input_data,
  ...,
  suffix = NULL,
  .strict = FALSE,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[find_all_vars] ", ...)

  #
  # TODO: This `if()` clause implements backwards-compatible support
  # for `suffix = ...`. What should be done is to remove all invocations
  # of `suffix = ...` in downstream code (e.g. `inventory::with_hierarchy()`).
  #
  if (!is.null(suffix)) {

    pattern <-
      stringr::str_c(
        stringr::str_remove(suffix, "\\$$"),
        "$")

    found_vars <-
      purrr::keep(
        names(input_data),
        ~ stringr::str_detect(., pattern))

  } else {

    which_found <-
      tidyselect::eval_select(
        rlang::expr(c(...)),
        data = input_data,
        strict = .strict)

    found_vars <-
      names(input_data)[which_found]

  }

  if (length(found_vars) == 0) {

    err_msg <-
      stringr::str_c(
        "No matching columns in your data.")

    stop(err_msg)

  } else {

    found_vars <- unname(found_vars)
    return(found_vars)

  }

}

#' @describeIn find_vars Find exactly one
#'
#' @export
find_var <- function (
  input_data,
  pattern,
  suffix = NULL,
  verbose = getOption("verbose")
) {

  found_vars <-
    find_all_vars(
      input_data,
      pattern,
      suffix = suffix,
      verbose = verbose)

  if (length(found_vars) > 1) {

    err_msg <-
      stringr::str_c(
        "found_var ",
        strtools::str_and(found_vars),
        " in your data. Which one should be used?")

    stop(err_msg)

  }

  found_var <- found_vars[1]
  return(found_var)

}
