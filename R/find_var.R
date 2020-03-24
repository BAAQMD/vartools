#' find_var
#'
#' Find the variable (i.e., column) matching a given pattern.
#'
#' @param input_data tabular data, like a tibble
#' @param pattern regular expression (also see [glob2rx()])
#' @param verbose display messages
#'
#' @details Warns if more than one variable in `input_data` matches `pattern`,
#'   but always returns the first such variable found. If there are no matches,
#'   `find_var()` will display an error message and stop.
#'
#' @return (character) name of variable matching `pattern`.
#'
#' @export
find_var <- function (
  input_data,
  pattern,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[find_var] ", ...)

  found <-
    tidyselect::vars_select(
      names(input_data),
      dplyr::matches(pattern))

  if (length(found) == 0) {

    err_msg <-
      stringr::str_c(
        "No columns matching ",
        pattern,
        " in your data.")

    stop(err_msg)

  } else if (length(found) > 1) {

    err_msg <-
      stringr::str_c(
        "Found ",
        strtools::str_and(found),
        " in your data. Which one should be used?")

    stop(err_msg)

  } else {

    found <- unname(found)
    return(found)

  }

}
