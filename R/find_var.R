#' find_var
#'
#' Find a single variable, with a given suffix, among the variables in your data
#'
#' @param input_data tabular data, like a tibble
#' @param suffix character
#'
#' @return First such variable (issues warning if more than one found)
#'
#' @export
find_var <- function (
  input_data,
  suffix
) {

  pattern <-
    stringr::fixed(str_c(suffix, "$"))

  found <-
    tidyselect::vars_select(
      names(input_data),
      dplyr::matches(pattern))

  if (length(found) == 0) {

    err_msg <-
      stringr::str_c(
        "No columns named xxx",
        suffix,
        " in your data.")

    stop(err_msg)

  } else if (length(found) > 1) {

    err_msg <-
      stringr::str_c(
        "Found ",
        strtools::str_csv(found),
        " in your data. Which one should be used?")

    stop(err_msg)

  } else {

    return(found)

  }

}
