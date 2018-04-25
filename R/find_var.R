#' find_var
#'
#' Find a single variable, with a given suffix, among the variables in your data
#'
#' @export
find_var <- function (input_data, suffix) {

  pattern <- fixed(str_c(suffix, "$"))
  found <- select_vars(names(input_data), dplyr::matches(pattern))

  if (length(found) == 0) {

    error_msg <- str_c("No columns named xxx", suffix, " in your data.")
    #stop(str_c(error_msg, see_help_msg))
    stop(error_msg)

  } else if (length(found) > 1) {

    error_msg <- str_c("Found ", str_csv(found), " in your data. Which one should be used?")
    #stop(str_c(error_msg, see_help_msg))
    stop(error_msg)

  } else {

    return(found)

  }

}
