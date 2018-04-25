#' Drop one or more variables from a data frame
#'
#' @param \dots passed to [tidyselect::vars_select()]
#' @param .strict logical
#' @param verbose logical
#'
#' @examples
#' df <- data_frame(foo = 1, bar = 2)
#' drop_vars(df, foo)
#' drop_vars(df, apple)
#'
#' @export
drop_vars <- function (input_data, ..., .strict = FALSE, verbose = FALSE) {

  msg <- function (...) if(isTRUE(verbose)) message("[drop_vars] ", ...)

  input_vars <- names(input_data)
  vars_to_drop <- tidyselect::vars_select(input_vars, !!!quos(...), .strict = .strict)

  if (length(vars_to_drop) == 0) {
    msg("not dropping anything")
    return(input_data)
  } else {
    msg("dropping ", str_csv(vars_to_drop))
    return(select(input_data, -one_of(vars_to_drop)))
  }

}
