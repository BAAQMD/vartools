#' @include find_vars.R

#' @describeIn find_vars Find variable ending in "qty"
#'
#' @export
find_qty_var <- function (
  input_data,
  ...
) {

  find_var(
    input_data,
    tidyselect::matches("_qty$"),
    ...)

}
