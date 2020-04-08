#' @include find_vars.R

#' @describeIn find_vars Find variable ending in "qty"
#'
#' @usage find_qty_var(...)
#'
#' @export
find_qty_var <- function (
  input_data,
  ...
) {

  find_var(
    input_data,
    dplyr::matches("_qty$"),
    ...)

}
