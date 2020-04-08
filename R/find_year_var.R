#' @include find_vars.R

#' @describeIn find_vars Find variable named "year" or ending in "yr"
#'
#' @usage find_year_var(...)
#'
#' @export
find_year_var <- function (
  input_data,
  ...
) {

  find_var(
    input_data,
    dplyr::matches("(^year|_yr)(s?)$"),
    ...)

}
