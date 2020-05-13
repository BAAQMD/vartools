#' @include find_vars.R

#' @describeIn find_vars Find variable named "year" or ending in "yr"
#'
#' @export
find_year_var <- function (
  input_data,
  ...
) {

  find_var(
    input_data,
    tidyselect::matches("(^year|_yr)(s?)$"),
    ...)

}
