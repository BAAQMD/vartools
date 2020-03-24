#' @include find_var.R

#' @describeIn find_var Find variable named "year" or ending in "yr"
#'
#' @usage find_year_var(...)
#'
#' @export
find_year_var <-
  purrr::partial(
    find_var,
    pattern = "(^year|_yr)$")
