#' @include find_var.R

#' @describeIn find_var Find variable ending in "qty"
#'
#' @usage find_qty_var(...)
#'
#' @export
find_qty_var <-
  purrr::partial(
    find_var,
    pattern = "qty$")
