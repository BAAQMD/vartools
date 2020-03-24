#' @include find_var.R

#' @describeIn find_var Find variable ending in "_id"
#'
#' @usage find_id_var(...)
#'
#' @export
find_id_var <-
  purrr::partial(
    find_var,
    pattern = "_id$")
