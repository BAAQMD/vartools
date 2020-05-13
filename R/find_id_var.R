#' @include find_vars.R

#' @describeIn find_vars Find variable ending in "_id"
#'
#' @export
find_id_var <- function (
  input_data,
  ...
) {

  find_var(
    input_data,
    dplyr::matches("_id$"),
    ...)

}
