#' Drop one or more variables from a data frame
#'
#' @param \dots passed to [tidyselect::vars_select()]
#' @param .strict logical
#' @param verbose logical
#'
#' @examples
#' drop_vars(mtcars, "cyl")
#' drop_vars(mtcars, c("mpg", "cyl"))
#' drop_vars(mtcars, mpg, cyl)
#' drop_vars(mtcars, mpg, foo, bar)
#' drop_vars(mtcars, c("mpg", "foo", "bar"))
#' v1 <- c("mpg", "cyl"); drop_vars(mtcars, !!v1)
#' v1 <- c("mpg", "cyl"); drop_vars(mtcars, !!!v1)
#' v2 <- c("mpg", "foo", "bar"); drop_vars(mtcars, !!!v2)
#'
#' @export
drop_vars <- function (
  input_data,
  ...,
  .strict = FALSE,
  verbose = FALSE
) {

  msg <- function (...) {
    if(isTRUE(verbose)) message("[drop_vars] ", ...)
  }

  input_vars <-
    names(input_data)

  vars_to_drop <-
    tidyselect::vars_select(
      input_vars,
      !!!quos(...),
      .strict = .strict)

  if (length(vars_to_drop) == 0) {
    msg("not dropping anything")
    return(input_data)
  } else {
    msg("dropping ", str_csv(vars_to_drop))
    return(select(input_data, -one_of(vars_to_drop)))
  }

}
