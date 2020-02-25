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
  verbose = TRUE
) {

  require(rlang)

  try({

    drop_vars <-
      tidyselect::eval_select(
        rlang::expr(c(...)),
        data = input_data,
        strict = .strict)

    if (length(drop_vars) > 0) {
      return(input_data[-drop_vars])
    }

  }, silent = TRUE)

  return(input_data)

}

drop_vars(mtcars, "cyl")
drop_vars(mtcars, c("mpg", "cyl"))
drop_vars(mtcars, mpg, cyl)
drop_vars(mtcars, mpg, foo, bar)
drop_vars(mtcars, mean)
drop_vars(mtcars, c("mpg", "foo", "bar"))
v1 <- c("mpg", "cyl"); drop_vars(mtcars, !!v1)
v1 <- c("mpg", "cyl"); drop_vars(mtcars, !!!v1)
v2 <- c("mpg", "foo", "bar"); drop_vars(mtcars, !!!v2)

