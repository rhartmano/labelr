#' Determine If Vector Belongs to Any of Specified Classes
#'
#' @description
#' `check_class` determines whether a vector's class is among those specified.
#'
#' @details
#' By default (strict = TRUE), if a vector is of multiple classes, all of its
#' classes must be among those specified via the classes argument.
#'
#' @param x the vector to check against specified classes.
#' @param classes a character vector of classes against which x is checked.
#' @param strict If TRUE, all of x's classes must be among those specified
#' in the classes argument. If FALSE, at least one but not necessarily all of
#' x's classes must be among those specified in the classes argument.
#'
#' @return a 1L logical vector indicating whether x's class is found among those
#' passed to the classes argument.
#' @export
#'
#' @examples
#' check_class(mtcars$mpg) # TRUE
#' check_class(mtcars$mpg, classes = c("numeric", "factor")) # TRUE
#' check_class(iris$Species) # TRUE
#' check_class(iris$Species, classes = c("logical", "numeric")) # FALSE
#' check_class(mtcars$mpg, classes = c("logical", "character", "factor")) # FALSE
check_class <- function(x,
                        classes = c(
                          "numeric", "integer", "logical",
                          "character", "factor", "ordered"
                        ),
                        strict = TRUE) {
  check_res <- all(class(x) %in% classes)
  if (!strict) check_res <- any(class(x) %in% classes)
  return(check_res)
}
