#' Are All Values in a Free-standing Vector Unique?
#'
#' @description
#' For a given vector, does the length of (number of values in) the vector equal
#' the number of unique values in the vector?
#'
#' Note 2: `all_univ` is a compact alias for `all_vals_uniquev`: they do the same
#' thing, and the former is easier to type
#'
#' @param x a vector.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA
#' values should be stripped before the computation proceeds.
#' @return a 1L logical.
#' @export
#' @examples
#' all_vals_uniquev(mtcars$am) # FALSE
#'
#' set.seed(35994)
#' z <- runif(25)
#' all_univ(z) # TRUE; all_uni is an alias for all_vals_uniquev()
#'
#' z[c(1, 2)] <- NA # two NA values added
#' all_univ(z, na.rm = FALSE) # FALSE, because the two NA values are not unique
all_vals_uniquev <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  test_val <- length(unique(x)) == length(x)
  return(test_val)
}

#' @export
#' @rdname all_vals_uniquev
all_univ <- all_vals_uniquev
