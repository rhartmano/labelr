#' Replace "Irregular" Values of a Vector with Some Other Value
#'
#' @description
#' Check a vector for the presence of "irregular" values (e.g., NA) and, if
#' found, replace with some other (single) user-specified value.
#'
#' @details
#' For purposes of `irregular2v`, irregular values consist of: NA values, other
#' arbitrary values you specify, and (by default): NaN, Inf, -Inf, and character
#' variants of same (i.e., upper, lower, or mixed-case variants of "NA","NAN",
#' "INF","-INF"). This function is used insider core labelr functions to manage
#' such values, but the typical labelr user will have no explicit need to use it
#' as part of an interactive session.
#'
#' @param x a vector.
#' @param to a single value to which all NA (and other irregular) values should
#' be converted (Note: if arg is character, returned vector will be
#' coerced to character).
#' @param nan.include treat NaN values as NA.
#' @param inf.include treat Inf and -Inf values as NA.
#' @param special a modifiable set of default character values that will be
#' treated as equivalent to NA values.
#' @param other additional user-specified values (of consistent class) that will
#' be treated as equivalent to NA values.
#'
#' @return a vector identical to x, with exception that NA (and other irregular)
#' values have been converted to the value specified in "to" argument.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(NA, Inf, -Inf, NaN, runif(6))
#' x
#' x1 <- irregular2v(x, to = 33)
#' x1
#'
#' set.seed(123)
#' x1 <- c(NA, Inf, -Inf, NaN, runif(6))
#' x
#' x1 <- irregular2v(x, to = 33, nan.include = TRUE, inf.include = FALSE)
#' x1
#'
#' set.seed(123)
#' x <- c(NA, "INF", "in", "nan", "NA", sample(letters, 5))
#' x
#' x1 <- irregular2v(x, to = "<-X->")
#' x1
irregular2v <- function(x, to = 99, nan.include = TRUE,
                        inf.include = TRUE,
                        special = c("NA", "NAN", "INF", "-INF"),
                        other = NULL) {
  if (length(to) != 1) {
    stop("
    \nYour \"to\" argument must be a single (1L) value.")
  }

  x[check_irregular(x,
    nan.include = nan.include,
    inf.include = inf.include,
    special = special,
    other = other,
    any = FALSE
  )] <- to

  return(x)
}
