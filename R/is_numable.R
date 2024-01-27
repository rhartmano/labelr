#' Test Whether Character Vector Is "Suitable" for Numeric Conversion
#'
#' @description
#' `is_numable` determines whether a character vector can be coerced to numeric
#' without generating new NA values.
#'
#' @details
#' Core labelr functions coerce integers to characters and back, which
#' `is_numable` facilitates.
#'
#' @param x a character vector.
#' @param nan2na treat NaN (including, e.g., "nan") values as NA values.
#' @param inf2na treat Inf, -Inf values (including, e.g., "inf") as NA values.
#' treated as equivalent to NA values.
#' @return a 1L (scalar) logical vector.
#' @export
#'
#' @examples
#' set.seed(123)
#' x1 <- runif(10)
#' x2 <- as.character(sample(c(1:20), 10, replace = TRUE))
#' x2_num_test <- is_numable(x2)
#' x2_num_test
#' x3 <- sample(LETTERS, 10, replace = TRUE)
#' x3_num_test <- is_numable(x3)
#' x3_num_test
is_numable <- function(x, nan2na = TRUE, inf2na = TRUE) {
  if (!is.numeric(x) && !is.factor(x)) {
    x <- irregular2v(x, to = NA, nan.include = nan2na, inf.include = inf2na)
    na_start <- sum(is.na(x))
    na_end <- sum(is.na(suppressWarnings(as.numeric(as.character(x)))))
    numable <- na_end == na_start
  } else {
    numable <- FALSE
  }
  return(numable)
}
