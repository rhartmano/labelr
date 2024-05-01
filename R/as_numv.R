#' Convert a Suitable Character Vector to Numeric
#'
#' @description
#' `as_numv` determines whether a character vector can be coerced to numeric
#' without generating new NA values and, if so, it makes that conversion
#' (similar to Stata's destring command).
#'
#' @details
#' Core labelr functions coerce integers to characters and back, which
#' `as_numv` facilitates. Note that character values of "NA" (including "na",
#' "Na", and "nA") will be converted to NA and, by default, so will other
#' "irregular" values (in the sense of `check_irregular`).
#'
#' @param x a character vector.
#' @param nan2na a logical argument. TRUE if the non-case-sensitive string "nan"
#' should be converted to NA.
#' @param inf2na a logical argument. TRUE if the non-case-sensitive strings
#' "inf" or "-inf should be converted to NA.
#'
#' @return a vector, converted to numeric if feasible (else, the same character
#' vector that was supplied).
#' @export
#'
#' @examples
#' set.seed(123)
#' x1 <- runif(10)
#' x2 <- as.character(sample(c(1:20), 10, replace = TRUE))
#' x2_num <- as_numv(x2)
#' class(x2)
#' class(x2_num)
#' head(x2)
as_numv <- function(x, nan2na = TRUE, inf2na = TRUE) {
  if (!is.numeric(x) && !is.factor(x)) {
    x <- irregular2v(x, to = NA, nan.include = nan2na, inf.include = inf2na)
    na_start <- sum(is.na(x))
    na_end <- sum(is.na(suppressWarnings(as.numeric(as.character(x)))))
    numable <- na_end == na_start
    if (numable) x <- as.numeric(x)
  }
  return(x)
}
