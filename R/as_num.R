#' Convert all Suitable Character Variables to Numeric
#'
#' @description
#' `as_num` identifies the character variables of a data.frame that can be
#' coerced to numeric without generating new NA values and, for those variables
#' where this can be done, it makes those conversions (similar to Stata's
#' destring command).
#'
#' @details
#' Core labelr functions coerce integers to characters and back, which `as_num`
#' facilitates. Note that character values of "NA" (including "na", "Na",
#' and "nA") will be converted to NA and, by default, so will other "irregular"
#' values (in the sense of `check_irregular`).
#'
#' @param data a data.frame object.
#' @param nan2na a logical argument. TRUE if the non-case-sensitive string "nan"
#' should be converted to NA.
#' @param inf2na a logical argument. TRUE if the non-case-sensitive strings
#' "inf" or "-inf should be converted to NA.
#'
#' @return a data.frame object with all applicable character variables coerced
#' to numeric.
#' @export
#'
#' @examples
#' set.seed(123)
#' x1 <- runif(10)
#' x2 <- as.character(sample(c(1:20), 10, replace = TRUE))
#' x3 <- sample(letters, size = 10, replace = TRUE)

#' df <- data.frame(x1, x2, x3)
#' head(df, 3)
#' sapply(df, class)
#' class(df$x2)
#'
#' df <- as_num(df)
#' head(df,3)
#' sapply(df, class)
#' class(df$x2)
as_num <- function(data, nan2na = TRUE, inf2na = TRUE) {
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

  for (i in seq_along(names(data))) {
    data[[i]] <- as_numv(data[[i]])
  }

  return(data)
}
