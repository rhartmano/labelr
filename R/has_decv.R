#' Determine if Vector Has Decimals
#'
#' @description
#' `has_decv` determines whether a vector has decimal values, using all values
#' for smaller vectors and using a non-random sample of observations for larger
#' vectors.
#'
#' @details
#' This function is used by core labelr functions to detect vectors that are
#' bad candidates for one-to-one value labeling (as implemented by, e.g.,
#' `add_val_labs`).
#'
#' @param x the vector to check for presence of decimals.
#' @param sample.after for larger (length(x)>1000) vectors,
#' take a non-random sample of 1000 observations from the vector.
#'
#' @return a 1L vector indicating whether x has decimal values.
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
#' df <- as_num(df)
#' #' head(df,3)
#' sapply(df, class)
#' sapply(mtcars, is.double)
#' sapply(mtcars, is.numeric)
#' sapply(mtcars, is.integer)
#' sapply(mtcars, has_decv)
has_decv <- function(x, sample.after = 1000) {
  if (is.numeric(x)) {
    if (length(x) > sample.after) {
      inds <- unique(floor(seq(1, length(x), length.out = 1000)))
    } else {
      inds <- seq_len(length(x))
    }
    x <- x[inds]
    x <- irregular2v(x, NA)
    x <- x[!is.na(x)]
    test_dec <- any(x %% 1 > 0)
  } else {
    test_dec <- FALSE
  }

  return(test_dec)
}
