#' Convert All Factor Variables of a Data Frame to Column Variables
#'
#' @description
#' Convenience function to convert all factor variables to character.
#'
#' @param data a data.frame object.
#' @return a data.frame identical to data, with exception that any factors
#' have been converted to character variables.
#' @export
#'
#' @examples
#' sapply(iris, class)
#' head(iris)
#'
#' iris_ch <- fact2char(iris)
#'
#' sapply(iris_ch, class)
#' head(iris_ch)
fact2char <- function(data) {
  data <- as.data.frame(data)
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  return(data)
}

#' @export
#' @rdname fact2char
f2c <- fact2char
