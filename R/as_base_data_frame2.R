#' Convert Augmented Data Frame to Base R Data Frame with Alternate Defaults
#'
#' @description
#' `as_base_data_frame2` noisily converts an augmented data.frame to a Base R
#' data.frame, with any factors converted to character vectors, and any irregular
#' values (see `irregular2()`) converted to NA.
#
#' @details
#' Note: `adf2` is a compact alias for `as_base_data_frame`: they do the same
#' thing, and the former is easier to type
#'
#' `as_base_data_frame2` is a variant of `as_base_data_frame` with different
#' default values for fact.to.char and irreg.to.na. Whereas both of these
#' default to FALSE in `as_base_data_frame`, they both default to TRUE in
#' `as_base_data_frame2`. This is the only difference between the two functions.
#' As such, `as_base_data_frame2` is intended as a simple shortcut to save
#' typing if one prefers to reverse these default logical argument values.
#'
#' @param data a data.frame object.
#' @param fact.to.char coerce all factor variables to character variables.
#' @param irreg.to.na convert all irregular values (see `irregular2v()`) to NA.
#' @return a data.frame object with any additional classes removed.
#' @export
#'
#' @examples
#' iris_tib <- tibble::as_tibble(iris)
#' class(iris_tib)
#' iris_tib$Sepal.Length[1] <- Inf
#' head(iris_tib, 1)
#' iris_df <- as_base_data_frame2(iris_tib)
#' class(iris_df)
#' sapply(iris_df, class)
#' head(iris_df, 1)
as_base_data_frame2 <- function(data, fact.to.char = TRUE, irreg.to.na = TRUE) {
  if (!"data.frame" %in% class(data)) {
    stop("
data argument object must be, but is not, a data.frame.")
  } else if (length(class(data)) != 1) {
    data <- as.data.frame(data)
    warning("
data argument object coerced from augmented to conventional (Base R) data.frame.")
  }

  # optionally convert factor to character
  if (fact.to.char) {
    data <- as.data.frame(data)
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)
  }

  # optionally replace all iregular values with NA
  if (irreg.to.na) {
    data[names(data)] <- lapply(data[names(data)], \(x) irregular2v(x, NA))
  }

  return(data)
}

#' @export
#' @rdname as_base_data_frame2
adf2 <- as_base_data_frame2
