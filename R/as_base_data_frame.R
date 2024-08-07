#' Convert Augmented Data Frame to Base R Data Frame
#'
#' @description
#' `as_base_data_frame` noisily converts an augmented data.frame to a Base R
#' data.frame.
#
#' @details
#' Note: `adf` is a compact alias for `as_base_data_frame`: they do the same
#' thing, and the former is easier to type
#'
#' To minimize dependencies and complexities, labelr label-assigning
#' functions are designed to work exclusively with Base R data.frames, not
#' alternative data structures like matrices or augmented data.frames, such as
#' data.tables or tibbles. The suggested labeling workflow is to first assign
#' and work with labels using a Base R data.frame and then convert the resulting
#' object to an augmented data.frame as desired and without any assumption that
#' labelr labels or functions will smoothly interoperate with the augmented
#' data.frame construct or functions that depend on it.
#'
#' `as_base_data_frame` determines whether data argument is a conventional Base
#' R data.frame, some kind of augmented data.frame (e.g., data.table, tibble),
#' or not a data.frame at all (e.g., matrix). If the object has multiple
#' classes, one of which is a data.frame, the object is coerced to be a
#' conventional Base R data.frame, and a message to that effect is issued. If
#' the supplied object is not any kind of data.frame (i.e., a matrix is not any
#' kind of data.frame, while a data.table is a kind of data.frame), an error is
#' thrown. If the supplied object already is a Base R data.frame with no
#' additional classes (i.e., not an augmented data.frame), that supplied object
#' is returned with no changes made and no messages.
#'
#' @param data a data.frame object.
#' @param fact.to.char coerce all factor variables to character variables.
#' @param irreg.to.na convert all irregular values (see `irregular2v()`) to NA.
#' @return a data.frame object with any additional classes removed.
#' @export
#'
#' @examples
#' x1 <- runif(10)
#' x2 <- as.character(sample(c(1:20), 10, replace = TRUE))
#' x3 <- sample(letters, size = 10, replace = TRUE)
#' df <- data.frame(x1, x2, x3)
#' dft <- tibble::as_tibble(df)
#' class(dft)
#' df_vanilla <- as_base_data_frame(dft)
#' class(df_vanilla)
as_base_data_frame <- function(data, fact.to.char = FALSE, irreg.to.na = FALSE) {
  # strip "labeled.data.frame" class if present
  if (any(class(data) %in% "labeled.data.frame")) {
    class(data) <- class(data)[!class(data) %in% "labeled.data.frame"]
  }

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
#' @rdname as_base_data_frame
adf <- as_base_data_frame
