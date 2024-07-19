#' Assign Class labeled.data.frame to a Data Frame Object
#'
#' @description
#' `as_labeled_data_frame` quietly assigns the class labeled.data.frame to a
#' data.frame and discards other augmented data.frame classes, returning a
#' data.frame with classes labeled.data.frame and data.frame.
#
#' @details
#' Note 1: `aldf` is a compact alias for `as_labeled_data_frame`: they do the same
#' thing, and the former is easier to type
#'
#' Note 2: `as_labeled_data_frame` is used internally by other labelr commands
#' and is not intended for interactive use.
#'
#' To minimize dependencies and complexities, labelr label-assigning
#' functions are designed to work exclusively with Base R data.frames, not
#' alternative data structures like matrices or augmented data.frames, such as
#' data.tables or tibbles.
#'
#' `as_labeled_data_frame` determines whether data argument is a conventional
#' Base R data.frame, some kind of augmented data.frame (e.g., data.table,
#' tibble), or not a data.frame at all (e.g., matrix). If the submitted object
#' is a type of data.frame, the object will be returned with the class
#' labeled.data.frame applied, the class data.frame retained, and any other
#' previous class attributes discarded. If the supplied object is not any kind
#' of data.frame (i.e., a matrix is not any kind of data.frame, while a data.table
#' is a kind of data.frame), an error is thrown.
#'
#' @param data a data.frame object.
#' @param fact.to.char coerce all factor variables to character variables.
#' @param irreg.to.na convert all irregular values (see `irregular2v()`) to NA.
#' @return an object of classes labeled.data.frame and data.frame, with any
#' additional classes removed.
#' @export
#'
#' @examples
#' x1 <- runif(10)
#' x2 <- as.character(sample(c(1:20), 10, replace = TRUE))
#' x3 <- sample(letters, size = 10, replace = TRUE)
#' df <- data.frame(x1, x2, x3)
#' dft <- tibble::as_tibble(df)
#' class(dft)
#' dfl <- as_labeled_data_frame(dft)
#' class(dfl)
as_labeled_data_frame <- function(data, fact.to.char = FALSE, irreg.to.na = FALSE) {
  if (!"data.frame" %in% class(data)) {
    stop("
data argument object must be, but is not, a data.frame.")
  } else if (length(class(data)) != 1) {
    data <- as.data.frame(data)
  }

  # optionally convert factor to character
  if (fact.to.char) {
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)
  }

  # optionally replace all iregular values with NA
  if (irreg.to.na) {
    data[names(data)] <- lapply(data[names(data)], \(x) irregular2v(x, NA))
  }

  # use any name.labs as label attributes
  for (i in names(data)) {
    name_lab <- suppressWarnings(get_name_labs(data, i)$lab)
    if (!is.na(name_lab[1]) && name_lab[1] != "NA") attr(data[[i]], "label") <- name_lab[1]
  }

  # add labeled.data.frame class
  data <- as.data.frame(data)
  class(data) <- unique(c("labeled.data.frame", class(data)))

  # re-arrange attributes to be in a clean, logical order
  all_att_names <- names(attributes(data))

  core_att_names <- c("names", "row.names", "class")
  core_in <- core_att_names[core_att_names %in% all_att_names]

  frame_name <- c("frame.lab", "name.labs")
  frame_name_in <- frame_name[frame_name %in% all_att_names]

  val_lab_names <- paste0("val.labs.", names(data))
  val_lab_in <- val_lab_names[val_lab_names %in% all_att_names]

  fact_names <- c(
    paste0("u.factor.", names(data)),
    paste0("o.factor.", names(data))
  )

  fact_in <- fact_names[fact_names %in% all_att_names]

  names_in_combined <- c(core_in, frame_name_in, val_lab_in, fact_in)

  other_names_in <- all_att_names[!all_att_names %in% names_in_combined]

  final_names <- c(
    core_in, frame_name_in,
    fact_in, val_lab_in,
    other_names_in
  )

  final_atts <- attributes(data)[final_names]
  attributes(data) <- NULL
  attributes(data) <- final_atts
  return(data)
}

#' @export
#' @rdname as_labeled_data_frame
aldf <- as_labeled_data_frame
