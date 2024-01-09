#' (Alias for) `add_frame_lab()`
#'
#' @description
#' `afl()` is an alias for `add_frame_lab()`.
#'
#' @details
#' `afl` assigns an overall descriptive "frame label" for a data.frame,
#' which can be retrieved using `get_frame_lab` or its alias `gfl`.
#' @param data a data.frame.
#' @param frame.lab quoted text of the descriptive data.frame label that you wish
#' to add to the data.frame.
#' @return A data.frame, with a frame.lab attribute added to the attributes
#' meta-data
#' @export
#' @examples
#' # add frame.lab to mtcars and assign to new data.frame mt2
#' mt2 <- afl(mtcars, frame.lab = "Data extracted from the 1974 Motor
#' Trend US magazine, comprising fuel consumption and 10 aspects of automobile
#' design and performance for 32 automobiles (1973–74 models). Source: Henderson
#' and Velleman (1981), Building multiple regression models interactively.
#'  Biometrics, 37, 391–411.")
#' attr(mt2, "frame.lab") # check for attribute
#' gfl(mt2) # return frame.lab alongside data.frame name as a data.frame
afl <- function(data, frame.lab = NULL) {
  # get data.frame name
  if (is.null(frame.lab)) frame.lab <- deparse(substitute(data))

  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # eliminate carriage returns and excess spaces
  frame.lab <- gsub("[\r\n]", "", frame.lab)
  frame.lab <- gsub("  ", " ", frame.lab)

  # attach attribute
  attr(data, "frame.lab") <- frame.lab

  # frame.lab must be 500 or fewer characters
  test_val <- nchar(attr(data, "frame.lab")) > 500

  if (test_val) {
    stop("
\nframe.lab may not exceed 500 characters.")
  }

  # update and resort attributes
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)

  return(data)
}
