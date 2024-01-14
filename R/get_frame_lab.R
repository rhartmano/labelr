#' Return a Data Frame's Frame Label
#'
#' @description
#' For a frame-labeled data.frame, `get_frame_lab` returns a derivative
#' 1x2 data.frame that lists the data.frame name and its frame.lab attribute.
#'
#' Note: `gfl` is a compact alias for `get_frame_lab`: they do the same thing,
#' and the former is easier to type.
#'
#' @details
#' `get_frame_lab` returns the overall descriptive "frame label" that is assigned
#' to a data.frame using `add_frame_lab.`
#' @param data a data.frame.
#' @return A 1x2 data.frame, consisting of "data.frame" and "frame.lab" values
#' for the supplied data.frame. If the supplied data.frame does not have a frame
#' label, its name also will be used as its frame label (i.e., both entries of
#' the returned 1x2 data.frame will be the data.frame name).
#' @export
#' @examples
#' # add frame.lab to mtcars and assign to new data.frame mt2
#' mt2 <- add_frame_lab(mtcars, frame.lab = "Data extracted from the 1974 Motor
#'                       Trend US magazine, comprising fuel consumption and 10
#'                       aspects of automobile design and performance for 32
#'                       automobiles (1973–74 models). Source: Henderson and
#'                       Velleman (1981), Building multiple regression models
#'                       interactively. Biometrics, 37, 391–411.")
#'
#' attr(mt2, "frame.lab") # check for attribute
#'
#' # return frame.lab alongside data.frame name as a data.frame
#' get_frame_lab(mt2)
get_frame_lab <- function(data) {
  dfname <- deparse(substitute(data))
  if (is.null(attributes(data)$frame.lab)) {
    data <- add_frame_lab(data, frame.lab = dfname)
  }

  frame_lab <- unname(attr(data, "frame.lab"))
  frame.data <- data.frame(data.frame = dfname, frame.lab = frame_lab)
  return(frame.data)
}

#' @export
#' @rdname get_frame_lab
gfl <- get_frame_lab
