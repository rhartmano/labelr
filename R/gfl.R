#' (Alias for) `get_frame_lab()`
#'
#' @description
#' `gfl()` is an alias for `get_frame_lab()`.
#'
#' @details
#' `gfl` returns the overall descriptive "frame label" that is assigned
#' to a data.frame using `add_frame_lab` or its alias `afl`.
#' @param data a data.frame.
#' @return A 1x2 data.frame, consisting of "data.frame" and "frame.lab" columns
#' for supplied data.frame.
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
#' gfl(mt2)
gfl <- function(data) {
  dfname <- deparse(substitute(data))
  if (is.null(attributes(data)$frame.lab)) {
    data <- add_frame_lab(data, frame.lab = dfname)
  }

  frame_lab <- unname(attr(data, "frame.lab"))
  frame.data <- data.frame(data.frame = dfname, frame.lab = frame_lab)
  return(frame.data)
}
