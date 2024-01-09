#' Remove Frame Label Attribute from a Data Frame
#'
#' @description
#' Remove the frame label attribute (see `add_frame_lab`) from a data.frame, if
#' one is present.
#'
#' @details
#' See `add_frame_lab` for more on this labeling construct.
#'
#' Note: `dfl` is a compact alias for `drop_frame_lab`: they do the same thing,
#' and the former is easier to type.
#'
#' @param data the data.frame with a frame label that you wish to drop (and which
#' was added using `add_frame_lab`).
#' @return a data.frame (with any previously applied frame.lab attribute removed).
#' @export
#' @examples
#' # add frame.lab to mtcars and assign to new data.frame mt2
#' mt2 <- add_frame_lab(mtcars, frame.lab = "Data extracted from the 1974 Motor
#'                     Trend US magazine, comprising fuel consumption and 10
#'                     aspects of automobile design and performance for 32
#'                     automobiles (1973–74 models). Source: Henderson and
#'                     Velleman (1981), Building multiple regression models
#'                     interactively. Biometrics, 37, 391–411.")
#'
#' get_frame_lab(mt2) # return frame.lab alongside data.frame name as a data.frame
#' drop_frame_lab(mt2) # remove this frame.lab
#' get_frame_lab(mt2) # the data.frame name now doubles as its frame label
#' is.null(attributes(data)[["frame.lab"]]) # the attribute is NULL
drop_frame_lab <- function(data) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  if (!is.null(attributes(data)[["frame.lab"]])) {
    attributes(data)[["frame.lab"]] <- NULL
  }

  return(data)
}
