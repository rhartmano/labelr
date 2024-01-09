#' Safely Merge Two Data Frames
#'
#' @description
#' `smerge` allows one to merge two data.frames, one or both of which may be
#' labeled, preserving the labelr attributes of the inputted data.frames.
#'
#' @details
#' Precedence is given to the labels of the first data.frame (argument x), such
#' that, if both data.frames include a label attribute with the same name, the
#' attribute from data.frame x will be preserved, and the same-named attribute
#' from data.frame y will be discarded.
#'
#' @param x a data.frame to be merged with y.
#' @param y a data.frame to be merged with x.
#' @param ... additional arguments passed to `base::merge()`
#' @return a data.frame.
#' @export
#' @examples
#' # assign mtcars to df
#' df <- mtcars
#'
#' # assign the rownames to a column
#' id <- rownames(df)
#'
#' df <- cbind(id, df)
#'
#' # split the data.frame into two
#' df_a <- df[c("id", "am")]
#' df_b <- df[c("id", "mpg")]
#'
#' # add value labels to df_a$am
#' df_a <- add_val_labs(
#'   data = df_a,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' # add numerical range value labels to df_b$mpg
#' df_b <- add_quant1(df_b, mpg, qtiles = 4)
#'
#' # now, safely merge them
#' df_m <- smerge(df_a, df_b)
#'
#' head(df_m)
#'
#' get_val_labs(df_m)
smerge <- function(x, y, ...) {
  # make sure these are Base R data.frames
  x <- as_base_data_frame(x)
  y <- as_base_data_frame(y)

  # lab atts found in x
  main_lab_atts <- get_all_lab_atts(x)

  # lab atts found in y but not x
  more_lab_atts <- base::setdiff(get_all_lab_atts(y), get_all_lab_atts(x))

  if (length(more_lab_atts) > 0) {
    length_main <- length(main_lab_atts)

    for (i in seq_along(more_lab_atts)) {
      main_lab_atts[[length_main + i]] <- more_lab_atts[[i]]
      names(main_lab_atts)[[length_main + i]] <- names(more_lab_atts)[[i]]
    }
  }

  xy <- base::merge(x, y, ...)
  xy <- add_lab_atts(xy, main_lab_atts, num.convert = FALSE)
  return(xy)
}
