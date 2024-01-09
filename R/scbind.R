#' Safely Combine Data Frames Column-wise
#'
#' @description
#' `scbind` allows one to bind columns together into a data.frame, while
#' preserving any labelr labels of the inputted data.frames.
#'
#' @details
#' Precedence is given to the labels of earlier-appearing arguments, such that,
#' if both an earlier and a later data.frame include a label attribute with the
#' same name, the attribute from the earlier data.frame will be preserved, and
#' the same-named attribute later data.frame(s) will be discarded.
#'
#' @param ... data.frames to be column-bound
#' @return a data.frame.
#' @export
#' @examples
#' # assign mtcars to df
#' df <- mtcars
#'
#' # add value labels to "am"
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' # add numerical range value labels to "mpg"
#' df <- add_quant1(df, mpg, qtiles = 4)
#'
#' df_a <- sselect(df, "am")
#' df_b <- sselect(df, "mpg")
#' df_c <- sselect(mtcars, "cyl")
#' df_all <- scbind(df_a, df_b, df_c)
#'
#' head(df_all)
#'
#' get_val_labs(df_all)
scbind <- function(...) {
  df_list <- list(...)
  lab_atts_count <- sum(lengths(lapply(df_list, get_all_lab_atts)))
  new_list <- vector(mode = "list", length = lab_atts_count)
  count <- 0
  for (i in seq_along(df_list)) {
    this_atts_list <- get_all_lab_atts(df_list[[i]])
    if (length(this_atts_list) > 0) {
      for (j in seq_along(this_atts_list)) {
        count <- count + 1
        if (!names(this_atts_list)[[j]] %in% names(new_list)) {
          new_list[[count]] <- this_atts_list[[j]]
          names(new_list)[[count]] <- names(this_atts_list)[[j]]
        } else {
          count <- count - 1
          new_list[[length(new_list)]] <- NULL
        }
      }
    }
  }

  df <- lapply(list(...), as.data.frame)
  df <- Reduce(cbind, df)
  df <- as.data.frame(df)
  df <- add_lab_atts(df, new_list, num.convert = FALSE)
  return(df)
}
