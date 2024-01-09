#' Safely Combine Data Frames Row-wise
#'
#' @description
#' `srbind` allows one to bind rows together into a data.frame, while preserving
#' any labelr labels of the inputted data.frames.
#'
#' @details
#' Precedence is given to the labels of earlier-appearing arguments, such that,
#' if both an earlier and a later data.frame include a label attribute with the
#' same name, the attribute from the earlier data.frame will be preserved, and
#' the same-named attribute later data.frame(s) will be discarded.
#'
#' @param ... data.frames to be row-bound
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
#' # now, add value labels
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' df <- add_val_labs(
#'   data = df,
#'   vars = "carb",
#'   vals = c(1, 2, 3, 4, 6, 8),
#'   labs = c(
#'     "1-carb", "2-carbs",
#'     "3-carbs", "4-carbs",
#'     "6-carbs", "8-carbs"
#'   )
#' )
#'
#' # split the data.frame into three
#' aa <- df[1:5, ]
#' bb <- df[6:11, ]
#' cc <- df[12:32, ]
#' # put them back together
#' df2 <- srbind(aa, bb, cc, cc)
#'
#' get_val_labs(df2)
srbind <- function(...) {
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
  df <- Reduce(rbind, df)
  df <- as.data.frame(df)
  df <- add_lab_atts(df, new_list, num.convert = FALSE)
  return(df)
}
