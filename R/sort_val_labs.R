#' Sort Ascending Any Variable Value Labels
#'
#' @description
#' `sort_val_labs` sorts the presentation order of variable value label meta-data.
#'
#' @details
#' This function is used in internally by other labelr functions to ensure that
#' value label meta-data is sorted in a logical, intuitive order. It is not
#' intended for interactive use.
#'
#' @param data a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' # note that this example is trivial, as value labels already are in order
#' df <- mtcars
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
#' df <- sort_val_labs(df)
sort_val_labs <- function(data) {
  data <- as_base_data_frame(data)

  vars_w_val_lab_info <- sapply(
    names(data),
    function(x) {
      check_labs_att(
        data,
        paste0("val.labs.", x)
      )
    }
  )

  vars_w_val_labs <- names(vars_w_val_lab_info)[unname(vars_w_val_lab_info)]

  if (length(vars_w_val_labs) != 0) {
    for (i in seq_along(vars_w_val_labs)) {
      var <- vars_w_val_labs[i]
      this_val_lab <- paste0("val.labs.", var)
      val_labs_w_NA <- attributes(data)[[this_val_lab]]
      val_labs_w_out_NA <- val_labs_w_NA[!names(val_labs_w_NA) == "NA"]
      val_lab_names_in_order <- names(val_labs_w_out_NA)[order(as_numv(names(val_labs_w_out_NA)))]
      val_labs_w_out_NA <- val_labs_w_out_NA[val_lab_names_in_order]
      na_element <- "NA"
      names(na_element) <- "NA"
      val_labs_w_NA_2 <- c(val_labs_w_out_NA, na_element)
      attributes(data)[[this_val_lab]] <- val_labs_w_NA_2
    }
  }
  return(data)
}
