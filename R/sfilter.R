#' Safely Filter Rows of a Labeled Data Frame
#'
#' @description
#' `sfilter` allows one to filter-subset a data.frame, selecting only rows that
#' satisfy conditions (after the fashion of base::subset() or dplyr::filter()),
#' returning the selected rows as a data.frame that preserves the labelr
#' attributes attached to the inputted data.frame.
#'
#' @details
#' This function accepts a data.frame, followed by conditional filtering
#' instructions and returns the selected rows (and all inputted data.frame
#' columns) as a data.frame that preserves the labelr attribute information of
#' the originally supplied data.frame. See `ssubset` for a variant that combines
#' `sfilter` row-filtering and `sselect` column selection in a single function.
#' See `sbrac` for a labelr attribute-preserving approach to row and/or column
#' indexing. See also `sdrop`, `ssort`, `srename`, `slab`, and `flab`.
#'
#' @param data the data.frame from which columns will be selected.
#' @param condition row-filtering conditions along the lines of base::subset()
#' and/or dplyr::select().
#' @return a labelr label attribute-preserving data.frame consisting of the
#' selected rows that meet the filtering condition(s).
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000) # another labelr:: function
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' head(df, 3)
#' check_labs_att(df, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dffil1 <- sfilter(df, raceth %in% c(1, 2, 3) & id < 50 & gender == 1)
#' head(dffil1, 3)
#' check_labs_att(dffil1, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dffil2 <- sfilter(df, !raceth %in% 1:5 | (age == 60))
#' head(dffil2, 8)
#' check_labs_att(dffil2, "val.labs.raceth") # "raceth" lab specifically TRUE
sfilter <- function(data, condition) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  row_names <- rownames(data)
  these_atts <- get_all_lab_atts(data)
  cond_call <- substitute(condition)
  col_names <- names(data)
  cond_ind <- eval(cond_call, data, parent.frame())
  cond_ind[is.na(cond_ind)] <- FALSE
  data <- data[cond_ind, , drop = FALSE]
  row_names <- row_names[cond_ind]
  rownames(data) <- row_names
  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
