#' Safely Drop Specified Columns of a Labeled Data Frame
#'
#' @description
#' `sdrop` allows one to remove columns from a data.frame, returning the
#' remaining columns as a data.frame that preserves the labelr attributes
#' attached to the inputted data.frame.
#'
#' @details
#' This function accepts a data.frame, followed by a set of comma-separated,
#' non-quoted column names to be discarded and returns the remaining columns as
#' a data.frame that preserves labelr attribute information. NOTE: This command
#' does NOT allow for positive specification of columns to be retained; rather,
#' all variables not specified will be retained by default. Further, `sdrop`
#' does not supported quoted column names, dplyr-like helper functions or
#' other special selection syntax or idioms. See also `ssubset`, `sselect`, or
#' `sbrac`); see also `sfilter`, `ssort`, `srename`, `slab`, and `flab`.
#'
#' @param data the data.frame from which columns will be removed.
#' @param ... comma-separated, unquoted column/variable names to be discarded
#' (e.g., cyl, mpg, not c("cyl", "mpg")), with no other special characters or
#' symbols, such as quotes, parentheses, colons, minus signs, exclamation
#' points, or other operators.
#' @return a labelr label attribute-preserving data.frame consisting of the
#' remaining (i.e., non-specified, non-discarded) subset of columns of the
#' supplied data.frame.
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
#' dfless <- sdrop(df, id, raceth) # select only the vars id and raceth
#' head(dfless, 3) # selection worked
#' check_labs_att(dfless, "val.labs.raceth") # "raceth" value labels preserved
sdrop <- function(data, ...) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  these_atts <- get_all_lab_atts(data)
  vars <- as.character(as.list(substitute(...())))
  var_inds2drop <- which(names(data) %in% vars)
  if (length(vars) >= 1) data <- data[-c(var_inds2drop)]
  data <- as.data.frame(data)
  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
