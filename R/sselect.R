#' Safely Select Specified Columns of a Labeled Data Frame
#'
#' @description
#' `sselect` allows one to subset (select) columns from a data.frame,
#' returning the selected columns as a data.frame that preserves the labelr
#' attributes attached to the inputted data.frame.
#'
#' @details
#' This function accepts a data.frame, followed by a set of comma-separated,
#' non-quoted column names to be retained and returns the selected columns in a
#' data.frame that preserves labelr attribute information. NOTE: This command
#' does not allow for negative selection, quoted columns, or dplyr-like helper
#' functions or special selection idioms, but: see `sdrop` for negative selection
#' ("return all columns except these"); see `sbrac` for a more flexible
#' subsetting command; and see also `ssubset`, `sfilter`, `ssort`, and `srename`,
#' as well as `slab` and `flab`.
#'
#' @param data the data.frame from which columns will be selected.
#' @param ... unquoted column/variable names to be selected (e.g., cyl, mpg,
#' not c("cyl", "mpg")).
#' @param ... comma-separated, unquoted column/variable names to be selected
#' (e.g., cyl, mpg, not c("cyl", "mpg")), with no other special characters or
#' symbols, such as quotes, parentheses, colons, minus signs, exclamation
#' points, or other operators.
#' @return a labelr label attribute-preserving data.frame consisting of the
#' selected subset of columns.
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
#' dfless <- sselect(df, id, raceth) # select only the vars id and raceth
#' head(dfless, 3) # selection worked
#' check_labs_att(dfless, "val.labs.raceth") # "raceth" value labels preserved
sselect <- function(data, ...) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  these_atts <- get_all_lab_atts(data)
  vars <- as.character(as.list(substitute(...())))
  if (length(vars) >= 1) data <- data[vars]
  data <- as.data.frame(data)
  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
