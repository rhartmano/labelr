#' Safely Subset a Labeled Data Frame
#'
#' @description
#' `ssubset` allows one to simultaneously filter-select rows that
#' satisfy conditions AND return only selected columns as a data.frame that
#' preserves the labelr attributes
#' attached to the inputted data.frame.
#'
#' @details
#' Combining `sfilter` and `sselect` functionality (along the lines of
#' base::subset(), this function accepts a data.frame, followed by conditional
#' filtering instructions (required) and (optional) comma-separated unquoted
#' column names to be selected (see examples), returning  the selected rows and
#' columns as a data.frame that preserves the labelr  attribute information of
#' the originally supplied data.frame. See `ssubset` for a variant that combines
#' `sfilter` row-filtering and `sselect` column selection in a single function.
#' See also `sbrac` for a labelr attribute-preserving approach to row-and/or-
#' column indexing. See also `sdrop`, `ssort`, `srename`, `slab` and `flab`.
#'
#' @param data the data.frame from which columns rows will be filtered (and,
#' possibly, columns selected).
#' @param condition row-filtering conditions along the lines of base::subset()
#' and/or dplyr::select(). Note: Row-filtering conditions (to include
#' condition==NULL) must be supplied.
#' @param ... Optionally supply one or more unquoted, comma-separated
#' column names that identify columns to be retained (Note: If no columns are
#' listed, all columns will be retained). Note: While row-filtering conditions
#' may leverage standard operators (e.g., &, |, ==, !=), the column-selection
#' portion of call may not incorporate special characters or symbols, such as
#' quotes, parentheses, colons, minus signs, exclamation points, or other
#' operators.
#' @return a labelr label attribute-preserving data.frame consisting of the
#' selected rows that meet the filtering condition(s).
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' df <- make_demo_data(n = 1000, seed = 555) # another labelr:: function
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
#' dfsub1 <- ssubset(df, raceth == 2 & age > 70, id, raceth, gender)
#' head(dfsub1, 3)
#' check_labs_att(dfsub1, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsub2 <- ssubset(df, raceth %in% c(2:4), age, raceth)
#' head(dfsub2, 3)
#' check_labs_att(dfsub2, "val.labs.raceth") # "raceth" lab specifically TRUE
#' # even if NULL, must supply explicit condition argument
#' head(ssubset(df, condition = NULL, age, raceth), 3) # better to just use sselect()
#' head(ssubset(df, age == 60), 3) # skip column selection (will return all cols)
ssubset <- function(data, condition, ...) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  row_names <- rownames(data)
  these_atts <- get_all_lab_atts(data)
  cond_call <- substitute(condition)
  vars <- as.character(as.list(substitute(...())))
  col_names <- names(data)

  if (!is.null(cond_call)) {
    filter_cols_needed <- unname(which(greml(col_names, cond_call)))
    filter_colnames_needed <- names(data)[filter_cols_needed]
    data2 <- data[filter_cols_needed]
    cond_ind <- eval(cond_call, data2, parent.frame())
    rm(data2)
    cond_ind[is.na(cond_ind)] <- FALSE
    row_names <- row_names[cond_ind]
    data <- data[cond_ind, ]
    rownames(data) <- row_names
  }

  if (length(vars) >= 1) data <- data[vars]
  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
