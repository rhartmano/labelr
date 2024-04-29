#' Subset a Data Frame Using Value Labels
#'
#' @description
#' `slab` ("subset using labels") allows one to filter rows and select columns
#' from a data.frame based on variable-specific value label attributes.
#'
#' @details
#' `slab` does base::subset-style data subsetting using variable value label
#' meta-data that are associated with variable values but are not themselves
#' values (i.e., will not appear in response to `View()`, `head()`, etc.).
#' In other words, value labels are supplied to the `slab()` call to direct the
#' filtering process, but those value labels are not displayed in the cells of
#' the returned data.frame -- the raw values themselves are. This functionality
#' may be useful for interactively subsetting a data.frame, where character
#' value labels may be more intuitive and easily recalled than the underlying
#' variable values themselves (e.g., raceth=="White" & gender="F" may be more intuitive or
#' readily recalled than raceth==3 & gender==2).
#'
#' `slab`takes as its arguments a labelr value-labeled data.frame, followed by
#' condition-based row-filtering instructions (required) and a list of unquoted
#' names of variables to be retained (optional; all variables returned by
#' default).
#'
#' Note 1: When using `slab`, any conditional row-filtering syntax involving
#' value-labeled variables must be expressed in terms of those variables' value
#' labels, not the raw values themselves. Filtering on non-value-labeled
#' variables is also permitted, with those variables' filtering conditions being
#' expressed in terms of raw values. Further, `slab()` calls may reference both
#' types of columns (i.e., value-labeled variables and non-value-labeled
#' variables), provided filtering conditions for the former are expressed in
#' terms of value labels.
#'
#' Note 2: `slab` (and labelr more broadly) is intended for moderate-sized (or
#' smaller) data.frames, defined loosely as those with a few million or fewer
#' rows. With a conventional (c. 2024) laptop, labelr operations on modest-
#' sized (~100K rows) take seconds (or less); with larger (> a few million rows)
#' data.frames, labelr may take several minutes (or run out of memory and fail
#' altogether!), depending on the complexity of the call and the number and type
#' of cells implicated in it.
#'
#' See also `flab`, `use_val_labs`, `add_val_labs`, `add_val1`,`add_quant_labs`,
#' `add_quant1`, \cr `get_val_labs`, `drop_val_labs`. For label-preserving
#' subsetting tools that subset in terms of raw values (not value labels), see
#' `sfilter`, `sbrac`, `ssubset`, `sdrop`.
#'
#' @param data the data.frame from which columns rows will be filtered (and,
#' possibly, columns selected)
#' @param condition row-filtering conditions along the lines of base::subset()
#' and/or dplyr::filter(). Note: Row-filtering conditions (to include
#' condition==NULL) must be supplied. Conditions involving value-labeled
#' variables must be expressed in terms of the value labels (see examples),
#' else try `ssubset.`
#' @param ... Optionally supply one or more unquoted, comma-separated
#' column names that identify columns to be retained (Note: If no columns are
#' listed, all columns will be retained). Note: While row-filtering conditions
#' may leverage standard operators (e.g., &, |, ==, !=), the column-selection
#' portion of call may not incorporate special characters or symbols, such as
#' quotes, parentheses, colons, minus signs, exclamation points, or other
#' operators. Only positive selection of columns is permitted (negative
#' selection -- i.e., "select not / select-all-except" specified columns is not
#' supported)
#'
#' @return a labelr label attribute-preserving data.frame consisting of the
#' selected rows that meet the filtering condition(s) and the columns whose
#' (unquoted, comma-separated) names are passed to dots (...) (or all columns
#' if no column names are passed).
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000)
#'
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' # let's add variable VALUE labels for variable "gender"
#' # note that, if we are labeling a single variable, we can use add_val1()
#' # distinction between add_val1() and add_val_labs() will become more
#' # meaningful when we get to our Likert example
#' df <- add_val1(
#'   data = df, gender, vals = c(0, 1, 2, 3, 4),
#'   labs = c("M", "F", "TR", "NB", "Diff-Term"), max.unique.vals = 50
#' )
#'
#' # see what we did
#' # get_val_labs(df)
#' get_val_labs(df, "gender")
#' get_val_labs(df, "raceth")
#'
#' # use --labels-- to subset w/ slab() ("*S*ubset using *lab*els")
#' dflab <- slab(df, raceth == "Asian" & gender == "F", id, gender)
#' head(dflab, 4)
#'
#' # equivalently, use --values--- to filter w/ sfilter() ("*S*afe filter")
#' dfsf <- ssubset(df, raceth == 3 & gender == 1, gender, raceth)
#' head(dfsf, 4)
slab <- function(data, condition, ...) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  backup_data <- data[FALSE, ]

  row_names <- rownames(data)

  initial_lab_atts <- get_all_lab_atts(data)
  data2 <- use_val_labs(data)
  cond_call <- substitute(condition)
  vars <- as.character(as.list(substitute(...())))
  col_names <- names(data2)

  # do filtering
  # get logical vector for filtering
  if (!is.null(cond_call)) {
    filter_cols_needed <- unname(which(greml(col_names, cond_call)))
    filter_colnames_needed <- names(data)[filter_cols_needed]
    data2 <- data2[filter_cols_needed]
    cond_ind <- eval(cond_call, data2, parent.frame())
    cond_ind[is.na(cond_ind)] <- FALSE
  }

  rm(data2)

  # if no rows match filtering conditions
  if (!any(cond_ind)) {
    data <- backup_data

    # if one or more rows match filtering conditions
  } else {
    data <- data[cond_ind, ]

    # to restore label attributes information
    data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
  }

  # proper column-selecting
  if (nrow(data) == 0) {
    data <- backup_data
    data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
    data <- data[vars]
    data <- as.data.frame(data)
    names(data) <- vars
  } else if (length(vars) > 1) {
    data <- data[vars]
    data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
  } else if (length(vars) == 1) {
    data <- data[vars]
    data <- as.data.frame(data)
    names(data) <- vars
    data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
  } else {
    data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
  }

  return(data)
}
