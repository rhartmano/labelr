#' Subset a data Frame Using Value Labels
#'
#' @description
#' `slab` ("subset using labels") allows one to filter rows and select columns
#' from a data.frame using value or numerical range labels.
#'
#' @details
#' `slab` does base::subset-style data subsetting using variable value label
#' meta-data that are associated with variable values but are not themselves
#' values (i.e., will not appear in response to View(), head(), etc.). `slab`
#' takes as its arguments a labelr value-labeled data.frame, followed by
#' condition-based row-filtering instructions (required) and a list of unquoted
#' names of variables to be retained (optional; all variables returned by
#' default).
#'
#' For example, if value labels were added to the integer nominal variable
#' "raceth" of (notional) data.frame df (using `add_val_labs` or `add_val1`), one
#' could then use flab to filter down to only raceth==3 rows and return only the
#' columns id and raceth, using a call like slab(df, raceth=="African-American", id,
#' raceth) (assuming here that a variable called id was present in df and that the
#' integer value raceth==3 has previously been given the value label
#' "African-American". As another example, slab(mtcars, am=="automatic", mpg,
#' cyl, am, disp) would return (only) the variables mpg, cyl, am, and disp and
#' only those rows of mtcars where automatic==0 (assuming that the value label
#' "automatic" has been uniquely associated with the mtcars$am value of 0 via a
#' prior call to `add_val_labs` or `add_val1`). This functionality may be useful
#' for interactively subsetting a data.frame, where character value labels may
#' be more intuitive and easily recalled than the underlying variable values
#' themselves (e.g., raceth=="White" & gender="F" may be more intuitive or readily
#' recalled than raceth==3 & gender==2).
#'
#' Note that `slab` (and labelr more broadly) is intended for moderate-sized (or
#' smaller) data.frames, defined loosely as those with a few million or fewer
#' rows. With a conventional (c. 2023) laptop, labelr operations on modest-
#' sized (~100K rows) take seconds (or less); with larger (> a few million rows)
#' data.frames, labelr may take several minutes (or run out of memory and fail
#' altogether!), depending on specifics.
#'
#' See also `flab`, `use_val_labs`, `add_val_labs`, `add_val1`,
#' `add_quant_labs`, `add_quant1`, \cr `get_val_labs`, `drop_val_labs`. For
#' label-preserving subsetting tools that subset in terms of raw values (not
#' value labels), see `sfilter`, `sbrac`, `ssubset`, `sdrop`.
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
#'   data = df, gender, vals = c(0, 1, 2),
#'   labs = c("M", "F", "O"), max.unique.vals = 50
#' )
#'
#' # see what we did
#' # get_val_labs(df)
#' get_val_labs(df, "gender")
#' get_val_labs(df, "raceth")
#'
#' # use --labels-- to filter w/ flab() ("*F*ilter *lab*el")
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
  } else if (length(vars) >= 1) {
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
