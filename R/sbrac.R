#' Safely Extract Elements of a Labeled Data Frame
#'
#' @description
#' `sbrac` allows one to do "bracket-like" row and/or column selection
#' (without actual brackets) on a labelr-labeled data.frame in a manner that
#' preserves its labelr label attributes.
#'
#' @details
#' `sbrac` allows one to select rows and columns of a data.frame in a bracket-
#' like fashion, using integers (positional indices), logicals, or (partial)
#' character names (row names and column/variable names). Whereas some forms of
#' Base R bracket subsetting will discard (destroy) labelr attributes, `sbrac`
#' preserves them. For example, sbrac(df, 1:5, 2:4) returns a data.frame
#' (with any labelr attribute meta-data intact), consisting of rows 1-5 and
#' columns 2-4 of data.frame df; while sbrac(mtcars, "Maz", "a", partial = TRUE)
#' returns all variables with substring "a" in their names for all rows with
#' substring "Maz" in their row names. Integer indices (only) can be enclosed in
#' -c() for negative index selection (i.e., "select not these"), (where
#' sbrac(df, -c(1,2), "x", partial = TRUE) means select all rows of df other
#' than rows 1 and 2 and all columns that feature the letter "x" in their names.
#' See also `ssubset`, `sfilter`, `sselect`, `sdrop`, `ssort`, and `srename`, as
#' well as `slab` and `flab` for other labelr attribute-preserving subsetting
#' tools.
#'
#' @param data the data.frame from which columns will be selected.
#' @param ri row indices (integer positional or logical) or row.names (or
#' partial row.names if partial = TRUE) to be selected.
#' @param ci column indices (integer positional or logical) or column / variable
#' names (or partial column names if partial = TRUE) to be selected.
#' @param partial if TRUE, any row or column that contains the relevant
#' character substring will be selected (e.g., sbrac(mtcars, c("Maz"), c("di"))
#' will return all "Mazda" car rows and the column "disp").
#' @return a labelr label attribute-preserving data.frame, consisting of the
#' selected rows and/or columns index-selected.
#' @export
#' @examples
#' # create a copy of the mtcars data set
#' mtc2 <- mtcars
#'
#' # variable names and their labels
#' names_labs_vec <- c(
#'   "mpg" = "Miles/(US) gallon",
#'   "cyl" = "Number of cylinders",
#'   "disp" = "Displacement (cu.in.)",
#'   "hp" = "Gross horsepower",
#'   "drat" = "Rear axle ratio",
#'   "wt" = "Weight (1000 lbs)",
#'   "qsec" = "1/4 mile time",
#'   "vs" = "Engine (0 = V-shaped, 1 = straight)",
#'   "am" = "Transmission (0 = automatic, 1 = manual)",
#'   "gear" = "Number of forward gears",
#'   "carb" = "Number of carburetors"
#' )
#'
#' # assign variable labels
#' mtc2 <- add_name_labs(mtc2,
#'   vars = names(names_labs_vec),
#'   labs = names_labs_vec
#' )
#'
#' # examples of sbrac() functionality
#' sbrac(mtc2, 1:4, ) # commas used in a bracket-like way: row 1:4 and all cols
#' sbrac(mtc2, , 1:4) # commas used in a bracket-like way: all rows and cols 1:4
#' sbrac(mtc2, 1, 2) # 1 is row, 2 is col
#' sbrac(mtc2, -c(8:32), -c(1:8)) # select NOT-these rows and cols (-)
#' sbrac(mtc2, 1:5, 1:2) # rows 1-5, cols 1 and 2
#'
#' # if partial = TRUE, partial matching to get all Mazda or Merc + all
#' # ...vars with "ar" in name
#' sbrac(mtc2, c("Mazda", "Merc"), c("ar"), partial = TRUE) # see what this does
#' mtc3 <- sbrac(mtc2, c("45"), 1:2, partial = TRUE) # see what this does
#' get_labs_att(mtc3, "name.labs") # name.labs still there
sbrac <- function(data, ri = NULL, ci = NULL, partial = FALSE) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  these_atts <- get_all_lab_atts(data)
  using_char <- FALSE

  if (partial) {
    # determine nature of ci vector
    if (!is.null(ci)) {
      colnames_data <- names(data)
      if (is.character(ci)) {
        using_char <- TRUE
        col_names <- gremlr(ci, colnames_data, vals = TRUE)
        col_char_names <- col_names
      } else {
        col_names <- colnames_data[ci]
        col_char_names <- colnames_data[ci]
      }
    }

    # determine nature of ri vector
    if (!is.null(ri)) {
      rownames_data <- rownames(data)
      if (is.character(ri)) {
        using_char <- TRUE
        row_names <- gremlr(ri, rownames_data, vals = TRUE)
        row_char_names <- row_names
      } else {
        row_names <- rownames_data[ri]
        row_char_names <- rownames_data[ri]
      }
    }
  } else { # if not doing partial matching
    # determine nature of ci vector
    if (!is.null(ci)) {
      names_data <- names(data)
      if (is.character(ci)) {
        col_names <- ci[which(ci %in% names_data)]
        col_char_names <- col_names
      } else {
        col_names <- names_data[ci]
        col_char_names <- names_data[ci]
      }
    }

    # determine nature of ri vector
    if (!is.null(ri)) {
      rownames_data <- rownames(data)
      if (is.character(ri)) {
        row_names <- ri[which(ri %in% rownames_data)]
        row_char_names <- row_names
      } else {
        row_names <- rownames_data[ri]
        row_char_names <- rownames_data[ri]
      }
    }
  }

  if (!is.null(ri) && is.null(ci)) {
    data <- data[row_char_names, , drop = FALSE]
    data <- as.data.frame(data)
    rownames(data) <- row_char_names
  } else if (!is.null(ri) && !is.null(ci)) {
    data <- data[row_char_names, col_char_names]
    data <- as.data.frame(data)
    rownames(data) <- row_char_names
    colnames(data) <- col_char_names
  } else if (is.null(ri) && !is.null(ci)) {
    data <- data[col_char_names]
    data <- as.data.frame(data)
    colnames(data) <- col_char_names
  } else {
    data <- data
  }

  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
