#' Is This a Many-to-One-Style Value-labeled Variable (Column)?
#'
#' @description
#' Determine whether a specific variable of a data.frame has many-to-one-style
#' value labels associated with it (i.e., via `add_m1_lab()` or `add1m1()`).
#'
#' @details
#' `hm1l` is a compact alias for `has_m1_labs`: they do the same thing, and the
#' former is easier to type
#'
#' @param data a data.frame.
#' @param var the unquoted name of the variable (column) to check for the
#' presence of many-to-one-style value labels.
#' @return A 1L logical.
#' @export
#' @examples
#' # add many-to-one style labels for "carb" and one-to-one style for "am"
#' df <- mtcars
#'
#' df <- add_m1_lab(df,
#'   vars = "carb",
#'   vals = 1:3,
#'   lab = "<=3",
#'   max.unique.vals = 10
#' )
#'
#' df <- add_m1_lab(df,
#'   vars = "carb",
#'   vals = c(4, 6, 8),
#'   lab = ">=4",
#'   max.unique.vals = 10
#' )
#'
#' df <- add_val_labs(df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("autom", "manu"),
#'   max.unique.vals = 10
#' )
#'
#' has_m1_labs(df, carb) # TRUE, carb has m1-style value labels
#'
#' has_val_labs(df, am) # TRUE, am does have value labels
#'
#' has_m1_labs(df, am) # FALSE, am's value labels are not not m1-style labels
has_m1_labs <- function(data, var) {
  # capture var argument
  vars <- deparse(substitute(var))
  test_quote <- any(grepl("\"", vars))
  if (test_quote && is.character(vars)) vars <- gsub("\"", "", vars)
  vars <- gsub("c\\(", "", vars)
  vars <- gsub("\\(", "", vars)
  vars <- gsub("\\)", "", vars)

  # test for presence of var in data.frame
  if (!all(vars %in% names(data)) || length(vars) != 1) {
    stop("
\nInvalid var argument specification: var arg should be a single, unquoted
name of a variable that is present in the data.frame.
         ")
  }

  att <- paste0("val.labs.", vars)
  att_list <- get_all_lab_atts(data)
  check_logical <- check_labs_att(data, att)
  if (check_logical) {
    test_val <- length(unique(att_list[att][[1]])) != length(att_list[att][[1]])
  } else {
    test_val <- FALSE
  }
  return(test_val)
}


#' @export
#' @rdname has_m1_labs
hm1l <- has_m1_labs
