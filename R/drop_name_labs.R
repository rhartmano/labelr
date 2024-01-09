#' Remove Name Label Attributes from a Data Frame
#'
#' @description
#' Remove one or more descriptive variable name label attributes previously
#' added to a data.frame using `add_name_labs`.
#'
#' Note: `dnl` is a compact alias for `drop_name_labs`: they do the same thing,
#' and the former is easier to type
#'
#' @details
#' `drop_name_labs` works with `add_name_labs`, `get_name_labs` and
#' `use_name_labs` to facilitate creation, accessing, substitution, and removal
#' of variable name labels for variable names. Each variable (column) of a
#' data.frame can receive one and only one "name label," which typically is a
#' noun phrase that expounds the meaning or contents of the variable's name
#' (e.g., "Weight in ounces at birth" might be a viable name label for a column
#' called "wgt"). `drop_name_labs` takes a data.frame and (optionally) a
#' character vector of variables whose name labels should be discarded: If only
#' a data.frame is provided, all variable name labels will be dropped. You can
#' assign new name labels using new calls to `add_name_labs` (If you wish to
#' change some or all name labels, you do not need to call `drop_name_labs`: you
#' can simply pass the new name labels to `add_name_labs`, and they will
#' overwrite the old ones (including any automatically generated provisional
#' ones), while leaving in place any previously added name labels that you do
#' not explicitly replace).
#'
#' @param data the data.frame with name labels that you wish to drop (and which
#' were added using `add_name_labs`).
#' @param vars the names of the columns/variables (not the name labels) whose
#' name labels you wish to drop. If NULL, all variables' name labels will be
#' dropped.
#' @return The same data.frame you submitted, except that the selected name
#' label attribute meta-data has been removed.
#' @export
#' @examples
#' # create a data set
#' df <- mtcars
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
#' df <- add_name_labs(df,
#'   vars = names(names_labs_vec),
#'   labs = names_labs_vec
#' )
#'
#' # see what we have
#' get_name_labs(df)
#'
#' # drop the name label for var/col "am"
#' df <- drop_name_labs(df, "am")
#'
#' # see what this did to the name label for "am"
#' get_name_labs(df)
#'
#' # now, drop all of the name labels
#' df <- drop_name_labs(df)
#' get_name_labs(df) # they're gone
drop_name_labs <- function(data, vars = NULL) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  any_name_labs <- any(grepl("name.labs", names(get_all_lab_atts(data))))
  if (any_name_labs) {
    if (!is.null(vars)) {
      x <- gremlr(vars, names(data), vals = TRUE)
    } else {
      vars <- names(data)
    }
    x <- vars
    x <- as.character(x)
    x <- unique(x)

    var_name_atts <- get_all_lab_atts(data)[["name.labs"]]
    prior_vals <- names(var_name_atts)
    names2keep <- names(var_name_atts)[!names(var_name_atts) %in% x]
    val_atts_updated <- var_name_atts[names2keep]
    attributes(data)[["name.labs"]] <- val_atts_updated
  } else {
    warning("\n \n  No name.lab variable labels found.\n")
  }

  # update and resort attributes
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
