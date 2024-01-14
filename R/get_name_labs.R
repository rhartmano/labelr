#' Return Lookup Table of Variable Names and Name Labels
#'
#' @description
#' For a name-labeled data.frame, `get_name_labs` returns a derivative
#' data.frame that lists each variable and its variable name label.
#'
#' Note: `gnl` is a compact alias for `get_name_labs`: they do the same thing,
#' and the former is easier to type
#'
#' @param data a data.frame.
#' @param vars a character vector with the name(s) of any specific variable(s)
#' (If NULL, returned data.frame will contain all variable name labels).
#' @return A two-column data.frame, consisting of "var" and "lab" columns,
#' where each row corresponds to a unique variable (column) from the user-
#' supplied data.frame.
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
get_name_labs <- function(data, vars = NULL) {
  if (!is.null(attributes(data)$name.labs)) {
    labs_back <- attributes(data)$name.labs
    if (!is.null(vars)) {
      vars <- gremlr(vars, names(data), vals = TRUE)
      labs_back <- labs_back[names(labs_back) %in% vars]
    }
  } else {
    warning("\n \n  No name.labs found.\n")
    labs_back <- "NA"
    names(labs_back) <- "NA"
  }

  labs_back <- data.frame(var = names(labs_back), lab = unname(labs_back))

  return(labs_back)
}

#' @export
#' @rdname get_name_labs
gnl <- get_name_labs
