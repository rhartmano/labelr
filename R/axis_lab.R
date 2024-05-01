#' Retrieve Variable's Name Label for Plot Labeling
#'
#' @description
#' `axis_lab` accepts a data.frame and single unquoted variable name and returns
#' that variable's name label for use in axis labeling or plot labeling function
#' options.
#'
#' @details
#' Note 1: `alb` is a compact alias for `axis_lab`: they do the same thing,
#' and the former is easier to type.
#'
#' Note 2: This command is intended exclusively for interactive use. In
#' particular, the var argument must be the literal name of a single variable
#' (column) found in the supplied data.frame and may NOT be, e.g., the name of a
#' character vector that contains the variable (column name) of interest.
#'
#' @param data a data.frame.
#' @param var the unquoted name of a variable that exists in the data.frame and
#' is name-labeled (using `add_name_labs()`).
#' @return a 1L character vector with var's name label.
#' @export
#' @examples
#' # copy mtcars to df
#' # create a data set
#' df <- mtcars
#'
#' # variable names and their labels
#' names_labs_vec <- c(
#'   "mpg" = "Miles/(US) gallon",
#'   "cyl" = "Number of cylinders",
#'   "wt" = "Weight (1000 lbs)"
#' )
#'
#' df <- add_name_labs(df, name.labs = names_labs_vec)
#'
#' # ggplot example of axis_lab()
#' library(ggplot2)
#' p <- ggplot(df, aes(mpg, wt, color = cyl)) +
#'   geom_point()
#' p <- p +
#'   labs(color = axis_lab(df, cyl)) +
#'   xlab(axis_lab(df, mpg)) +
#'   ylab(axis_lab(df, wt))
#'
#' # Base R plot example (using alb() alias)
#' with(df, plot(mpg, wt,
#'   xlab = alb(df, mpg),
#'   ylab = alb(df, wt)
#' ))
#'
axis_lab <- function(data, var) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  var <- deparse(substitute(var))
  test_quote <- any(grepl("\"", var))
  if (test_quote && is.character(var)) var <- gsub("\"", "", var)
  name_lab <- get_name_labs(data, var)$lab
  return(name_lab)
}

#' @export
#' @rdname axis_lab
alb <- axis_lab
