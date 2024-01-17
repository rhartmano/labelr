#' Return Last Rows of a Data Frame with Value Labels Visible
#'
#' @description
#' `taill` accepts a labelr value-labeled data.frame and returns the last n
#' value-labeled rows of that data.frame
#'
#' @details
#' Whereas `utils::tail` returns the last n rows of a data.frame, `taill` does
#' the same thing, substituting value labels for values wherever the former exist.
#' See also `headl` and `somel`.
#'
#' @param data a data.frame.
#' @param n the number of consecutive rows at the end / bottom of the data.frame
#' to return.
#' @return a data.frame.
#' @importFrom utils head tail
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
#' # let's add variable VALUE labels for variable "gender"
#' # note that, if we are labeling a single variable, we can use add_val1()
#' # distinction between add_val1() and add_val_labs() will become more meaningful
#' # when we get to our Likert example
#' df <- add_val1(
#'   data = df, gender, vals = c(0, 1, 2),
#'   labs = c("M", "F", "O"), max.unique.vals = 50
#' )
#'
#' tail(df) # utils::tail
#' taill(df) # same, but with value labels in place of values
taill <- function(data, n = 6L) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  to_n <- nrow(data)
  from_n <- to_n - (n - 1)
  data <- sbrac(data, from_n:to_n, )
  tail(use_val_labs(data), n = n)
}
