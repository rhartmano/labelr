#' Return a Random Sample of Data Frame Rows with Value Labels Visible
#'
#' @description
#' `somel` accepts a labelr value-labeled data.frame and returns a random sample
#' of n value-labeled rows of that data.frame
#'
#' @details
#' `somel` is inspired by the function `some` from the car package. See also
#' `headl` and `taill`.
#' @param data a data.frame.
#' @param n the number of random rows of the data.frame to return.
#' @return a data.frame.
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
#' somel(df) # six random rows with value labels visible
somel <- function(data, n = 6L) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  these_atts <- get_all_lab_atts(data)
  inds2sample <- seq_len(nrow(data))
  inds <- sample(inds2sample, n, replace = FALSE)
  datax <- data[inds, ]
  datax <- add_lab_atts(datax, these_atts, num.convert = FALSE)
  data <- use_val_labs(datax)
  return(data)
}
