#' Evaluate an Expression in a Value Labels-on Data Environment
#'
#' @description
#' `with_val_labs` wraps a data.frame in `use_val_labs` and wraps the resulting
#' data.frame in `base::with` in support of `base::with`-like non-standard
#' evaluation (see examples).
#'
#' Note: `wvl` is a compact alias for `with_val_labs`: they do the same thing,
#' and the former is easier to type
#'
#' @details
#' `with_val_labs` (see also alias `wvl`) is useful for applying certain
#' nominal-variable-friendly functions (chiefly, `table` and the like) to
#' value-labeled data.frames. See also `base::with`. See also `tabl`,
#' `with_name_labs`, and `with_both_labs`.
#'
#' @param data a data.frame with value-labeled columns.
#' @param ... additional arguments passed to dots, typically an expression
#' involving a function called on unquoted variable(s) (see examples).
#' @export
#'
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
#' # let's add variable VALUE labels for variable "gender"
#' # note that, if we are labeling a single variable, we can use add_val1()
#' # distinction between add_val1() and add_val_labs() will become more meaningful
#' # when we get to our Likert example
#' df <- add_val1(
#'   data = df, gender, vals = c(0, 1, 2),
#'   labs = c("M", "F", "O"), max.unique.vals = 50
#' )
#'
#' # "with_val_labs" - with()-like function that swaps value labels out for value values
#' # compare with(df, ...) to with_val_labs(df,...)
#' with(df, table(gender, raceth)) # without labels
#'
#' # the same data (note that presentation order changes d/t alphabetical ordering)
#' with_val_labs(df, table(gender, raceth)) # with labels
#' with(use_val_labs(df), table(gender, raceth)) # above is shorthand for this
#'
#' # just raceth
#' with(df, table(raceth)) # with
#' with_val_labs(df, table(raceth)) # with_val_labs
#'
#' # another use case
#' with(df, unique(raceth)) # with
#' with_val_labs(df, unique(raceth)) # with_val_labs
#'
#' # another
#' with(df, modelr::typical(raceth)) # numerical median!
#' with_val_labs(df, modelr::typical(raceth)) # modal label (not the median!)
with_val_labs <- function(data, ...) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  with(use_val_labs(data), ...)
}

#' @export
#' @rdname with_val_labs
wvl <- with_val_labs

