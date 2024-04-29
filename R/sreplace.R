#' Safely Replace a Data Frame Variable (Column)
#'
#' @description
#' `sreplace` allows one to replace a data.frame column with new values, while
#' preserving the labelr attributes attached to the inputted data.frame.
#'
#' @details
#' Note that, while `sreplace` preserves label meta-data of the supplied
#' data.frame, it does not update or add any new labels. Therefore, if you are
#' altering the range of values for an extant variable, you will need to
#' explicitly instantiate any new or modified labels that you desire via
#' follow-up calls to functions such as `add_val_labs()`, `drop_val_labs()`, etc.
#'
#' You may not use `sreplace` to generate a new variable (i.e., one not already
#' present in a the supplied data.frame). For that, see `sgen` or `schange`.
#' @param data a data.frame.
#' @param ... an expression that replaces a column in the data.frame.
#' @return a data.frame.
#' @export
#' @examples
#' df <- mtcars
#' # now, add value labels
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' df <- add_val_labs(
#'   data = df,
#'   vars = "carb",
#'   vals = c(1, 2, 3, 4, 6, 8),
#'   labs = c(
#'     "1-carb", "2-carbs",
#'     "3-carbs", "4-carbs",
#'     "6-carbs", "8-carbs"
#'   )
#' )
#'
#' df <- sreplace(df, mpg = mpg^2) # replace var "mpg"
#' df <- sreplace(df, am = ifelse(am == 0, 2, am)) # replace var "am"
#'
#' head(df, 4) # show that data.frame modifications have been made
#' get_all_lab_atts(df)
#'
#' df <- add_quant_labs(
#'   data = df,
#'   vars = "mpg",
#'   vals = c(200, 400, 600, 1000, 1500),
#'   labs = NULL
#' )
#'
#' df <- drop_val_labs(
#'   data = df,
#'   vars = "am"
#' )
#'
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am",
#'   vals = c(1, 2),
#'   labs = c("manual", "automatic")
#' )
#'
#' get_val_labs(df)
sreplace <- function(data, ...) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  lab_atts_list <- get_all_lab_atts(data)

  length_test <- as.character((as.list(substitute(...()))))
  if (length(length_test) > 1) {
    stop("
\nYou may not pass more than one expression for evaluation.")
  }

  x <- eval(substitute(alist(...)))
  name_x <- names(x)

  if (!name_x %in% names(data)) {
    stop("
\nSpecified variable not found in data.frame (i.e., nothing to sreplace()).
Try sgen() or schange()?")
  }
  x <- eval(parse(text = as.character(x)), envir = data)
  data[[name_x]] <- x

  data <- add_lab_atts(data, lab_atts_list, num.convert = FALSE)
  return(data)
}
