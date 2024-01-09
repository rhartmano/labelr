#' Safely Generate a Data Frame Variable (Column)
#'
#' @description
#' `sgen` allows one to add a column to a data.frame while preserving the labelr
#' attributes attached to the inputted data.frame.
#'
#' @details
#' Note that, while `sgen` preserves label meta-data of the supplied
#' data.frame, it does not update or add any new labels. Therefore, you will
#' need to explicitly instantiate any new or modified labels that you desire via
#' follow-up calls to functions such as `add_val_labs()`, `drop_val_labs()`, etc.
#'
#' You may not use `sgen` to replace a variable already present in a the supplied
#' data.frame. For that, see `sreplace` or `schange`.
#' @param data a data.frame.
#' @param ... an expression that will create a new column in the data.frame.
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
#' df <- sgen(df, mpg_sq = mpg^2) # create var "mpg_sq"
#' df <- sgen(df, am2 = ifelse(am == 0, 2, am)) # create var "am2"
#'
#' head(df, 4) # show that data.frame modifications have been made
#' get_all_lab_atts(df)
#'
#' df <- add_quant_labs(
#'   data = df,
#'   vars = "mpg_sq",
#'   vals = c(200, 400, 600, 1000, 1500),
#'   labs = NULL
#' )
#'
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am2",
#'   vals = c(1, 2),
#'   labs = c("manual", "automatic")
#' )
#'
#'
#' get_val_labs(df)
sgen <- function(data, ...) {
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

  if (name_x %in% names(data)) {
    stop("
\n Specified variable is already present in data.frame.
Try sreplace() or schange()?")
  }
  x <- eval(parse(text = as.character(x)), envir = data)
  data[[name_x]] <- x

  data <- add_lab_atts(data, lab_atts_list, num.convert = FALSE)
  return(data)
}
