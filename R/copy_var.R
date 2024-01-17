#' Copy a Data Frame Variable and its Value labels to Another Variable
#'
#' @description
#' Note: `copy_var` copies an existing variable and its value labels from a
#' data.frame to another new or (if force = TRUE) existing variable of the
#' data.frame.
#'
#' @details
#' Any non-labelr R operation that changes a variable's (column's) name or that
#' copies its contents to another variable (column) with a different name will
#' not associate the original variable's value labels with the new variable name.
#' To mitigate this, `copy_var` allows one to copy both a variable (column) and
#' its value labels and assign those to another variable.
#'
#' @param data a data.frame to which variable value labels will be added.
#' @param from.var the unquoted name of the variable whose values and labels will
#' be assigned to the to.var. This variable must presently exist in the data.frame.
#' @param to.var the unquoted name of the variable to which the from.var's values
#' and labels will be assigned. If force = FALSE, this must be a new variable name
#' (one that does not refer to a variable that already exists in the data.frame).
#' @param force if to.var already exists in the data.frame, allow it to be
#' overwritten. If FALSE, this will not be allowed, and an error will be issued.
#'
#' @return A data.frame.
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
#' head(df, 4)
#' df <- copy_var(df, from.var = raceth, to.var = re_copy)
#' df <- copy_var(df, from.var = x1, to.var = var1)
#' head(df, 4)
#' get_val_labs(df)
copy_var <- function(data, from.var, to.var, force = FALSE) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  these_atts <- get_all_lab_atts(data)

  # make this work with or without the variable being quoted
  to.var <- deparse(substitute(to.var))
  test_quote <- any(grepl("\"", to.var))
  if (test_quote && is.character(to.var)) to.var <- gsub("\"", "", to.var)

  # make this work with or without the variable being quoted
  to.var <- deparse(substitute(to.var))

  test_quote <- any(grepl("\"", to.var))
  if (test_quote && is.character(to.var)) to.var <- gsub("\"", "", to.var)

  # make this work with or without the variable being quoted
  from.var <- deparse(substitute(from.var))

  test_quote_2 <- any(grepl("\"", from.var))
  if (test_quote_2 && is.character(from.var)) {
    from.var <- gsub(
      "\"", "",
      from.var
    )
  }

  # verify variables are in the data.frame
  from_out <- !from.var %in% names(data)

  if (from_out) {
    stop("
\nSpecified from.var is not present in the supplied data.frame\n")
  }

  # warn the user if they are replacing a variable
  if (any(to.var %in% names(data)) && force) {
    data[[to.var]] <- NULL
    warning(sprintf("
\nVar --%s-- was already present in this data.frame and will be replaced.\n", to.var))
  } else if (any(to.var %in% names(data)) && !force) {
    stop(sprintf("
\nVar --%s-- was already present in this data.frame and cannot be replaced unless
option force = TRUE.\n", to.var))
  }

  x <- data[[from.var]]

  data <- cbind(data, x)
  data <- as.data.frame(data)
  names(data)[ncol(data)] <- to.var
  to_var_val_label <- paste0("val.labs.", to.var)
  from_var_val_label <- paste0("val.labs.", from.var)

  data <- add_lab_atts(data, these_atts, num.convert = FALSE)

  if (check_labs_att(data, from_var_val_label)) {
    attributes(data)[[to_var_val_label]] <- attributes(data)[[from_var_val_label]]
  }

  # re-arrange attributes as needed
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
