#' Convert a Factor Variable Column to Value-labeled Integer Variable Column
#'
#' @description
#' `factor_to_lab_int` converts a factor variable (column) of a data.frame to a
#' value-labeled integer variable and converts the factor level labels to labelr
#' value labels, returning the modified data.frame.
#'
#' @details
#' Note: `f2int` is a compact alias for `factor_to_lab_int`: they do the same thing,
#' and the former is easier to type.
#'
#' @param data a data.frame object.
#' @param var the (unquoted) name of a factor variable found in data.
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' class(iris[["Species"]])
#' iris_df <- factor_to_lab_int(iris, Species)
#' class(iris_df[["Species"]])
#' get_val_labs(iris_df, "Species")
factor_to_lab_int <- function(data, var) {
  var_name <- deparse(substitute(var))
  var_old <- data[[var_name]]

  if (!is.factor(var_old)) {
    stop("
var is not but must be a factor.")
  }

  labs <- as.character(levels(var_old))
  vals <- as.integer(sort(unique(var_old)))

  var_new <- as.integer(var_old)
  data[[var_name]] <- var_new

  data <- add_val_labs(data, var_name, vals = vals, labs = labs)
  return(data)
}


#' @export
#' @rdname factor_to_lab_int
f2int <- factor_to_lab_int
