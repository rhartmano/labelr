#' Convert a Value-labeled Integer Variable Column to a Factor Variable Column
#'
#' @description
#' `lab_int_to_factor` converts a value-labeled integer variable to a factor,
#' using labelr value labels as factor level labels and returning the modified
#' data.frame.
#'
#' @details
#' Note: `int2f` is a compact alias for `lab_int_to_factor`: they do the same
#' thing, and the former is easier to type.
#'
#' @param data a data.frame object.
#' @param var the (unquoted) name of a value-labeled integer variable found in
#' data.
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' class(iris[["Species"]])
#'
#' iris_sp_int <- factor_to_lab_int(iris, Species)
#' class(iris_sp_int[["Species"]])
#'
#' get_val_labs(iris_sp_int, "Species")
#' iris_sp_fac <- lab_int_to_factor(iris_sp_int, Species)
#'
#' class(iris_sp_fac[["Species"]])
#'
#' levels(iris_sp_fac[["Species"]])
lab_int_to_factor <- function(data, var) {
  var_name <- deparse(substitute(var))

  if (!check_labs_att(data, paste0("val.labs.", var_name))) {
    stop("
var has no value labels.")
  }

  var_old <- data[[var_name]]

  if (!is.numeric(var_old) || has_decv(var_old)) {
    stop("
var is not but must be an integer variable.")
  }

  labs <- unname(get_labs_att(data, paste0("val.labs.", var_name))[[1]])
  labs <- labs[labs != "NA"]

  data <- use_val_labs(data, vars = var_name)
  data[[var_name]] <- factor(data[[var_name]], labels = labs)
  return(data)
}

#' @export
#' @rdname lab_int_to_factor
int2f <- lab_int_to_factor
