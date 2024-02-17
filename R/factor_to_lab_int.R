#' Convert a Factor Variable Column to Value-labeled Integer Variable Column
#'
#' @description
#' `factor_to_lab_int` converts a factor variable (column) of a data.frame to a
#' value-labeled integer variable and converts the factor level labels to labelr
#' value labels, returning the modified data.frame.
#'
#' @details
#' Note 1: `f2int` is a compact alias for `factor_to_lab_int`: they do the same thing,
#' and the former is easier to type.
#'
#' Note 2: `factor_to_lab_int()` is NOT an "undo" for `lab_int_to_factor()`.
#' `factor_to_lab_int()` will assign sequential integer values from 1 to k (the
#' number of distinct factor levels) in factor level order, and this will not
#' necessarily match the integer values of a variable previously subjected to a
#' `lab_int_to_factor()` call. See extended second example below for demo.
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
#'
#' # copy data.frame mtcars to mt2
#' carb_orig_int <- mtcars
#'
#' # !NOTE! factor_to_lab_int() is NOT an "undo" for lab_int_to_factor()
#' # Integer values will be sequential integers in factor level order
#' # Demo this
#'
#' # add value labels to mtcars$carb; and assign data.frame to carb_orig_int
#' carb_orig_int <- add_val_labs(
#'   data = mtcars,
#'   vars = "carb",
#'   vals = c(1, 2, 3, 4, 6, 8),
#'   labs = c(
#'     "1c", "2c", # a tad silly, but these value labels will demo the principle
#'     "3c", "4c",
#'     "6c", "8c"
#'   )
#' )
#'
#' # carb as labeled numeric
#' class(carb_orig_int$carb) # numeric
#' levels(carb_orig_int$carb) # none, not a factor
#' head(carb_orig_int$carb, 3) # compare to carb_to_int (below)
#' mean(carb_orig_int$carb) # compare to carb_to_int (below)
#' lm(mpg ~ carb, data = carb_orig_int) # compare to carb_to_int (below)
#' (adj_r2_int <- summary(lm(mpg ~ carb, data = carb_orig_int))$adj.r.squared)
#' AIC(lm(mpg ~ carb, data = carb_orig_int)) # compare to carb_to_int (below)
#'
#' # carb as factor
#' carb_fac <- carb_orig_int # copy carb_orig_int to new data.frame carb_fac
#' carb_fac <- lab_int_to_factor(carb_fac, carb) # alias int2f() also works
#' class(carb_fac$carb) # factor
#' levels(carb_fac$carb) # has levels
#' head(carb_fac$carb, 3)
#' lm(mpg ~ carb, data = carb_fac) # factor
#' (adj_r2_fac <- summary(lm(mpg ~ carb, data = carb_fac))$adj.r.squared)
#' AIC(lm(mpg ~ carb, data = carb_fac)) # compare to R2, AIC for carb_to_int
#'
#' # ??back?? to integer? Not quite. Compare carb_to_int to carb_orig_int
#' carb_to_int <- carb_fac # copy carb_fac to carb_to_int
#' carb_to_int <- factor_to_lab_int(carb_to_int, carb) # alias int2f() also works
#' class(carb_to_int$carb) # Is an integer
#' levels(carb_to_int$carb) # NOT a factor
#' head(carb_to_int$carb, 3) # NOT the same as carb_orig_int
#' mean(carb_to_int$carb) # NOT the same as carb_orig_int
#' lm(mpg ~ carb, data = carb_to_int) # NOT the same as carb_orig_int
#' (adj_r2_fac <- summary(lm(mpg ~ carb, data = carb_to_int))$adj.r.squared)
#' AIC(lm(mpg ~ carb, data = carb_to_int)) # NOT the same as carb_orig_int
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
