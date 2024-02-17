#' Convert a Value-labeled Integer Variable Column to a Factor Variable Column
#'
#' @description
#' `lab_int_to_factor` converts a value-labeled integer variable to a factor,
#' using labelr value labels as factor level labels and returning the modified
#' data.frame.
#'
#' @details
#' Note 1: `int2f` is a compact alias for `lab_int_to_factor`: they do the same
#' thing, and the former is easier to type.
#'
#' Note 2: This function can be used to produce ordered factors but will not do
#' so by default (see argument ordered).
#'
#' Note 3: This function's effects are NOT straightforwardly "undone" by
#' `factor_to_lab_int()` See the latter's documentation for more information and
#' an example demonstration.
#'
#' @param data a data.frame object.
#' @param var the (unquoted) name of a value-labeled integer variable found in
#' data.
#' @param ordered logical flag to determine if resulting factor levels should be
#' regarded as ordered (in ascending order of the integer values of var).
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
#'
#' # copy data.frame mtcars to mt2
#' mt2 <- mtcars
#'
#' # add value labels to mtcars$carb and assign data.frame to object mt2
#' mt2 <- add_val_labs(
#'   data = mt2,
#'   vars = "carb",
#'   vals = c(1, 2, 3, 4, 6, 8),
#'   labs = c(
#'     "1c", "2c", # a tad silly, but these val labels will demo the principle
#'     "3c", "4c",
#'     "6c", "8c"
#'   )
#' )
#'
#' # carb as labeled integer
#' class(mt2$carb)
#' levels(mt2$carb)
#' head(mt2$carb, 3)
#' lm(mpg ~ carb, data = mt2)
#' (adj_r2_int <- summary(lm(mpg ~ carb, data = mt2))$adj.r.squared)
#' AIC(lm(mpg ~ carb, data = mt2))
#'
#' # carb as factor
#' carb_fac <- mt2 # copy mt2 to new data.frame carb_fac
#' carb_fac <- lab_int_to_factor(carb_fac, carb) # alias int2f() also works
#' class(carb_fac$carb)
#' levels(carb_fac$carb)
#' head(carb_fac$carb, 3)
#' lm(mpg ~ carb, data = carb_fac)
#' (adj_r2_fac <- summary(lm(mpg ~ carb, data = carb_fac))$adj.r.squared)
#' AIC(lm(mpg ~ carb, data = carb_fac))
lab_int_to_factor <- function(data, var, ordered = FALSE) {
  var_name <- deparse(substitute(var))

  val_labs_att <- paste0("val.labs.", var_name)
  if (!check_labs_att(data, val_labs_att)) {
    stop(sprintf(
      "
No value labels found for supplied var --%s--.",
      var_name
    ))
  }

  var_old <- data[[var_name]]

  if (!is.numeric(var_old) || has_decv(var_old)) {
    stop("
var is not but must be an integer variable.")
  }

  labs <- unname(get_labs_att(data, paste0("val.labs.", var_name))[[1]])
  labs <- labs[labs != "NA"]

  data <- use_val_labs(data, vars = var_name)
  data[[var_name]] <- factor(data[[var_name]],
    labels = labs,
    ordered = ordered
  )
  return(data)
}

#' @export
#' @rdname lab_int_to_factor
int2f <- lab_int_to_factor
