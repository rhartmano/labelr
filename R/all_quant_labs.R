#' Add Quantile-based Value Labels to All Numeric Vars that Meet Specifications
#'
#' @description
#' Add variable-specific quantile-based value labels to all numeric variables of
#' a data.frame that meet specified conditions.
#'
#' @details
#' Note: `allq` is a compact alias for `all_quant_labs`: they do the same thing,
#' and the former is easier to type.
#'
#' Numerical variables that feature decimals or large numbers of distinct values
#' are not eligible to receive conventional `add_val_labs()`-style value labels.
#' `all_quant_labs` allows one to label such variables based on quantile
#' thresholds.
#'
#' @param data a data.frame.
#' @param qtiles the number of quantile categories to employ (e.g., 4 would
#' indicate quartiles, 5 would indicate quintiles, 10 for deciles, etc.).
#' @param not.vars used to specify any numeric variables that should be exempted
#' from this operation.
#' @param unique.vals.thresh an integer. Numeric variables with fewer than this
#' many unique variables will be exempted from the operation (i.e., will NOT
#' receive quantile value labels).more than this.
#'
#' @return A data.frame, with new variable value labels added.
#' @importFrom stats quantile
#' @export
#' @examples
#' # mtcars demo
#' df <- mtcars
#' get_val_labs(df) # none
#' # add quintile val labs for all numeric vars with >10 unique vals
#' df <- all_quant_labs(data = df, qtiles = 5, unique.vals.thresh = 10)
#' get_val_labs(df) # here now
#' headl(df) # show them; note this is labelr::headl(), not utils::head()
all_quant_labs <- function(data,
                           qtiles = 5,
                           not.vars = NULL,
                           unique.vals.thresh = 10) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  if (nrow(data) > 300000) {
    warning("
\nNote: labelr is not optimized for data.frames this large.")
  }

  var_names <- names(data)
  if (!is.null(not.vars)) var_names <- base::setdiff(var_names, not.vars)

  for (i in var_names) {
    val_labs_i <- paste0("val.labs.", i)
    if (!check_labs_att(data, val_labs_i)) {
      if (is.numeric(data[[i]]) && length(unique(data[[i]])) >= unique.vals.thresh) {
        data <- suppressWarnings(add_quant_labs(data, i, qtiles = qtiles))
      }
    }
  }

  # end main loop over vars being value-labeled
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}

#' @export
#' @rdname all_quant_labs
allq <- all_quant_labs
