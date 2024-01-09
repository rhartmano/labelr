#' Initialize labelr Attributes
#'
#' @description
#' `init_labs` pre-populates a data.frame with "placeholder" labelr label meta-
#' data, which will be overwritten if/when you explicitly assign your own
#' preferred label attributes.
#'
#' @details
#' `init_labs` is used inside other labelr functions but is not intended for
#' interactive use at the console.
#'
#' @param data the data.frame that you will be labeling via functions like
#' `add_val_labs` and `add_name_labs`.
#' @param max.unique.vals constrains the variables that may receive value labels
#' to those whose total unique values do not exceed the integer value supplied
#' to this argument. Note that labelr sets a hard ceiling of 5000 on the total
#' number of unique value labels that any variable is permitted to have under
#' any circumstance, as labelr is primarily intended for interactive use with
#' moderately-sized (<=~1M-row) data.frames.
#'
#' @return a data.frame with initial placeholder labelr meta-data added.
#'
#' @examples
#' # make toy demographic (gender, race, etc.) data set
#' df <- make_demo_data(n = 1000, seed = 555) # another labelr:: function
#' df2 <- init_labs(df) # df2 is not df
#' get_all_lab_atts(df) # this is df; is not df2
#' get_all_lab_atts(df2) # this is df2
#' @export
init_labs <- function(data, max.unique.vals = 5000) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # initialize name.labs, if not already present
  if (is.null(attributes(data)[["name.labs"]])) {
    vals_vec <- names(data)
    names(vals_vec) <- names(data)
    attr(data, "name.labs") <- vals_vec
  }

  # keep only variables that stay within max.unique.vals
  elig_vars <- names(data)[sapply(
    data,
    function(x) length(unique(x)) <= max.unique.vals
  )]

  # streamline the data.frame
  sunique <- function(data, vars = NULL) {
    lab_atts <- get_all_lab_atts(data)
    if (!is.null(vars)) {
      data <- data[vars]
      data <- as.data.frame(data)
      names(data) <- vars
    }

    data_unique <- unique(data)
    data_unique <- add_lab_atts(data_unique, lab_atts, num.convert = FALSE)
    return(data_unique)
  }

  ### streamline your data.frame
  data_unique <- sunique(data, vars = elig_vars)

  # initialize val.labs for applicable variables, if necessary
  # initialize value labels or update preexisting variable value labels
  init_val_labs <- function(data, unique.vals = 5000) {
    for (i in seq_along(colnames(data))) {
      this_var_name <- colnames(data)[i]
      this_val_label <- paste0("val.labs.", this_var_name)
      atts_already_test <- check_labs_att(data, this_val_label)
      has_dec_test <- has_decv(data[[this_var_name]])
      prohib_class_test <- !check_class(data[[this_var_name]])

      if (!any(atts_already_test, has_dec_test, prohib_class_test)) {
        x <- as.character(data[[this_var_name]])
        x <- irregular2v(x, to = "NA", inf.include = TRUE, nan.include = TRUE)
        vals <- sort(unique(x))
        names(vals) <- vals

        if (length(vals) <= unique.vals) {
          attr(data, this_val_label) <- vals
        }
      }
    }
    return(data)
  }

  if (nrow(data) < max.unique.vals) max.unique.vals <- nrow(data)
  data_unique <- init_val_labs(data_unique, unique.vals = max.unique.vals)

  # initialize factor information, if applicable
  if (!check_any_lab_atts(data_unique, "factor.") && any(sapply(data_unique, is.factor))) {
    data_unique <- add_factor_info(data_unique)
  }

  lab_atts <- get_all_lab_atts(data_unique)

  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
