#' Convert from Haven-style to labelr Variable Value Labels
#'
#' @description
#' Convert a data.frame with Haven package-style labels to a data.frame with
#' labelr name labels and `add_val_labs`-style one-to-one, value labels.
#'
#' @param data the data.frame with Haven-style vector value label attributes.
#' @param max.unique.vals constrains the variables that may receive value labels
#' to those whose total unique values do not exceed the integer value supplied
#' to this argument. Note that labelr sets a hard ceiling of 5000 on the total
#' number of unique value labels that any variable is permitted to have under
#' any circumstance, as labelr is primarily intended for interactive use with
#' moderately-sized (<=~1M-row) data.frames.
#'
#' @return a data.frame.
#' @export
#' @examples
#' # convert haven vector value labels to labelr value labels
#' library(haven)
#' library(tibble)
#' x1 <- labelled(1:8, c(good = 1, bad = 5))
#' x2 <- labelled(1:8, c(good = 1, mediocre = 4, bad = 5, horrible = 8))
#'
#' # make this a tibble
#' hdf <- tibble::tibble(x1, x2)
#' hdf # how it looks
#'
#' # convert value labels to labelr label values
#' hdf1 <- convert_labs(hdf)
#'
#' # show select values of hdf1
#' head(hdf1)
#'
#' # show that labelr labels are there for the using
#' head(use_val_labs(hdf1))
#'
#' # filter hdf1 using x1's "bad" labelr value label (with flab())
#' head(flab(hdf1, x1 == "bad"), 3)
#'
#' # filter hdf1 using x1's "good" value label (with flab())
#' head(flab(hdf1, x1 == "good"), 3)
#'
#' # return select rows and columns with slab()
#' slab(hdf1, x2 %in% c("good", 2), x2)
#' slab(hdf1, x2 %in% c("good", 2), x1)
#'
convert_labs <- function(data, max.unique.vals = 50) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # name label conversions
  name_labs <- names(data)
  names(name_labs) <- names(data)

  for (i in names(data)) {
    if (any(names(attr(data[[i]], "label")) == i)) {
      name_labs[i] <- unname(attr(
        data[[i]],
        "label"
      ))[names(attr(
        data[[i]],
        "label"
      )) == i]
    }
  }

  attr(data, "name.labs") <- name_labs

  # value label conversions
  for (i in seq_along(names(data))) {
    this_var <- names(data)[i]
    other_lab_pres <- !is.null(attributes(data[[this_var]])$labels)
    if (other_lab_pres) {
      these_vals <- unname(attributes(data[[this_var]])$labels)
      these_labs <- names(attributes(data[[this_var]])$labels)
      if (is.factor(data[[this_var]])) {
        data[[this_var]] <- as.character(data[[this_var]])
        warning(sprintf("
Variable --%s-- converted from factor to character.\n", this_var))
      }

      if (has_decv(data[[this_var]])) {
        data[[this_var]] <- as.character(data[[this_var]])
      }

      attributes(data[[this_var]]) <- NULL
      data <- add_val_labs(data, this_var,
        vals = these_vals,
        labs = these_labs,
        max.unique.vals = max.unique.vals
      )

      data[[this_var]] <- as_numv(data[[this_var]])
    }
  }

  # re-arrange attributes as needed
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
