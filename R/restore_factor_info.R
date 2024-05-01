#' Restore Factor Status, Levels to a Character Column of a Labeled Data Frame
#'
#' @description
#' `restore_factor_info` searches a data.frame for labelr-specific factor
#' meta-data (added by `add_factor_info()`) and, if found, uses that information
#' to coerce a character vector that was formerly a factor back into a
#' factor, with former levels and (if applicable) "ordered" factor status,
#' as well.
#' @param data the data.frame to which labelr-specific factor attribute meta-
#' data may have been applied via `add_factor_info`.
#' @return A data.frame.
#' @export
#' @examples
#' # this function does not strictly require prior or other use of labelr
#' zz <- add_factor_info(iris) # we'll find out what this does
#' sapply(zz, class) # Species is a factor
#' zz$Species <- as.character(zz) # now it's a character
#' sapply(zz, class) # yup, it's a character
#' zz <- restore_factor_info(zz) # we'll find out what this does
#' sapply(zz, class) # now it's back to a factor
#' levels(zz$Species) # levels are back, too.
restore_factor_info <- function(data) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  any_factor_att_check <- length(get_factor_atts(data)) > 0
  if (any_factor_att_check) {
    factor_names <- names(get_factor_atts(data))
    unord_factor_names <- factor_names[grep("u.factor", factor_names)]
    unord_factor_names <- sub("u.factor.", "", unord_factor_names)
    ord_factor_names <- factor_names[grep("o.factor", factor_names)]
    ord_factor_names <- sub("o.factor.", "", ord_factor_names)
    var_names_in_data <- base::intersect(names(data), c(
      unord_factor_names,
      ord_factor_names
    ))
    if (length(var_names_in_data) > 0) {
      for (i in seq_along(var_names_in_data)) {
        var_name <- var_names_in_data[i]

        # restore unordered factors
        if (var_name %in% unord_factor_names) {
          these_levels <- attributes(data)[[paste0("u.factor.", var_name)]]
          data[[var_name]] <- factor(data[[var_name]],
            levels = these_levels,
            labels = these_levels
          )

          # restore ordered factors
        } else {
          these_levels <- attributes(data)[[paste0("o.factor.", var_name)]]
          data[[var_name]] <- factor(data[[var_name]],
            ordered = TRUE,
            levels = these_levels,
            labels = these_levels
          )
        }
      }
    }
  }

  return(data)
}
