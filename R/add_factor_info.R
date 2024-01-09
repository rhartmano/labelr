#' Add Factor-specific Attributes to a Data Frame
#'
#' @description
#' `add_factor_info` searches a data.frame for labelr-specific factor meta-data,
#' which it records and retains for future use. It is used by other labelr
#' functions and need not be used directly by labelr end users.
#'
#' @param data the data.frame to which you wish to add labelr-specific factor
#' variable meta-data attributes (if any factors are present).
#' @return a data.frame.
#' @export
#' @examples
#' # this function does not strictly require prior or other use of labelr
#' ir2 <- add_factor_info(iris)
#' mt2 <- add_factor_info(mtcars)
#' get_factor_info(mtcars) # none
#' get_factor_info(iris) # none
#' get_factor_info(mt2) # none
#' get_factor_info(ir2) # some!
add_factor_info <- function(data) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  factor_info <- get_all_factors(data)
  for (i in seq_along(factor_info)) {
    this_name <- names(factor_info[i])
    this_factor_info <- unlist(unname(factor_info[i]))
    if (is.ordered(data[[this_name]])) {
      attributes(data)[[paste0("o.factor.", this_name)]] <- this_factor_info
    } else {
      attributes(data)[[paste0("u.factor.", this_name)]] <- this_factor_info
    }
  }
  return(data)
}
