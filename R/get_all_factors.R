#' Put Data Frame Factor Level Information into a List
#'
#' @description
#' `get_all_factors` returns a list of character vectors, where each character
#' vector is a given factor variable's unique levels, and where the vector
#' is given the same name as the factor variable itself. If the data.frame
#' contains no factors, an empty (length 0) list is returned.
#'
#' @param data the data.frame you are checking for factor variables.
#' @return A list of 0, 1, or more character variables.
#' @export
#' @examples
#' class(get_all_factors(iris))
#' length(get_all_factors(iris))
#' zz <- iris
#' zz$u <- zz$Species # zz has two factor variables
#' class(get_all_factors(zz))
#' length(get_all_factors(zz))
#' get_all_factors(mtcars)
#' length(get_all_factors(mtcars))
get_all_factors <- function(data) {
  # get names and any factor levels from a variable
  get_factor <- function(x) {
    if (is.factor(x)) {
      levels_v <- levels(x)
      this_list <- list(levels_v)
    } else {
      this_list <- list(NULL)
    }
    return(this_list)
  }

  any_factor_check <- any(sapply(data, function(x) is.factor(x)))
  if (any_factor_check) {
    which_are_factors <- which(sapply(data, function(x) is.factor(x)))
    from_data_factor_info <- sapply(data[which_are_factors], get_factor)
  } else {
    from_data_factor_info <- vector(mode = "list")
  }

  return(from_data_factor_info)
}
