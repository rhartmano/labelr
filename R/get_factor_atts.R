#' Get Factor Attributes from a Labeled Data Frame
#'
#' @description
#' `get_factor_atts` searches a labelr labelled data.frame for factors. If any
#' are found, a list of character vectors of factor levels is returned, with
#' each character vector being the set of unique levels for a factor variable,
#' and with each character vector named according to the convention "factor." +
#' variable name (e.g.,"factor.Species" for iris$Species). Used internally by
#' other labelr functions to get information about factors in labeled
#' data.frames.
#'
#' @param data the labelr labelled data.frame you are checking for factor
#' variables.
#' @return A list of character vectors, each named according to the convention
#' "factor." + variable name (e.g.,"factor.Species" for iris$Species). If the
#' data.frame lacks labelr attributes or lacks factors, an empty list will be
#' returned.
#' @export
#' @examples
#' ir2 <- iris
#' unique(ir2$Species)
#'
#' ir2 <- add_val_labs(ir2,
#'   vars = "Species", vals = c(
#'     "setosa",
#'     "versicolor",
#'     "virginica"
#'   ),
#'   labs = c("se", "ve", "vi")
#' )
#' get_val_labs(ir2)
#' head(use_val_labs(ir2))
#' get_factor_atts(iris) # no such info: iris is not labelr labelled
#' get_factor_atts(ir2) # this one has info: it's labelr labelled
get_factor_atts <- function(data) {
  atts_data <- attributes(data)
  all_names <- names(atts_data)
  labs_names <- names(atts_data)[sapply(names(atts_data), function(x) grepl("factor.", x))]
  labs_atts <- atts_data[labs_names]
  return(labs_atts)
}
