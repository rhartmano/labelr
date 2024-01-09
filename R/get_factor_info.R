#' Return Factor Attributes as a Data Frame
#'
#' @description
#' `get_factor_info` searches a labelr labeled data.frame for factors. If any
#' are found, a data.frame is returned with the name and unique factor levels of
#' each, along with a logical indicator of whether the factor is ordered, with
#' one row per level per factor. If none are found, a one-row data.frame of NA
#' values is returned.
#'
#' @param data the labelr labeled data.frame you are checking for factor
#' variables.
#' @param var a 1L character vector with the name of a specific variable (column)
#' of data, if you wish to restrict query to a single variable (else, keep NULL
#' and will get info for any relevant variables).
#'
#' @return A data.frame with three columns: "factor.var" (the name of the factor
#' variable in question), "levels" (the given level of that factor, expressed as
#' a character), and "ordered" (TRUE if so, FALSE if not). If no factors are
#' present in the supplied data.frame, a one-row data.frame of same structure
#' with all three cells set to NA.
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
#' get_factor_info(iris) # no such info: iris is not labelr labeled
#' get_factor_info(ir2) # this one has info: it's labelr labeled
get_factor_info <- function(data, var = NULL) {
  labs_att_check <- check_any_lab_atts(data, "factor.")
  # get all factor. values and levels
  get_factor_info_broad <- function(data) {
    val_atts_list <- get_all_lab_atts(data, "factor.")
    lvls_list <- vector(mode = "list", length = length(val_atts_list))
    for (i in seq_along(val_atts_list)) {
      ivar <- sub("factor.", "", names(val_atts_list)[[i]])
      ivals <- unname(val_atts_list[[i]])
      idf <- data.frame(ivar, ivals)
      lvls_list[[i]] <- idf
    }

    factor_info_data <- do.call("rbind", lvls_list)
    names(factor_info_data) <- c("factor.var", "levels")
    ordered_info <- grepl("^o\\.", factor_info_data[["factor.var"]])
    factor_info_data <- cbind(factor_info_data, ordered_info)
    names(factor_info_data) <- c("factor.var", "levels", "ordered")
    factor_info_data[["factor.var"]] <- gsub(
      "^o\\.|^u\\.", "",
      factor_info_data[["factor.var"]]
    )
    return(factor_info_data)
  }

  if (labs_att_check) {
    factor_info_data <- get_factor_info_broad(data)
  } else {
    factor_info_data <- data.frame(factor.var = NA, levels = NA, ordered = NA)
  }

  if (any(names(data) %in% unique(factor_info_data[["factor.var"]]))) {
    lvls_less <- factor_info_data[factor_info_data$factor.var %in% names(data), ]
  } else {
    lvls_less <- data.frame(factor.var = NA, levels = NA, ordered = NA)
  }

  if (min(dim(factor_info_data)) == 0) {
    factor_info_data <- data.frame(factor.var = NA, levels = NA, ordered = NA)
  }

  if (!is.null(var)) {
    factor_info_data <- factor_info_data[factor_info_data$factor.var == var, ]
  }

  if (all(is.na(unique(unlist(sapply(factor_info_data, unique)))))) {
    warning("\n \n  No factor information. found.")
  }

  return(factor_info_data)
}
