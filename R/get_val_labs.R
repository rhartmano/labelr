#' Return Lookup Table of Variable Values and Value Labels
#'
#' @description
#' For a variable-value-labeled data.frame, `get_val_labs` returns a derivative
#' data.frame that features the variable name, value, and associated value
#' labels for each value-labeled variable.
#'
#' Note: `gvl` is a compact alias for `get_val_labs`: they do the same thing,
#' and the former is easier to type
#'
#' @param data a data.frame.
#' @param var a character vector with the name(s) of any specific variable(s)
#' (If NULL, returned data.frame will contain all variable value labels).
#' @return A three-column data.frame, consisting of "var", "vals", and "labs"
#' columns, where each row corresponds to a unique value of a value-labeled
#' variable (column) from the user-supplied data.frame (or, for value-labeled
#' numerical variables, the upper bound of numerical values that fall within a
#' given value label). Note that all "vals" of "vals" column are typically
#' expressed as character even if the underlying values are, e.g., integer,
#' because the "vals" column typically includes values from multiple input
#' data.frame variables, which may be of various classes, such as integer,
#' character, factor. Accordingly, the `get_val_labs`-returned data.frame is
#' intended to facilitate --visual-- inspection of what (if any) value label
#' have been attached to each variable value; it is --not-- intended to be used
#' for accurate interrogation of the underlying classes or types of variables in
#' the primary data.frame that you passed to it. For that, use, e.g.,
#' sapply(data, class), sapply(use_val_labs(data), class), etc. on that primary
#' data.frame.
#' @export
#' @examples
#' # add val labs to multiple variables at once
#' # make a "Likert"-type fake data set to demo
#' # note, by default, add_val_labs() "vars" arg will do partial matching
#' # in this case, we catch all vars with "x" in their name
#' dflik <- make_likert_data(scale = 1:7, seed = 272)
#' vals2label <- 1:7
#' labs2use <- c(
#'   "VSD",
#'   "SD",
#'   "D",
#'   "N",
#'   "A",
#'   "SA",
#'   "VSA"
#' )
#'
#' dflik <- add_val_labs(
#'   data = dflik, vars = c("x", "y3"), # note the vars args
#'   vals = vals2label,
#'   labs = labs2use,
#'   partial = TRUE
#' )
#'
#' # note, all "x" vars get the labs, as does "y3"
#' get_val_labs(dflik)
#' get_val_labs(dflik, "x1")
get_val_labs <- function(data, var = NULL) {
  labs_att_check <- check_any_lab_atts(data, "val.labs")
  # get all val.labs values and labels
  get_val_labs_broad <- function(data) {
    val_atts_list <- get_all_lab_atts(data, "val.labs")
    labs_list <- vector(mode = "list", length = length(val_atts_list))
    for (i in seq_along(val_atts_list)) {
      ivar <- sub("val.labs.", "", names(val_atts_list)[[i]])
      ivals <- names(val_atts_list[[i]])
      ilabs <- unname(val_atts_list[[i]])
      idf <- data.frame(ivar, ivals, ilabs)
      labs_list[[i]] <- idf
    }

    labs_data <- do.call("rbind", labs_list)
    names(labs_data) <- c("var", "vals", "labs")
    return(labs_data)
  }

  if (labs_att_check) {
    # ensure value labels are sorted
    data <- sort_val_labs(data)

    labs_data <- get_val_labs_broad(data)
  } else {
    labs_data <- data.frame(var = NA, vals = NA, labs = NA)
  }

  if (any(names(data) %in% unique(labs_data[["var"]]))) {
    labs_less <- labs_data[labs_data$var %in% names(data), ]
  } else {
    labs_less <- data.frame(var = NA, vals = NA, labs = NA)
  }

  if (min(dim(labs_data)) == 0) {
    labs_data <- data.frame(var = NA, vals = NA, labs = NA)
  }

  if (!is.null(var)) {
    labs_data <- labs_data[labs_data$var %in% var, ]
  }

  if (all(is.na(unique(unlist(sapply(labs_data, unique)))))) {
    warning("\n \n  No val.labs found.")
  }

  return(labs_data)
}
