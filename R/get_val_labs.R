#' Return Look-up Table of Variable Values and Value Labels
#'
#' @description
#' For a data.frame with value-labeled variables, `get_val_labs` returns a
#' derivative data.frame that shows the value-to-label mapping for each unique
#' value of each value-labeled variable.
#'
#' @details
#' Note 1: `get_val_labs` returns a data.frame that is intended strictly to
#' facilitate human-in-the-loop --visual-- display and inspection of what (if
#' any) value label has been associated with each variable value. It is --not--
#' intended for use in automated querying or subsetting or as an indicator of
#' of the supplied data.frame's columns' underlying classes or atomic types. In
#' particular, all columns of the --returned-- data.frame object are coerced to
#' character for display purposes, as a result of concatenating value
#' information from different variables of potentially different atomic types or
#' classes. For example, all elements of the "vals" column are expressed as
#' character even if the underlying values themselves are numeric.
#'
#' Note 2: `gvl` is a compact alias for `get_val_labs`: they do the same thing,
#' and the former is easier to type
#'
#' @param data a data.frame.
#' @param var a character vector with the name(s) of any specific variable(s)
#' (If NULL, returned data.frame will contain all variable value labels).
#' @return A three-column data.frame, consisting of "var", "vals", and "labs"
#' columns, where each row corresponds to a unique value of a value-labeled
#' variable (column) from the user-supplied data.frame OR -- for variables
#' labeled using `add_quant_labs` (or `add_quant1`) -- the upper bound of
#' numerical values that fall within that label's range of coverage. Note that
#' all variables of the returned data.frame are coerced to character (see Note 1
#' of details).
#' @export
#' @examples
#' # add val labs to multiple variables at once
#' # make a "Likert"-type fake data set to demo
#' # note, by default, add_val_labs() "vars" arg will do partial matching
#' # in this case, we catch all vars with "x" in their name
#' set.seed(272)
#' dflik <- make_likert_data(scale = 1:7)
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

#' @export
#' @rdname get_val_labs
gvl <- get_val_labs
