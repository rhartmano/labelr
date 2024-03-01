#' Return Look-up Table of One Variable's Value Labels
#'
#' @description
#' For a data.frame with value-labeled variables, `get_val_lab1` returns a
#' derivative data.frame or vector that shows the value-to-label mapping for
#' each unique value of that value-labeled variable.
#'
#' @details
#' `get_val1` is a variant of `get_val_labs` that allows you to specify
#' only one var whose value-to-label mapping you wish to look up.
#'
#' Note 1: As with `get_val_labs()`, `get_val_lab1()` exists to provide a visual,
#' human-interpretable quick look at how value labels map to underlying values
#' and is NOT intended for use in automated querying, subsetting, or other
#' manipulation of those value labels. Further: Unlike `get_val_labs()`, which
#' may return value-to-label mappings for --several-- variables of potentially
#' different atomic types, `get_val_lab1()` limits itself to returning the value
#' labels of a --single-- variable (column) of the supplied data.frame.
#'
#' For this reason, and in contrast to the behavior of `get_val_labs()`, if
#' `get_val_lab1()`'s simplify argument is set to FALSE (the default), the
#' returned data.frame will express var values as numeric if this can be done
#' without creating new NA values (i.e., in the sense of `as_numv()`). In
#' contrast, if simplify is TRUE, the look-up table information will be returned
#' as a named character vector.
#'
#' Note 2: `gvl1` is a compact alias for `get_val_lab1`: they do the same thing,
#' and the former is easier to type
#'
#' @param data a data.frame.
#' @param var the unquoted name of the variable (column) for which a value-to-label
#' look-up mapping is sought.
#' @param simplify return the mapping as a named vector, not a data.frame
#' (defaults to FALSE).
#' @return By default, a three-column data.frame, consisting of "var", "vals",
#' and "labs" columns, where each row corresponds to a unique value of var OR --
#' for variables labeled using `add_quant_labs` (or `add_quant1`) -- the
#' approximate (i.e., possibly rounded) upper bound of numerical values that
#' fall within that label's range of coverage.  If simplify is FALSE, a character
#' vector will returned.
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
#' get_val_lab1(dflik, x1)
#'
#' get_val_lab1(dflik, x1, simplify = TRUE)
get_val_lab1 <- function(data, var, simplify = FALSE) {
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

  # capture var argument
  vars <- deparse(substitute(var))
  test_quote <- any(grepl("\"", vars))
  if (test_quote && is.character(vars)) vars <- gsub("\"", "", vars)
  vars <- gsub("c\\(", "", vars)
  vars <- gsub("\\(", "", vars)
  vars <- gsub("\\)", "", vars)

  # test for presence of var in data.frame
  if (!all(vars %in% names(data)) || length(vars) != 1) {
    stop("
\nInvalid var argument specification: var arg should be a single, unquoted
name of a variable that is present in the data.frame.
         ")
  }

  # verify presence of value labels
  if (check_labs_att(data, paste0("val.labs.", vars))) {
    # subset data frame
    data <- sbrac(data, 1, vars)

    # ensure value labels are sorted
    data <- sort_val_labs(data)

    # recover the value labels as a data.frame
    labs_data <- get_val_labs_broad(data)

    # optionally simplify to vector
    if (simplify) {
      vals_as_names <- labs_data[["vals"]]
      labs_data <- labs_data[["labs"]]
      names(labs_data) <- vals_as_names

      # if returning data.frame where values column could be numeric, express it
      # ...as numeric
    } else {
      labs_data[["vals"]] <- as_numv(labs_data[["vals"]])
    }
  } else {
    labs_data <- data.frame(var = NA, vals = NA, labs = NA)
    warning("\n \n  No val.labs found.")
  }

  return(labs_data)
}

#' @export
#' @rdname get_val_lab1
gvl1 <- get_val_lab1
