#' Drop a Single Variable's Value Labels
#'
#' @description
#' Drop all value labels previously applied to one or more variables using
#' `add_val_labs`, `add_quant_labs`,`add_m1_lab`, and related functions (e.g.,
#' `add_val1`) or aliases (e.g., `avl`).
#'
#' Note: `dvl1` is a compact alias for `drop_val1`: they do the same thing,
#' and the former is easier to type
#'
#' @details
#' Note: `drop_val1` is the `drop_val_labs` analogue to `add_val1`: just as
#' `add_val1` is a variant of `add_val_labs` that allows you to specify only one
#' variable at a time unquoted for value labeling, `drop_val1` allows you to
#' pass one unquoted variable name at a time for value dropping. See those
#' functions for further details regarding the conventions.
#'
#' @param data a data.frame.
#' @param var the unquoted name of the variable whose value labels will be
#' dropped.
#'
#' @return A data.frame, with all value labels dropped from specified variable.
#' @export
#' @examples
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
#' dflik <- add_val1(
#'   data = dflik, var = x3,
#'   vals = vals2label,
#'   labs = labs2use
#' )
#'
#' # see what this did
#' get_val_labs(dflik, "x3")
#'
#' dfdrop <- drop_val1(dflik,
#'   var = x3
#' ) # odd choice, but ok
#'
#' # var x3's value labels are gone, like we asked for
#' get_val_labs(dfdrop, "x3")
drop_val1 <- function(data, var) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  if (nrow(data) > 300000) {
    message("
\nNote: labelr is not optimized for data.frames this large.")
  }

  var <- deparse(substitute(var))
  test_quote <- any(grepl("\"", var))
  if (test_quote && is.character(var)) var <- gsub("\"", "", var)

  var_val_label <- paste0("val.labs.", var)
  any_val_labs <- any(grepl(var_val_label, names(get_all_lab_atts(data))))

  if (any_val_labs) {
    attributes(data)[[var_val_label]] <- NULL
    message(sprintf(
      "\n  Dropping all value labels from variable --%s--.\n", var
    ))
  } else {
    warning("\n \n  No value labels found.\n")
  }

  # update and resort attributes
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}

#' @export
#' @rdname drop_val1
dvl1 <- drop_val1
