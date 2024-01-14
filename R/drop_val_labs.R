#' Drop Value Labels from One or More Variables
#'
#' @description
#' Drop all value labels previously applied to one or more variables using
#' `add_val_labs`, `add_quant_labs`,`add_m1_lab`, and related functions (e.g.,
#' `add_val1`) or aliases (e.g., `avl`).
#'
#' Note: `dvl` is a compact alias for `drop_val_labs`: they do the same thing,
#' and the former is easier to type
#'
#' @details
#' `drop_val_labs` works with other labelr functions (e.g., `add_val_labs`,
#' `get_val_labs`, `use_val_labs`, `add_lab_cols`) to facilitate the creation,
#' accessing, modification, use, or deletion of variable value labels, each of
#' which is uniquely associated with a specific distinct value of a specific
#' variable (e.g., "Manual Transmission" might be the value label for the
#' distinct value mtcars$am==1).
#'
#' @param data a data.frame.
#' @param vars a character vector that corresponds to the name(s) (or substring
#' within the name(s), if partial = TRUE) of one or more variables form which
#' value labels will be removed. If NULL, all value labels will be removed from
#' all value-labeled variables.
#' @param partial To drop labels for many, similarly named variables (e.g., "x1"
#' through "x20"), you can provide a substring only and set partial = TRUE
#' (default is FALSE). For example, to drop value labels for colnames "x1"
#' through "x20",  you could use vars = c("x"), along with partial = TRUE. Be
#' careful with this, as it also will attempt to drop value labels for columns
#' with colnames "sex" or "tax.bracket" (etc.), because they, too, contain an "x"
#' in their names).
#' @param not.vars use of the partial argument can result in situations where
#' you inadvertently attempt to drop value labels for a variable. For example,
#' if vars="x", and partial=TRUE, then `drop_val_labs` will attempt to drop
#' labels for not only "x1", "x2","x3", and "x4", but also for "sex",
#' "tax.bracket", and other "x"-containing variable names. Use of not.vars
#' allows you to indicate variables that you wish to exempt from value label-
#' dropping, even if their names contain the string found in vars. Note that
#' not.vars gets priority: setting vars="x", partial=TRUE, and not.vars="x" is
#' tantamount to telling `drop_val_labs` that you actually do not wish to drop
#' value labels for any of the variables that you specified in vars, resulting
#' in no value labels being dropped.
#'
#' @return A data.frame, with all value labels dropped from specified variables.
#' @export
#' @examples
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
#' dfdrop <- drop_val_labs(dflik,
#'   vars = c("x2", "y3"),
#'   partial = FALSE
#' )
#'
#' # var x2's value labels are gone, like we asked for
#' get_val_labs(dfdrop, "x2")
#'
#' # var x1's value labels are intact, b/c we didn't ask to drop them
#' get_val_labs(dfdrop, "x1")
#'
#' dfxgone <- drop_val_labs(dflik,
#'   c("x"),
#'   partial = TRUE
#' )
#'
#' # still a lot of value labels, but all are for "y" vars,
#' # ...none is left for "x" vars
#' get_val_labs(dfxgone)
drop_val_labs <- function(data, vars = NULL, partial = FALSE,
                          not.vars = NULL) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  if (nrow(data) > 300000) {
    message("
\nNote: labelr is not optimized for data.frames this large.")
  }

  # capture variable names substrings
  if (is.null(vars)) vars <- names(data)

  # capture variable names substrings
  if (partial) {
    vars <- gremlr(vars, names(data), vals = TRUE)

    message(
      "\nRemember: This command uses partial var arg matching.
Use specific var argument or drop_val1() to restrict this operation to fewer/more specific variables.\n"
    )
  }

  # drop any vars in not.vars
  if (!is.null(not.vars)) {
    if (partial) not.vars <- gremlr(not.vars, names(data), vals = TRUE)
    vars <- base::setdiff(vars, not.vars)
  }

  vars <- unique(vars)

  # begin main loop
  for (i in seq_along((vars))) {
    var <- vars[i]

    # get var
    if (!var %in% names(data)) {
      stop(sprintf(
        "\n variable --%s-- not found in your data.frame\n", var
      ))
    }

    var_val_label <- paste0("val.labs.", var)
    any_val_labs <- any(grepl(var_val_label, names(get_all_lab_atts(data))))

    if (any_val_labs) {
      attributes(data)[[var_val_label]] <- NULL

      message(sprintf(
        "\n  Dropping all value labels from variable --%s--.\n", var
      ))
    } else {
      message(sprintf(
        "\n  No value labels found for variable --%s--.\n", var
      ))
    }
  }

  # update and resort attributes
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}

#' @export
#' @rdname drop_val_labs
dvl <- drop_val_labs
