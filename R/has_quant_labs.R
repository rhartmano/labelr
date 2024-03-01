#' Does This Variable (Column) Have `add_quant_labs()`-style Numerical
#' Range-based Value Labels?
#'
#' @description
#' Determine whether a specific variable of a data.frame has numerical
#' range-based value labels associated with it (i.e., via `add_m1_lab()` or
#' `add1m1()`).
#'
#' @details
#' `hql` is a compact alias for `has_quant_labs`: they do the same thing, and
#' the former is easier to type
#'
#' @param data a data.frame.
#' @param var the unquoted name of the variable (column) to check for the
#' presence of `add_quant_labs()`-style numerical range-based value labels.
#' @return A 1L logical.
#' @export
#' @examples
#' # copy mtcars to mt2 and assign various types of value labels
#' mt2 <- mtcars
#'
#' # add 1-to-1 value labels
#' mt2 <- add_val_labs(
#'   data = mt2,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' has_val_labs(mt2, am) # TRUE, it does
#' has_m1_labs(mt2, am) # FALSE, they are NOT add_m1_lab()-style
#' has_quant_labs(mt2, am) # FALSE, they are NOT add_quant_labs() -style
#'
#' # add many-to-1 value labels
#' mt2 <- add_m1_lab(
#'   data = mt2,
#'   vars = "gear",
#'   vals = 4:5,
#'   lab = "4+"
#' )
#'
#' has_val_labs(mt2, gear) # TRUE, it does
#' has_m1_labs(mt2, gear) # TRUE, they ARE add_m1_lab()-style
#' has_quant_labs(mt2, gear) # FALSE, they NOT not add_quant_labs() -style
#'
#' # add quartile-based numerical range value labels
#' mt2 <- add_quant_labs(
#'   data = mt2,
#'   vars = "disp",
#'   qtiles = 4
#' )
#'
#' has_val_labs(mt2, disp) # TRUE, it does
#' has_m1_labs(mt2, disp) # FALSE, they are NOT add_m1_lab()-style
#' has_quant_labs(mt2, disp) # TRUE, they ARE add_quant_labs() -style
has_quant_labs <- function(data, var) {
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

  att <- paste0("val.labs.", vars)
  att_list <- get_all_lab_atts(data)
  val_labs_test_val <- check_labs_att(data, att)

  if (val_labs_test_val) {
    m1_test_val <- length(unique(att_list[att][[1]])) != length(att_list[att][[1]])
  } else {
    m1_test_val <- FALSE
  }

  if (val_labs_test_val && !m1_test_val) {
    unique_labs <- length(unique(unname(get_labs_att(
      data,
      paste0(
        "val.labs.",
        vars
      )
    )[[1]])))
    unique_vals <- unname(vapply(
      data[vars],
      function(x) length(unique(x)),
      integer(1)
    ))

    test_val <- unique_vals > unique_labs
  } else {
    test_val <- FALSE
  }

  return(test_val)
}


#' @export
#' @rdname has_quant_labs
hql <- has_quant_labs
