#' Is this an `add_quant_labs()`-style Value-labeled Variable (Column)?
#'
#' @description
#' Determine whether a specific variable of a data.frame has value labels
#' associated with it that were added using `add_quant_labs()` or `add_quant1()`.
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
  type <- "q"

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
  any_val <- check_labs_att(data, att)
  m1_val <- FALSE
  q_val <- FALSE

  if (any_val) {
    m1_val <- length(unique(att_list[att][[1]])) != length(att_list[att][[1]])

    q_unique_labs <- length(unique(unname(get_labs_att(
      data,
      paste0(
        "val.labs.",
        vars
      )
    )[[1]])))
    q_unique_vals <- unname(vapply(
      data[vars],
      function(x) length(unique(x)),
      integer(1)
    ))

    q_val <- q_unique_vals > q_unique_labs
  }

  out_val <- FALSE

  if (type == "any") {
    out_val <- any_val
  } else if (type == "m1" && any_val && !q_val) {
    out_val <- m1_val
  } else if (type == "q" && !m1_val && any_val) {
    out_val <- q_val
  } else if (type == "1to1" && !q_val && !m1_val) {
    out_val <- any_val
  }

  return(out_val)
}

#' @export
#' @rdname has_avl_labs
hql <- has_quant_labs
