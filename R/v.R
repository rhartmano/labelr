#' Specify Column Names without Quoting Them
#'
#' @description
#' Alternative to the base `c()` combine operator that allows one to select
#' columns by passing unquoted comma-separated column names instead of quoted,
#' comma-separated column names.
#' @details
#' Does not support or combine with other subsetting operators, such as negative
#' indexing or colon: names must be full, individual column names, separated by
#' commas (see examples).
#' @param ... a vector of unquoted, comma-separated column names.
#' @return A character vector of quoted, comma-separated, column names.
#' @export
#' @examples
#' mt2a <- mtcars[c("am", "cyl", "mpg")]
#' mt2b <- mtcars[v(am, cyl, mpg)]
#' identical(mt2a, mt2b) # TRUE
#'
#' # silly demo
#' mtlabs <- mtcars
#' mtlabs <- add_val_labs(
#'   data = mtlabs,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = v(a, m) # equivalent to c("a", "m")
#' )
#'
#' get_val_labs(mtlabs)
v <- function(...) {
  x <- as.character((as.list(substitute(...()))))
  return(x)
}
