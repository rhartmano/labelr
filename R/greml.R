#' Check Whether Each Character in a Vector Appears in Another Vector
#'
#' @description
#' `greml` accepts two character vectors of strings and searches every element
#' of the second one for the presence of any element of the first one. Every
#' element of the first vector that matches any element of the second vector is
#' considered a match.
#'
#' @details
#' This function accepts a character vector of text substring patterns or
#' regular expressions (patterns), and searches another character vector (x) to
#' determine for each pattern (text substring) whether that pattern appears
#' anywhere in x. If vals = TRUE (default is FALSE), each matched pattern
#' element is returned; if vals = FALSE, a vector of named logical values equal
#' in length to patterns is returned, indicating for each patterns element
#' whether a match for it was found anywhere in x (TRUE if so, FALSE if not),
#' with the names corresponding to the elements of the patterns vector.
#' If ignore.case = TRUE (the default), neither vector is treated case-
#' sensitively (both are coerced to lower-case before other operations). Used
#' internally by various labelr functions (e.g., `use_val_labs`). Note that this
#' is the same search and syntax that is performed by `gremlr`
#' ("`greml` in reverse"), except that, whereas `greml` returns matches in terms
#' of patterns argument, `gremlr` returns matches in terms of x argument.
#' @param patterns a character vector of comma-separated, quoted character
#' strings or regular expressions.
#' @param x a character vector that will be tested for presence/absence of
#' the patterns passed via the patterns argument.
#' @param ignore.case search in a non-case-sensitive fashion if TRUE.
#' @param vals by default, vals = FALSE and will return a named vector that
#' indicates, for each unique element of patterns, whether a match for that
#' pattern was found somewhere in the x vector (one named logical element
#' returned for each patterns argument vector element). If TRUE, vals returns the
#' unique values of patterns that were matched (one character element for each
#' matched patterns argument vector element).
#' @return a vector, either character (if vals = TRUE) or logical (if vals =
#' FALSE).
#' @export
#' @examples
#' # search for "AB" (case-sensitively) anywhere in subsequent vector
#' greml(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = FALSE)
#' # character(0)
#'
#' # search for "AB" (non-case-sensitively; the default) anywhere in next vector
#' greml(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = TRUE)
#' # [1] "AB"
#'
#' # other searches
#' greml(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = FALSE)
#' greml(c("ab"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' greml(c("ab", "Q"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' greml(c("a|b", "Q"), c("a", "b", "abc", "z"), vals = FALSE)
#' greml(c("a|b", "Q"), c("a", "b", "z"), vals = FALSE)
#' greml(c("a|b", "Q"), c("bq", "z"), vals = FALSE)
#'
#' # compare greml (above) to gremlr() (here)
#' gremlr(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = FALSE)
#' gremlr(c("ab"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' gremlr(c("ab", "Q"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' gremlr(c("a|b", "Q"), c("a", "b", "abc", "z"), vals = FALSE)
#' gremlr(c("a|b", "Q"), c("a", "b", "z"), vals = FALSE)
#' gremlr(c("a|b", "Q"), c("bq", "z"), vals = FALSE)
greml <- function(patterns, x, ignore.case = TRUE, vals = FALSE) {
  log_mat <- sapply(
    patterns,
    function(z) grepl(z, x, ignore.case = ignore.case)
  )

  x <- apply(log_mat, 2, any)
  if (vals) x <- names(x)[which(x)]
  return(x)
}
