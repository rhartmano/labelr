#' Determine Which Pattern Elements of One Character Vector Are Found in at
#' Least One Element of A Second Character Vector
#'
#' @description
#' `greml` takes two character vectors of strings and, for each pattern
#' represented by an element of the first vector, searches all elements of the
#' second vector to see if any of those elements of the second vector matches
#' that pattern element of the first vector.
#'
#' @details
#' This function accepts a character vector of text substring patterns or
#' regular expressions (patterns argument), and searches a second character
#' vector (x argument) to determine which elements of the first vector
#' (patterns) match at least one element anywhere in the second vector (x). If
#' vals = TRUE (default is FALSE), each matched pattern element is returned; if
#' vals = FALSE, a vector of named logical values equal in length to the
#' object supplied to the patterns argument is returned, indicating for each
#' patterns element whether a match for it was found anywhere in x (TRUE if so,
#' FALSE if not), with the names corresponding to the elements of the patterns
#' vector. If ignore.case = TRUE (the default), neither vector is treated case-
#' sensitively (both are coerced to lower-case before other operations occur).
#'
#' Used internally by various labelr functions (e.g., `use_val_labs`). Note that
#' this is the same search and syntax that is performed by `gremlr()`
#' (`gremlr` means "`greml` in reverse"), except that, whereas `greml` returns
#' matches in terms of the patterns argument (i.e., which patterns elements
#' match at least one x element), `gremlr` returns matches in terms of the x
#' argument.
#'
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
#' greml(c("a|b", "Q"), c("bq", "z"), vals = TRUE)
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

  x <- apply(as.matrix(log_mat), 2, any)
  if (vals) x <- names(x)[which(x)]
  return(x)
}
