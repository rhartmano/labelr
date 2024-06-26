#' Determine Which Elements of a Character Vector Match at Least One Pattern
#' Contained in Any of the Elements of Another Character Vector
#'
#' @description
#' `gremlr` accepts two character vectors of strings and, for each element of
#' the second vector, determines whether that element matches any of the
#' patterns supplied via any of the elements of the first vector.
#'
#' @details
#' This function accepts a character vector of text substring patterns or
#' regular expressions (patterns argument), and searches another character
#' vector (x argument) to determine for each element of x, whether that element
#' is a match (in the sense of `base::grepl()`) for any pattern element of
#' of patterns. If vals = TRUE (default is FALSE), each matched x element is
#' returned; if vals = FALSE, a vector of named logical values equal in length
#' to x is returned, indicating for each x element whether it contains any text
#' substring or pattern found in any element of patterns (TRUE if so, FALSE if
#' not), with the names corresponding to the elements of the x vector. Used
#' internally by various labelr functions. If ignore.case = TRUE (the default),
#' neither vector is treated case-sensitively (both are coerced to lower-case
#' before other operations).
#'
#' Used internally by various labelr functions  Note that this is the same
#' search and syntax that is performed by `greml`(`gremlr` is "`greml` in
#' reverse"), except that, whereas `greml` returns matches in terms of the
#' patterns argument, `gremlr` returns matches in terms of x argument.
#'
#' @param patterns a character vector of comma-separated, quoted character
#' strings or regular expressions.
#' @param x a character vector that will be tested for presence/absence of
#' the patterns passed via the patterns argument.
#' @param ignore.case search in a non-case-sensitive fashion if TRUE.
#' @param vals by default, vals = FALSE and will return a named vector that
#' indicates, for each unique element of x, whether that x element was a match
#' for any element of patterns (one named logical element returned for each x
#' vector element). If TRUE, vals returns the unique values of x that were
#' matched (one character element for each matched x argument vector
#' element).
#' @return a vector, either character (if vals = TRUE) or logical (if vals =
#' FALSE).
#' @export
#' @examples
#' # search for "AB" (case-sensitively) anywhere in subsequent vector
#' gremlr(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = FALSE)
#' # character(0)
#'
#' # search for "AB" (non-case-sensitively; the default) anywhere in next vector
#' gremlr(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = TRUE)
#' # [1] "ab"  "ab"  "abc"
#'
#' # other searches
#' gremlr(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = FALSE)
#' gremlr(c("ab"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' gremlr(c("ab", "Q"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' gremlr(c("a|b", "Q"), c("a", "b", "abc", "z"), vals = FALSE)
#' gremlr(c("a|b", "Q"), c("a", "b", "z"), vals = FALSE)
#' gremlr(c("a|b", "Q"), c("bq", "z"), vals = FALSE)
#'
#' # compare gremlr (above) to greml() (here)
#' greml(c("AB"), c("ab", "ab", "abc", "z"), vals = TRUE, ignore.case = FALSE)
#' greml(c("ab"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' greml(c("ab", "Q"), c("ab", "ab", "abc", "z"), vals = FALSE)
#' greml(c("a|b", "Q"), c("a", "b", "abc", "z"), vals = FALSE)
#' greml(c("a|b", "Q"), c("a", "b", "z"), vals = FALSE)
#' greml(c("a|b", "Q"), c("bq", "z"), vals = FALSE)
gremlr <- function(patterns, x, ignore.case = TRUE, vals = FALSE) {
  log_mat <- sapply(
    patterns,
    function(z) grepl(z, x, ignore.case = ignore.case)
  )

  log_mat <- as.matrix(log_mat)

  xmat <- apply(log_mat, 1, any)

  names(xmat) <- x

  if (vals) xmat <- x[which(xmat)]

  return(xmat)
}
