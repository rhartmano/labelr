#' Check Vector for "Irregular" Values
#'
#' @description
#' Check a vector for the presence of "irregular" values, defined as NA values,
#' other arbitrary values you specify, and (by default): NaN, Inf, -Inf, and
#' character variants of same (i.e., upper, lower, or mixed-case variants of
#' "NA","NAN","INF","-INF").
#'
#' @details
#' `check_irregular` is used by core labelr functions (e.g., `add_val_labs`) to
#' ensure that NA and other irregular (e.g., Inf) values are handled in a simple
#' and consistent -- and, hence, rigid -- fashion. It is not intended as a user-
#' facing command as part of a labelr data-analytic workflow, though it may be
#' useful in other applications where one wishes to test a vector against a
#' focal and user-extensible class of NA-esque (or other) offending values.
#'
#' @param x an atomic vector to checked for the presence of (any) NA values.
#' @param nan.include treat NaN values as NA (i.e., return TRUE if present).
#' @param inf.include treat Inf and -Inf values as NA (i.e., return TRUE if
#' present).
#' @param special a modifiable set of default character values that will be
#' treated as equivalent to NA values (i.e., will return TRUE if present).
#' @param other an argument for additional values of arbitrary but consistent
#' class (e.g., all numeric, all character) that will be treated as equivalent
#' to NA values (i.e., `check_irregular` will return TRUE where/if found).
#' @param any if TRUE, return a 1L vector that is TRUE if any irregular/NA-esque
#' value is found in the vector x, FALSE if no such value is found; if
#' any=FALSE, function will return a logical value for every element of x
#' (TRUE if that specific value meets the "irregular"-ity test).
#' @return A logical vector (1L if any==TRUE; length of x if any==FALSE).
#' @export
#' @examples
#' # below is FALSE, because there is nothing NA-like in this vector
#' check_irregular(1:10)
#'
#' # below is TRUE, because we're treating 99 as "NA-esque"
#' check_irregular(1:100, other = 99)
#'
#' # below is TRUE, because of NA val
#' check_irregular(c(1:100, NA))
#'
#' # below is TRUE, because nan.include is on (by default)
#' check_irregular(c(1:100, NaN), nan.include = TRUE)
#'
#' # below is TRUE, because inf.include is on (by default)
#' check_irregular(c(1:100, Inf), inf.include = TRUE)
#'
#' # below is TRUE, because inf.include is on (by default)
#' check_irregular(c(1:100, -Inf), inf.include = TRUE)
#'
#' # below is FALSE, it's just letters
#' check_irregular(letters)
#'
#' # below is TRUE - see default vals for arg special (function not case-sens)
#' check_irregular(c(letters, "NA"))
#'
#' # below is TRUE - see default vals for arg special (function not case-sens)
#' check_irregular(c(letters, "NAN"))
#'
#' # below is TRUE - see default vals for arg special (function not case-sens)
#' check_irregular(c(letters, "-iNf"))
#'
#' # below is FALSE, search for irregular vals is not substring/regex-based
#' check_irregular(c(letters, "nan-iNf"))
check_irregular <- function(x, nan.include = TRUE, inf.include = TRUE,
                            special = c("NA", "NAN", "INF", "-INF"), other = NULL,
                            any = FALSE) {
  # internal function to be sapply-ed
  check_irregular_1L <- function(x, nan.include = TRUE, inf.include = TRUE,
                                 special = c("NA", "NAN", "INF", "-INF"),
                                 other = NULL) {
    test_result <- any(is.na(x))
    if (nan.include) test_result <- any(is.nan(x), test_result)
    if (inf.include) test_result <- any(is.infinite(x), test_result)
    if (!is.null(special)) {
      x2 <- toupper(as.character(x))
      special <- toupper(as.character(special))
      test_result <- any(is.element(x2, special), test_result)
    }

    if (!is.null(other)) {
      if (is.character(other)) {
        x2 <- toupper(as.character(x))
        other <- toupper(as.character(other))
        test_result <- any(is.element(x2, other), test_result)
      } else {
        test_result <- any(is.element(x, other), test_result)
      }
    }
    return(test_result)
  }

  ### end of to-be-sapply-ed 1L version

  if (any) {
    test_result <- check_irregular_1L(x,
      nan.include = nan.include,
      inf.include = inf.include,
      special = special,
      other = other
    )
  } else {
    # vectorize
    test_result <- sapply(x, function(z) {
      check_irregular_1L(z,
        nan.include = nan.include,
        inf.include = inf.include,
        special = special,
        other = other
      )
    })
  }
  return(test_result)
}
