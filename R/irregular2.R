#' Convert All "Irregular" Data Frame Values to NA or Other Specified Value
#'
#' @description
#' Check all (or specified) columns of a data.frame for the presence of
#' "irregular" values (e.g., NA, Inf, NaN) and, if found, replace them with NA
#' (or some other specified value).
#'
#' @details
#' For purposes of `irregular2`, irregular values consist of: NA values, other
#' arbitrary values you specify, and (by default): NaN, Inf, -Inf, and character
#' variants of same (i.e., upper, lower, or mixed-case variants of "NA","NAN",
#' "INF","-INF"). This function converts all such values to NA (or some other
#' specified value).
#'
#' @param data a data.frame object.
#' @param vars a character vector that corresponds to the name(s) of one or more
#' variables (columns) to which the operational will be applied. If NULL, will
#' be applied to all variables.
#' @param to a single value (by default: NA) to which all irregular values will
#' be converted (Note: if arg is character, returned vector will be coerced to
#' character).
#' @param nan.include convert NaN values to NA.
#' @param inf.include convert Inf and -Inf values to NA.
#' @param special additional specific character values that will be
#' converted to NA values.
#' @return a data.frame identical to data, with exception that irregular values
#' have been converted to the value specified in "to" argument (NA, by default).
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- c(NA, Inf, -Inf, NaN, runif(6))
#' y <- c("a", "inf", "NaN", NA, sample(letters, 6, replace = TRUE))
#' df <- data.frame(x, y)
#'
#' head(df, 10)
#'
#' df_1 <- irregular2(df)
#'
#' head(df_1, 10)
#'
#' df_2 <- irregular2(df, vars = "x", to = ";-)")
#'
#' head(df_2)
irregular2 <- function(data, vars = NULL, to = NA, nan.include = TRUE,
                       inf.include = TRUE,
                       special = c("NA", "NAN", "INF", "-INF")) {
  if (is.null(vars)) vars <- names(data)

  data[vars] <- lapply(
    data[vars],
    \(x) irregular2v(x,
      to = to,
      nan.include = nan.include,
      inf.include = inf.include,
      special = special,
      other = NULL
    )
  )

  return(data)
}
