#' Recode Values of a Free-standing Vector
#'
#' @description
#' Takes a stand-alone vector (x), and recodes select values (bef) to
#' some other set of values (aft), returning the recoded vector.
#' @details
#' While labelr users do not need to engage `recode_vals` directly, it is the
#' underlying function that powers the core labelr functions, such as
#' `add_val_labs` and `add_name_labs`. The bef argument identifies the values of
#' x to recode, and aft argument indicates what each bef value should be recoded
#' to (order matters: bef=c("a", "b", "c"), aft=c(1, 2, 3) means that "a" values
#' of x will be recoded to 1 values in returned vector, "b" values will be
#' recoded to 2, and "c" values will be recoded to 3).
#' @param x an integer, character, factor, or logical vector.
#' @param bef the "before" (i.e., current) values of x to be recoded.
#' @param aft the "after" (recoded) values to be substituted in the returned
#' vector in place of the positionally corresponding bef values of the x vector
#' ("positionally corresponding" means that the first element of aft is the
#' replacement (recode) for all x instances of the first element of bef, and so
#' on for the respective second bef and aft elements, etc.; see examples).
#' variables to which value labels will be added.
#' @param default.lab the "aft" value to be used for values of x for which no
#' "bef" value is specified. default.lab = "bef" (the default) will use
#' (retain) the existing value of x as its own recode, coercing to character
#' as needed. For example, if the value x=4 is observed in x but is not included
#' in the "bef" argument, the returned vector will have values of 4 (integer)
#' or "4" (character), depending on whether the recodes that --are-- supplied
#' are numeric (then 4) or character (then "4").
#' @param unique if TRUE, return only the mapping itself (bef argument values as
#' names, aft argument values as values), else if FALSE (default), return the
#' full recoded vector of x values.
#'
#' @return A vector of length equal length of supplied vector, with x values
#' found in bef argument switched to the corresponding values found in the aft
#' argument.
#' @export
#' @examples
#' z <- mtcars$gear
#' z[1] <- NA
#' z
#' recode_vals(z, c(5, 3, 4), c("five", "three", "four"))
#' irsp <- iris$Species[c(1:3, 60:62, 148:150)]
#' irsp
#' recode_vals(irsp, c("setosa", "versicolor", "virginica"), c("SE", "VE", "VI"))
#' class(irsp) # factor
#' class(recode_vals(
#'   irsp, c("setosa", "versicolor", "virginica"),
#'   c("SE", "VE", "VI")
#' )) # coerced to character
#' set.seed(112)
#' x_logic <- sample(c(TRUE, FALSE), 10, replace = TRUE)
#' x_logic
#' recode_vals(x_logic, bef = c(FALSE), c("Fake News!"))
recode_vals <- function(x, bef, aft, default.lab = "bef", unique = FALSE) {
  x <- as.character(x)
  xuni <- sort(unique(x))
  oldxuni <- xuni
  bef <- as.character(bef)
  aft <- as.character(aft)
  length_equal <- length(bef) == length(aft)
  if (!length_equal) stop("bef and aft must be same length")
  for (i in seq_along(bef)) {
    xuni[oldxuni == bef[i]] <- aft[i]
  }

  names(xuni) <- oldxuni

  if (default.lab != "bef") {
    xuni[!xuni %in% aft] <- default.lab
  }

  if (!unique) {
    x <- xuni[x]
    na_start <- sum(is.na(x))
    na_end <- sum(is.na(suppressWarnings(as.numeric(as.character(x)))))
    numable <- na_end == na_start
    if (numable) x <- as.numeric(x)
    x <- unname(x)
  } else {
    x <- xuni
  }
  return(x)
}
