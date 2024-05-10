#' Safely Sort (Re-order) a Labeled Data Frame
#'
#' @description
#' `ssort` allows one to sort (after the fashion of base::order or
#' dplyr::arrange) the rows of a data.frame based on column values.
#'
#' @details
#' This function accepts a data.frame, followed by a quoted vector of column
#' names (or an integer vector of column position indices), followed by
#' an indication of which are to be sorted ascending (default) or descending.
#' If multiple columns are supplied to vars, sorting prioritizes the columns
#' that appear earlier, with values of subsequent columns being sorted within
#' distinct values of earlier columns. Note: `ssort` is fast enough on small
#' data.frames and very slow on "larger" (>500K records) data.frames,
#' particularly for more complex or demanding sort requests. Other R packages
#' may provide faster sorting while preserving labelr attributes.
#'
#' @param data the data.frame to be sorted.
#' @param vars the variables to be sorted on, specified as a quoted character
#' vector of variable names or an integer vector of column position indices.
#' @param descending whether to sort the given variable of vars in descending
#' or ascending order. Default is FALSE, which will be recycled to all vars
#' arguments.
#' @param na.last force NA values to appear last in a variable's sort order if
#' TRUE (default).
#' @param fact.to.char coerce all factor variables to character variables.
#' @return a labelr label attribute-preserving data.frame consisting of the
#' re-sorted data.frame.
#'
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000) # another labelr:: function
#'
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' head(df, 3)
#' check_labs_att(df, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsort1 <- ssort(df, c("raceth", "gender", "age"), descending = c(TRUE, FALSE, FALSE))
#'
#' head(dfsort1, 20)
#'
#' check_labs_att(dfsort1, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsort2 <- ssort(df, c("age", "gender"))
#'
#' head(dfsort2, 20)
#'
#' check_labs_att(dfsort2, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsort3 <- ssort(df, c("raceth"))
#'
#' head(dfsort3, 10)
#'
#' check_labs_att(dfsort3, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
ssort <- function(data, vars, descending = FALSE, na.last = TRUE,
                  fact.to.char = TRUE) {
  # recycle descending arg
  if (length(descending) == 1 && length(vars) > 1) {
    descending <- rep(descending, length(vars))
  } else if (length(descending) > 1 && length(vars) != length(descending)) {
    stop("Number of args to vars and descending do not match.")
  }

  # handle var column number indices, if supplied
  if (is.numeric(vars)) vars <- names(data)[vars]

  # capture labelr attributes, so, they can be re-associated at the end
  these_atts <- get_all_lab_atts(data)

  # coerce/create data.frames
  data <- as_base_data_frame(data)

  # optionally convert factor to character
  if (fact.to.char) {
    data <- as.data.frame(data)
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)
  }

  # order
  if (length(vars) == 1) {
    sort.ord2 <- order(data[, vars], decreasing = descending, na.last = na.last)
    data <- data[sort.ord2, ]
  } else {
    data <- data[do.call("order", c(data[vars], list(decreasing = descending, method = "radix", na.last = na.last))), ]
  }

  # re-associate label information
  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
