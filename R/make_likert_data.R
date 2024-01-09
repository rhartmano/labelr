#' Construct a Fake Likert Survey Response Data Frame
#'
#' @description
#' `make_likert_data` generates a data.frame with select (entirely fictional)
#' numerically coded responses of fictional people to fictional survey items to
#' demonstrate and explore labelr functionalities.
#'
#' @details
#' Data is entirely fictional and strictly for purposes of demonstrating labelr.
#'
#' @param rows number of observations (`nrow`) to create.
#' @param seed random number seed to pass to `set.seed` for reproducibility.
#' @param scale the sequence of distinct integer values describing the raw /
#' naive numerical codings of Likert-type survey items.
#' @param rownames create memorable but arbitrary rownames for inspection (if
#' TRUE).
#'
#' @return a data.frame.
#' @export
#' @examples
#' # add_val_labs() "vars" arg will do partial matching if partial = TRUE
#' # in this case, we catch all vars with "x" in their name
#' dflik <- make_likert_data(scale = 1:7, seed = 272)
#' vals2label <- 1:7
#' labs2use <- c(
#'   "VSD",
#'   "SD",
#'   "D",
#'   "N",
#'   "A",
#'   "SA",
#'   "VSA"
#' )
#'
#' dflik <- add_val_labs(
#'   data = dflik, vars = c("x", "y3"), # note the vars args
#'   vals = vals2label,
#'   labs = labs2use,
#'   partial = TRUE
#' )
#'
#' # note, all "x" vars get the labs, as does "y3"
#' # see vars = args above
#' lik1 <- use_val_labs(dflik)
#' head(lik1)
make_likert_data <- function(rows = 1000,
                             seed = 123,
                             scale = 1:7,
                             rownames = TRUE) {
  set.seed(seed)
  id <- 1:rows
  data <- data.frame(id = id)

  for (i in seq_len(5)) {
    this_coln <- ncol(data) + 1
    x <- sample(c(scale), rows, replace = TRUE)
    data <- cbind(data, x)
    names(data)[this_coln] <- paste0("x", i)
  }

  for (i in seq_len(5)) {
    this_coln <- ncol(data) + 1
    y <- sample(c(scale), rows, replace = TRUE)
    data <- cbind(data, y)
    names(data)[this_coln] <- paste0("y", i)
  }

  if (rownames) {
    lett <- sample(LETTERS, size = length(id), replace = TRUE)
    rownames(data) <- paste0(lett, "-", id)
  }

  return(data)
}
