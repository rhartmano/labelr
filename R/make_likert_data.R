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
#' @param n number of observations (rows) of hypothetical data set to create.
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
#' set.seed(272)
#' dflik <- make_likert_data(scale = 1:7)
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
make_likert_data <- function(n = 1000,
                             scale = 1:7,
                             rownames = TRUE) {
  id <- 1:n
  data <- data.frame(id = id)

  for (i in seq_len(5)) {
    this_coln <- ncol(data) + 1
    x <- sample(c(scale), n, replace = TRUE)
    data <- cbind(data, x)
    names(data)[this_coln] <- paste0("x", i)
  }

  for (i in seq_len(5)) {
    this_coln <- ncol(data) + 1
    y <- sample(c(scale), n, replace = TRUE)
    data <- cbind(data, y)
    names(data)[this_coln] <- paste0("y", i)
  }

  if (rownames) {
    lett <- sample(LETTERS, size = length(id), replace = TRUE)
    rownames(data) <- paste0(lett, "-", id)
  }

  return(data)
}
