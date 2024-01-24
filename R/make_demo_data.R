#' Construct a Fake Demographic Data Frame
#'
#' @description
#' `make_demo_data` generates a data.frame with select (entirely fictional)
#' "demographic" variables purely for the purposes of demonstrating or exploring
#' common labelr behaviors and uses and is not designed to accurately emulate or
#' represent the frequencies or relationships among demographic variables.
#'
#' @param n number of observations (rows) of hypothetical data set to create.
#' @param age.mean mean value of (fictional) age variable (assuming a normal
#' distribution) recorded in a hypothetical data set.
#' @param age.sd standard deviation of (fictional) age variable (assuming a normal
#' distribution) recorded in a hypothetical data set.
#' @param gend.prob probabilities of three gender categories for a gender identity
#' variable recorded in a hypothetical data set.
#' @param raceth.prob probabilities of categories of a hypothetical race/ethnicity
#' variable recorded in a hypothetical data set.
#' @param edu.prob probabilities of categories of a hypothetical "highest level of
#' education" variable recorded in a hypothetical data set.
#' @param rownames create memorable but arbitrary rownames for inspection (if
#' TRUE).
#'
#' @return a data.frame.
#' @importFrom stats rnorm runif
#' @export
#'
#' @examples
#' # make toy demographic (gender, race, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000)
#'
#' # val labels are "variable value labels"
#' # each value label can apply to only one distinct value
#' # not suitable for numerics with decimals
#' # max.unique.vals dictates how many unique values a variable can have to be
#' # considered value "label-able"
#'
#' # NOTE: NA values (and Inf, -Inf, and NaN) are automatically given label "NA",
#' # ...and any subsequent use_val_labs() coercion will convert all such values to NA
#' # You cannot modify this behavior -- labelr is rigid about NA and other "irregular"
#' # ...(i.e., NaN, Inf) values
#' #'
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#' head(df)
#' summary(df)
make_demo_data <- function(n = 1000,
                           age.mean = 43,
                           age.sd = 15,
                           gend.prob = c(0.475, 0.475, 0.05),
                           raceth.prob = c(
                             1 / 7, 1 / 7, 1 / 7, 1 / 7, 1 / 7,
                             1 / 7, 1 / 7
                           ),
                           edu.prob = c(0.03, 0.32, 0.29, 0.24, 0.12),
                           rownames = TRUE) {
  raceth <- sample(c(1, 2, 3, 4, 5, 6, 7), n, replace = TRUE, prob = raceth.prob)
  gender <- sample(c(0, 1, 2), n, replace = TRUE, prob = gend.prob)
  age <- round(rnorm(n, mean = age.mean, sd = age.sd), 0)
  age[age < 0] <- 0
  edu <- sample(c(1:5), n, replace = TRUE, prob = edu.prob)
  x1 <- round(rnorm(n, 100, 20), 2)
  x2 <- round(runif(n), 4)
  id <- 1:n
  data <- data.frame(id, age, gender, raceth, edu, x1, x2)
  if (rownames) {
    lett <- sample(LETTERS, size = length(id), replace = TRUE)
    rownames(data) <- paste0(lett, "-", id)
  }

  return(data)
}
