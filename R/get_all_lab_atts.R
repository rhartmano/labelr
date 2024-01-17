#' Put all Data Frame label attributes into a List
#'
#' @description
#' `get_all_lab_atts` returns a list of labelr-generated meta-data attributes
#' attached to a data.frame, all of which should have names beginning with one
#' of these character strings: "frame.lab", "name.labs", \cr
#' "val.labs", "factor.".
#'
#' @param data the data.frame you are checking for labelr meta-data attributes.
#' @param atts default is to look for all/any, but you can specify a more
#' narrow subset (or some other, altogether-irrelevant attribute, but do not do
#' that).
#' @return A free-standing list of labelr attributes.
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000) # another labelr:: function
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' get_all_lab_atts(df) # returns all; is default
#' get_all_lab_atts(df, "val.labs") # returns only "val.labs" attributes
#' get_all_lab_atts(df, "class") # You can (but probably should not) use this way.
get_all_lab_atts <- function(data, atts = c("name.labs|val.labs|frame.lab|factor.")) {
  data_atts <- attributes(data)
  all_names <- names(data_atts)
  labs_names <- names(data_atts)[sapply(names(data_atts), function(x) grepl(atts, x))]
  labs_atts <- data_atts[labs_names]
  return(labs_atts)
}
