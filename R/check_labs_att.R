#' Check Data Frame for Specified labelr Attribute
#'
#' @description
#' `check_labs_att` returns TRUE if your data.frame has the specific attribute
#' indicated and FALSE if it does not.
#'
#' @param data the data.frame you are checking for the presence (or absence)
#' of labelr meta-data.
#' @param att the specific label meta-data you are looking for. Default of NULL
#' will return TRUE if any valid labelr meta-data item of types "frame.lab",
#' "name.labs", "val.labs", or "factor." (period is part of the substring) is
#' present.
#' @return TRUE if any instance of the default or user-specified meta-data
#' attribute is found, FALSE if not.
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000) # another labelr:: function
#' # let's add variable VALUE labels for variable "race"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' check_labs_att(df) # is any valid labelr lab(el) attribute present?
#' check_labs_att(df, "val.labs.race") # "race" lab specifically TRUE
check_labs_att <- function(data, att = NULL) {
  atts <- c("name.labs|val.labs|frame.lab|factor.")
  data_atts <- attributes(data)
  all_names <- names(data_atts)
  labs_names <- names(data_atts)[sapply(names(data_atts), function(x) grepl(atts, x))]
  labs_atts <- data_atts[labs_names]

  check_logical <- length(labs_atts) != 0
  if (length(labs_atts) != 0 && !is.null(att)) {
    check_logical <- any(names(labs_atts) == att)
  }
  return(check_logical)
}
