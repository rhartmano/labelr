#' Return Specified Label Attribute, if Present
#'
#' @description
#' `get_labs_att` returns the specified piece of labelr lab(el) attribute meta-
#' data information if it is present.
#'
#' @param data the data.frame you are checking for the presence (or absence)
#' of labelr meta-data.
#' @param att the specific label meta-data you are looking for. Default of NULL
#' will return any and all meta-data with name substring "name.labs",
#' "val.labs", or "factor." (period is part of the substring).
#' @return A list.
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' df <- make_demo_data(n = 1000, seed = 555) # another labelr:: function
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
# get_labs_att(df, "var.labs") # a lot!
# get_labs_att(df, "val.labs.raceth") #less
get_labs_att <- function(data, att) {
  # ensure value labels are sorted
  data <- sort_val_labs(data)

  att_list <- get_all_lab_atts(data)
  check_logical <- check_labs_att(data, att)
  if (check_logical) {
    att_val <- att_list[att]
  } else {
    warning("Attribute not found")
    att_val <- NA
  }
  return(att_val)
}
