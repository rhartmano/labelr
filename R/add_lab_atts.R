#' Add labelr Attributes from a list to a Data Frame
#'
#' @description
#' `add_lab_atts` allows one to apply a list of labelr label attribute meta-data
#' (created by `get_all_lab_atts`) to a data.frame.
#'
#' @details
#' See `get_all_lab_atts`.
#'
#' `add_lab_atts` allows one to add or restore label attributes from a free-
#' standing list (created by `get_all_lab_atts`) to a data.frame.
#'
#' @param data a data.frame object.
#' @param lab.atts.list a list previously created using `get_all_lab_atts`.
#' @param strip.first FALSE if you do not wish to strip the data.frame of all
#' label attribute information it may already have (this information may still
#' be overwritten, depending on what is in the lab.atts.list list).
#' @param num.convert attempt to convert to numeric any data.frame variables
#' where this can be done without producing new NA values.
#' @param clean after adding label attributes, put them into a neat, logical
#' order and drop any attributes that describe variables (columns) not present
#' in the data.frame to which they have been added.
#'
#' @return a data.frame object with label attribute information (re-) attached
#' (if it exists in the specified lab.atts.list).
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
#' get_val_labs(df, "raceth") # it's here
#'
#' zlab.df <- get_all_lab_atts(df) # back up labelr attributes for df
#'
#' df <- strip_labs(df) # this removes labs from df
#'
#' get_val_labs(df, "raceth") # it's gone
#'
#' check_any_lab_atts(df) # FALSE (means "no labs here")
#'
#' df <- add_lab_atts(df, zlab.df) # restore them
add_lab_atts <- function(data, lab.atts.list,
                         strip.first = FALSE,
                         num.convert = FALSE,
                         clean = TRUE) {
  # strip "labeled.data.frame" class if present
  # will be added back at the end
  if (any(class(data) %in% "labeled.data.frame")) {
    class(data) <- class(data)[!class(data) %in% "labeled.data.frame"]
  }

  if (length(class(data)) != 1 && "data.frame" %in% class(data)) {
    warning("
data argument object is an augmented data.frame and may not interoperate with all
labelr functions. Consider coercing to Base R data.frame using as.data.frame().")
  }

  if (length(lab.atts.list) != 0) {
    if (num.convert) data <- as_num(data)
    if (strip.first) data <- strip_labs(data)
    atts_names <- names(lab.atts.list)
    atts_vals <- unname(lab.atts.list)
    atts.inds <- seq_along(lab.atts.list)

    for (i in atts.inds) {
      attributes(data)[atts_names[i]] <- atts_vals[i]
    }

    # if clean = TRUE, clean and re-rearrange attributes
    if (clean) {
      data <- clean_data_atts(data)
    }

    # sort all variable value labels in ascending order of values
    data <- sort_val_labs(data)
  }

  # add "labeled.data.frame" class
  data <- as_labeled_data_frame(data)

  return(data)
}
