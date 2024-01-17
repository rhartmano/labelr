#' Strip All labelr Meta-data from a Data Frame
#'
#' @description
#' `strip_labs` removes all labelr meta-data attributes ("name.labs", "val.labs",
#' "frame.lab", and "factor.") from a data.frame, because you no longer want/need
#' it for some reason or another.
#'
#' @details
#' Some labelr functions automatically use `strip_labs`, but you should only
#' use it if you no longer want or need a given data.frame's labelr meta-data
#' (i.e., labels!). If you have saved your labelr attributes (using
#' `get_all_lab_atts`), you can restore them to the data.frame using
#' `add_lab_atts`).
#'
#' @param data a data.frame object.
#'
#' @return a data.frame object with label attribute meta-data stripped from it.
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
#' get_val_labs(df, "raceth") # it's here
#' df <- strip_labs(df) # this removes labs from df
#' get_val_labs(df, "raceth") # it's gone
#' check_any_lab_atts(df) # FALSE (means "no labs here")
strip_labs <- function(data) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)
  vlab_atts <- names(attributes(data))[grepl("val.labs", names(attributes(data)))]
  nlab_att <- names(attributes(data))[grepl("name.labs", names(attributes(data)))]
  flab_att <- names(attributes(data))[grepl("frame.lab", names(attributes(data)))]
  flv_att <- names(attributes(data))[grepl("factor.", names(attributes(data)))]

  attribs <- c(vlab_atts, nlab_att, flab_att, flv_att)

  if (length(attribs) != 0) {
    for (i in seq_along(attribs)) {
      attributes(data)[[attribs[i]]] <- NULL
    }
  }
  return(data)
}
