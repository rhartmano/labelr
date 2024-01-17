#' Transfer Labels from One Variable (Column) Name to Another
#'
#' @description
#' Note: `transfer_labs` searches a data.frame's name.labs and val.labs
#' attributes and transfers the labels associated with one variable name to
#' another, so that the first variable no longer has name or value labels
#' associated with it, and so that whatever name or value labels previously were
#' associated with it are now associated with the second variable.
#'
#' @details
#' Certain non-labelr data management functions will preserve the labelr labels
#' that are attached to the passed data.frame, but they will not update those
#' labels to reflect any changes the function makes to the variable(s). For
#' example, if one were to use dplyr::rename to change the name of a
#' value-labeled variable from old name "x1" to new name "satisfaction", the
#' labelr attributes associated with "x1" would not be transferred to label
#' "satisfaction." `transfer_labs` allows one to transfer those labels, dis-
#' associating them with the old name (here, "x1") and associating them with new
#' name (here, "satisfaction").
#'
#' @param data a data.frame.
#' @param from the unquoted variable name from which labels will be transferred.
#' Note, even if the variable itself has been dropped from the data.frame (to
#' include being renamed), its label attribute meta-data may still be present and
#' available for use by this function (use `get_all_lab_atts()` to see).
#' @param to the unquoted name of the variable to which the labels will be
#' transferred.
#' @return A data.frame.
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
#' df <- add_val1(
#'   data = df, gender, vals = c(0, 1, 2),
#'   labs = c("Male", "Female", "Other"), max.unique.vals = 50
#' )
#'
#' # let's add variable NAME labels
#' df <- add_name_labs(df, name.labs = c(
#'   "age" = "Age in years",
#'   "raceth" = "racial-ethnic group category",
#'   "gender" = "gender identity"
#' ))
#' head(df, 4)
#' get_name_labs(df)
#' get_val_labs(df)
#'
#' df <- dplyr::rename(df, race = raceth) # new name is on left of = sign
#' df <- dplyr::rename(df, gend = gender) # new name is on left of = sign
#'
#' head(df, 4)
#' get_name_labs(df)
#' get_val_labs(df)
#'
#' df <- transfer_labs(df, from = raceth, to = race) # labs info transferred from raceth
#' df <- transfer_labs(df, from = gender, to = gend) # labs info transferred to gend
#' df <- transfer_labs(df, from = gend, to = nothere) # var nothere does not exist!
#'
#' head(df, 4)
#' get_name_labs(df)
#' get_val_labs(df)
transfer_labs <- function(data, from, to) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # this flag will be used to throw an error if no name or val labs
  # ..found for "from" var
  flag <- 0

  # make this work with or without the variable being quoted
  to <- deparse(substitute(to))
  test_quote <- any(grepl("\"", to))
  if (test_quote && is.character(to)) to <- gsub("\"", "", to)

  # make this work with or without the variable being quoted
  to <- deparse(substitute(to))

  test_quote <- any(grepl("\"", to))
  if (test_quote && is.character(to)) to <- gsub("\"", "", to)

  # make this work with or without the variable being quoted
  from <- deparse(substitute(from))

  test_quote_2 <- any(grepl("\"", from))
  if (test_quote_2 && is.character(from)) {
    from <- gsub(
      "\"", "",
      from
    )
  }

  from_var_val_label <- paste0("val.labs.", from)
  to_var_val_label <- paste0("val.labs.", to)

  # shift val.labs from "from" var to "to" var
  if (check_labs_att(data, from_var_val_label)) {
    attributes(data)[[to_var_val_label]] <- attributes(data)[[from_var_val_label]]
    attributes(data)[[from_var_val_label]] <- NULL
  } else {
    flag <- flag + 1
  }

  # if a name.lab exists for from, switch its colname to reflect to
  if (!is.null(attributes(data)[["name.labs"]])) {
    if (!is.null(attributes(data)[["name.labs"]][from])) {
      names_lab_att <- attributes(data)[["name.labs"]]
      names(names_lab_att)[names(names_lab_att) %in% from] <- to
      attributes(data)[["name.labs"]] <- names_lab_att
    }
  } else {
    flag <- flag + 1
  }

  # if neither name or value labels found for from, throw error
  if (flag == 2) {
    stop("
\nNo labels associated with from variable were found.\n")
  }

  # re-arrange attributes as needed
  all_att_names <- names(attributes(data))

  core_att_names <- c("names", "row.names", "class")
  core_in <- core_att_names[core_att_names %in% all_att_names]

  frame_name <- c("frame.lab", "name.labs")
  frame_name_in <- frame_name[frame_name %in% all_att_names]

  val_lab_names <- paste0("val.labs.", names(data))
  val_lab_in <- val_lab_names[val_lab_names %in% all_att_names]

  fact_names <- c(
    paste0("u.factor.", names(data)),
    paste0("o.factor.", names(data))
  )

  fact_in <- fact_names[fact_names %in% all_att_names]

  names_in_combined <- c(core_in, frame_name_in, val_lab_in, fact_in)

  other_names_in <- all_att_names[!all_att_names %in% names_in_combined]

  final_names <- c(
    core_in, other_names_in, frame_name_in,
    val_lab_in, fact_in
  )

  final_atts <- attributes(data)[final_names]

  attributes(data) <- NULL
  attributes(data) <- final_atts


  # if neither name or value labels found for from, throw error
  if (!to %in% names(data)) {
    warning("
\nLabels have been transferred, but note: the variable specified in your \"to\"
argument does not (yet) exist in the supplied data.frame. To transfer back,
call `transfer_labs` again, flipping your \"from\" and \"to\" arguments.\n")
  }

  return(data)
}
