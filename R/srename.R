#' Safely Rename a Variable and Preserve Its Value Labels
#'
#' @description
#' Note: `srename` renames and existing variable and preserves its value labels,
#' overwriting an existing variable only if option force = TRUE.
#'
#' @details
#' Any non-labelr R operation that changes a variable's (column's) name or that
#' copies its contents to another variable (column) with a different name will
#' not associate the original variable's value labels with the new variable name.
#' To mitigate this, `srename` allows one to rename a data.frame variable while
#' preserving its value labels -- that is, by associating the old.name's value
#' labels with the new.name. If the old.name variable (column) has a name label
#' (in "name.labs" attribute), the column name associated with that name label
#' will be changed from old.name to new.name.
#'
#' @param data a data.frame.
#' @param old.name the unquoted name of the existing variable being renamed (to
#' new.name).
#' @param new.name the unquoted name that will be used to rename the variable
#' specified in the old.name argument.
#' @param force if a variable with the same name as new.name already exists in
#' the data.frame, allow it to be overwritten. If FALSE, this will not be
#' allowed, and an error will be issued.
#'
#' @return A data.frame.
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
#' head(df, 4)
#' df <- srename(df, old.name = gender, new.name = genid)
#' df <- srename(df, old.name = raceth, new.name = racid)
#' df <- srename(df, old.name = x1, new.name = var1)
#' head(df, 4)
srename <- function(data, old.name, new.name, force = FALSE) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # make this work with or without the variable being quoted
  new.name <- deparse(substitute(new.name))
  test_quote <- any(grepl("\"", new.name))
  if (test_quote && is.character(new.name)) new.name <- gsub("\"", "", new.name)

  # make this work with or without the variable being quoted
  new.name <- deparse(substitute(new.name))

  test_quote <- any(grepl("\"", new.name))
  if (test_quote && is.character(new.name)) new.name <- gsub("\"", "", new.name)

  # make this work with or without the variable being quoted
  old.name <- deparse(substitute(old.name))

  test_quote_2 <- any(grepl("\"", old.name))
  if (test_quote_2 && is.character(old.name)) {
    old.name <- gsub(
      "\"", "",
      old.name
    )
  }

  # verify variables are in the data.frame
  old_name_out <- !old.name %in% names(data)

  if (old_name_out) {
    stop(sprintf("\n
No column with name --%s-- was found the supplied data.frame.

(1) Double-check old.name column spelling and
(2) Make sure your old.name and new.name arguments are correctly specified
(e.g., in the correct positional order).\n", old.name))
  }

  # warn the user if they are replacing a variable
  if (any(new.name %in% names(data)) && force) {
    data[[new.name]] <- NULL
    warning(sprintf("
\nVar --%s-- was already present in this data.frame and will be replaced.\n", new.name))
  } else if (any(new.name %in% names(data)) && !force) {
    stop(sprintf("
\nVar --%s-- was already present in this data.frame and cannot be replaced unless
option force = TRUE.\n", new.name))
  }

  names(data)[which(names(data) %in% old.name)] <- new.name
  to_var_val_label <- paste0("val.labs.", new.name)
  from_var_val_label <- paste0("val.labs.", old.name)

  # shift val.labs from old.name var to new.name var
  if (check_labs_att(data, from_var_val_label)) {
    attributes(data)[[to_var_val_label]] <- attributes(data)[[from_var_val_label]]
    attributes(data)[[from_var_val_label]] <- NULL
  }

  # if a name.lab exists for old.name, switch its colname to reflect new.name
  if (!is.null(attributes(data)[["name.labs"]])) {
    if (!is.null(attributes(data)[["name.labs"]][old.name])) {
      names_lab_att <- attributes(data)[["name.labs"]]
      names(names_lab_att)[names(names_lab_att) %in% old.name] <- new.name
      attributes(data)[["name.labs"]] <- names_lab_att
    }
  }

  # re-arrange attributes as needed
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
