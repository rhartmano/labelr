#' Create a Value Labels Column for a Single Variable and Add to the Data Frame
#'
#' @description
#' For a single value-labeled column of a data.frame, create a copy of that
#' column that replaces all of its values with the corresponding value labels
#' and added that copy to the supplied data.frame.
#'
#' @details
#' Note 1: `add_lab_col1` is a variant of `add_lab_cols` that allows you to
#' specify only one variable at a time but that allows you to pass its name
#' without quoting it (compare add_lab_col1(mtcars, am) to
#' add_lab_cols(mtcars, "am")).
#'
#' Note 2: `alc1` is a compact alias for `add_lab_col1`: they do the same thing,
#' and the former is easier to type.
#'
#' Note 3: This command is intended exclusively for interactive use. In
#' particular, the var argument must be the literal name of a single variable
#' (column) found in the supplied data.frame and may NOT be, e.g., the name of a
#' character vector that contains the variable (column name) of interest. If you
#' wish to supply a character vector with the names of variables (columns) of
#' interest, use `add_lab_cols()`.
#'
#' `add_lab_col1` creates a "labels-on" version of a value-labeled column and
#' adds that new column to the supplied data.frame. Here, "labels-on" means that
#' the column's original values are replaced with the corresponding value
#' labels. Note that this column does not replace but is added to its
#' parent/source columns in the returned data.frame. The resulting "labels-on"
#' column is a simple, self-contained character column that cannot itself be
#' converted or reverted to the original ("labels-off") values of its
#' parent/source column. See `add_lab_cols` for a list of other functions that
#' may be useful in working with value labels.
#'
#' @param data a data.frame.
#' @param var the unquoted name of the column (variable) whose values you wish
#' to replace with the corresponding value labels.
#' @param suffix a suffix that will be appended to the name of the labels-on
#' column that is added to the data.frame (e.g., if suffix = "_lab," the
#' labels-on version of "x1" will be "x1_lab").
#'
#' @return A data.frame consisting of the originally supplied data.frame, along
#'  with the labels-on column added to it.
#' @export
#' @examples
#' # add "labels-on" version of "am" to copy of mtcars
#' df <- mtcars # copy of mtcars
#'
#' # now, add value labels
#' df <- add_val1(
#'   data = df,
#'   var = am,
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' # add value labels-on version of "am" to df, assign to df_plus
#' df_plus <- add_lab_col1(df, am)
#' head(df_plus[c("am", "am_lab")])
add_lab_col1 <- function(data, var, suffix = "_lab") {
  # use numeric range labs for numeric variables
  use_q_labsv <- function(data, var) {
    x <- data[[var]]
    x <- irregular2v(x, to = NA, nan.include = TRUE, inf.include = TRUE)
    this_val_label_var <- paste0("val.labs.", var)
    char_q <- attributes(data)[[this_val_label_var]]
    char_q <- char_q[char_q != "NA"]
    qvals <- as.numeric(names(char_q))
    names(qvals) <- as.character(char_q)
    qvals <- rev(qvals)

    x_out <- rep("Other", length(x))

    for (i in seq_along(qvals)) {
      this_val <- qvals[i]
      this_lab <- names(qvals)[i]
      x_out[!is.na(x) & x <= this_val] <- this_lab
    }

    x_out[is.na(x)] <- "NA"
    x_out <- as_numv(x_out)
    return(x_out)
  }

  # make var character value
  vars <- deparse(substitute(var))
  test_quote <- any(grepl("\"", vars))
  if (test_quote && is.character(vars)) vars <- gsub("\"", "", vars)
  vars <- gsub("c\\(", "", vars)
  vars <- gsub("\\(", "", vars)
  vars <- gsub("\\)", "", vars)

  # test for presence of var in data.frame
  if (!all(vars %in% names(data)) || length(vars) != 1) {
    stop("
\nInvalid var argument specification: var arg should be a single, unquoted
name of a variable that is present in the data.frame.
         ")
  }

  # test length of var
  if (length(suffix) != 1) {
    stop("\n
invalid suffix argument")
  }

  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # ensure value labels are sorted
  data <- sort_val_labs(data)

  if (nrow(data) > 300000) {
    warning("
\nNote: labelr is not optimized for data.frames this large.")
  }

  # get label attributes, to restore when we're done
  initial_lab_atts <- get_all_lab_atts(data)

  # get value labs
  val.labs <- get_val_labs(data)

  # capture variable names
  if (is.null(vars)) {
    vars <- gsub("val.labs.", "", names(get_all_lab_atts(data, "val.labs")))
    if (length(vars) == 0) {
      stop("
\nNo value-labeled variables found in data.frame./n")
    }
  }

  if (!all(vars %in% names(data))) {
    stop("
\nInvalid var argument specification: var arg should be a single, unquoted
name of a value-labeled variable present in the data.frame.
         ")
  }

  # use the labels (recode from vals to labels)
  for (i in seq_along(vars)) {
    var_name <- vars[i]
    var_name_suffix <- paste0(var_name, suffix)
    val_lab_name <- paste0("val.labs.", var_name)

    if (!check_labs_att(data, val_lab_name)) {
      stop(sprintf(
        "
No value labels found for supplied var --%s--.",
        var_name
      ))
    }

    # test for whether variable could be numeric
    num_test <- is_numable(names(attributes(data)[[val_lab_name]]))

    # test for presence of many-to-one (m1) labels
    this_var_val_lab <- get_labs_att(data, val_lab_name)[[1]]
    not_m1_test <- length(unique(names(this_var_val_lab))) == length(unique(unname(this_var_val_lab)))

    # if not m1 and is numable, use use_q_labsv() vals-to-labs conversion
    if (num_test && not_m1_test) {
      var_new <- use_q_labsv(data, var_name)
      data[[var_name_suffix]] <- var_new

      # handle other nominal value-labeled variables
      # these are the add_val_labs() and add_val1() value labels
    } else if (var_name %in% val.labs$var) {
      var_old <- data[[var_name]]
      var_old <- as.character(var_old)
      var_old <- irregular2v(var_old, NA)
      val_labv <- unlist(attributes(data)[val_lab_name])
      names(val_labv) <- gsub(paste0(val_lab_name, "."), "", names(val_labv))
      var_new <- val_labv[var_old]
      var_new <- unname(var_new)
      var_new <- as_numv(var_new)
      data[[var_name_suffix]] <- var_new
      vals_to_fix <- which(is.na(var_new) & !is.na(var_old))
      data[vals_to_fix, var_name_suffix] <- var_old[vals_to_fix]
    }
  }

  # to restore label attributes information
  data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
  return(data)
}

#' @export
#' @rdname add_lab_col1
alc1 <- add_lab_col1
