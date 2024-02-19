#' Add Variable Value Label Columns to a Data Frame
#'
#' @description
#' Add copies of value-labeled columns to a data.frame, where the new columns'
#' values are replaced with the corresponding value labels.
#'
#' @details
#' Note: `alc` is a compact alias for `add_lab_cols`: they do the same thing,
#' and the former is easier to type.
#'
#' `add_lab_cols` adds one or more "labels-on" columns to a data.frame, where
#' "labels-on" means that the column's original values are replaced with the
#' corresponding value labels. Note that these columns do not replace but are
#' added to their parent/source columns in the returned data.frame. The
#' resulting "labels-on" columns are simple, self-contained character columns
#' that cannot themselves be converted or reverted to the original
#' ("labels-off") values of their parent/source columns.
#'
#' For other ways of accessing or leveraging value labels, see, e.g.,
#' `use_val_labs`, `val_labs_vec`, `add_lab_dummies`, `lab_int_to_factor`,
#' `flab`, `slab`, `get_val_labs`, `with_val_labs`, `headl`, `taill`, `somel`,
#' and `tabl`. In particular, see `use_val_labs` if, rather than adding a
#' "labels-on" column to a data.frame, you wish to replace a column's values
#' with the corresponding value labels. See `val_labs_vec` if you wish to
#' convert a single, value-labeled column's values to labels and return the
#' result as a stand-alone vector, see `val_labs_vec`.
#'
#' @param data a data.frame.
#' @param vars the names of the columns (variables) for which "labels-on"
#' (values replaced with value labels) versions of the variable will be added to
#' the returned data.frame.
#' @param suffix a suffix that will be added to the names of all labels-on
#' variables added to the data.frame (the non-suffix portion of the variable
#' name will be identical to the original variable, e.g., the labels-on version
#' of "x1" will be "x1_lab" (or whatever alternative suffix you supply)).
#'
#' @return A data.frame consisting of the originally supplied data.frame, along
#'  with (all or the select) labels-on variable versions added to it.
#' @export
#' @examples
#' # one variable at a time, mtcars
#' df <- mtcars
#' # now, add value labels
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' df <- add_val_labs(
#'   data = df,
#'   vars = "carb",
#'   vals = c(1, 2, 3, 4, 6, 8),
#'   labs = c(
#'     "1-carb", "2-carbs",
#'     "3-carbs", "4-carbs",
#'     "6-carbs", "8-carbs"
#'   )
#' )
#'
#' # var arg can be unquoted if using add_val1()
#' # note that this is not add_val_labs(); add_val1() has "var" (not "vars) arg
#' df <- add_val1(
#'   data = df,
#'   var = cyl, # note, "var," not "vars" arg
#'   vals = c(4, 6, 8),
#'   labs = c(
#'     "four-cyl",
#'     "six-cyl",
#'     "eight-cyl"
#'   )
#' )
#'
#' df <- add_val_labs(
#'   data = df,
#'   vars = "gear",
#'   vals = c(3, 4),
#'   labs = c(
#'     "3-speed",
#'     "4-speed"
#'   )
#' )
#'
#' # Oops, we forgot 5-speeds; let's finish the job.
#' df <- add_val_labs(
#'   data = df,
#'   vars = "gear",
#'   vals = 5,
#'   labs = "5-speed"
#' )
#'
#' # add value labels-on versions of the foregoing to df and return as "df_plus"
#' df_plus <- add_lab_cols(df)
#' head(df_plus)
#' head(df_plus[c("am", "am_lab")])
add_lab_cols <- function(data,
                         vars = NULL,
                         suffix = "_lab") {
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

  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # ensure value labels are sorted
  data <- sort_val_labs(data)


  if (nrow(data) > 300000) {
    warning("
\nNote: labelr is not optimized for data.frames this large.")
  }

  # check systematically for all found values being NA
  size <- 5000
  if (nrow(data) < size) size <- nrow(data)
  inds2check <- unique(floor(seq(1, nrow(data), length.out = size)))
  any_all_na_init <- any(sapply(data[inds2check, ], function(x) all(is.na(x))))

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
\nOne or more vars supplied to add_lab_cols() not found in the supplied data.frame.
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

  # check systematically for columns that lost many values to NA
  inds2check <- unique(floor(seq(1, nrow(data), length.out = size)))
  any_all_na_end <- any(sapply(data[inds2check, ], function(x) all(is.na(x))))

  # throw an error if some column acquired new NA values based on
  # non-comprehensive but systematic test
  if (!any_all_na_init && any_all_na_end) {
    stop("
\nThis application of add_lab_cols() would lead a column to be coerced to all NA values,
which is not allowed. This may result from attempting multiple nested or redundant
calls to add_lab_cols() and/or related functions.
           ")
  }
  return(data)
}

#' @export
#' @rdname add_lab_cols
alc <- add_lab_cols
