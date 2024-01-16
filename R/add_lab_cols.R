#' "Add Variable Value Label Columns to a Data Frame
#'
#' @description
#' For a data.frame with value-labeled columns, make copies of those columns
#' for which the labels are "turned on" (i.e., the values are converted to the
#' labels), and add those columns to the data.frame (with the same names as the
#' source columns plus a name suffix - "_lab," by default).
#'
#' @details
#' labelr defines "value labels" as a vector of character strings, each of which
#' is uniquely associated with a single distinct value of a data.frame column,
#' such that there is a one-to-one mapping where converting labels to values and
#' back again results in no information loss (i.e., no "collapsing" of multiple
#' distinct values into a common label-defined category). `add_lab_cols` is
#' an alternative to `use_val_labs` that preserves all variables (including
#' value-labeled ones) in their present state and adds a version of each value-
#' labeled variable for which values have converted labels -- allowing for
#' a direct, side-by-side view of each value's corresponding label (e.g., follow
#' along any row from "x1" to "x1_lab," noting which values go together). For
#' manageably-sized data.frames, this may be an acceptably compact and
#' comparatively more convenient, intuitive, and "safe" (i.e., less confusion-
#' or error-inducing) alternative to other workflows, such as those that involve
#' toggling back and forth between `use_val_labs` and `use_vals` and/or multiple
#' data.frames derived from them. (Note that the "labels-on" variables that are
#' added to the data.frame will be simple, self-contained character vectors that
#' cannot themselves be converted or reverted to the original ("labels-off")
#' values of the parent variables. Rather than acting as value-labeled variables
#' unto themselves, the labels-on variables that are added to the data.frame
#' should be regarded simply as free-standing character vectors with a purely
#' conceptual relationship to their "parent" variables, serving as pragmatic
#' adjuncts and/or surrogates for their parent variables wherever and to
#' whatever extent may be useful). See also `use_val_labs`, `use_vals`,
#' `add_val_labs`, `add_val1`, `get_val_labs`, `drop_val_labs`, and `drop_val1`.
#'
#' If you wish to convert a single, value-labeled column's values to labels and
#' return the result as a stand-alone vector, see `val_labs_vec`.
#' @param data the data.frame whose variable value labels you wish to leverage
#' to add labels-on version of value-labeled variables to the supplied
#' data.frame.
#' (aka swap, turn on, activate, etc.)
#' @param vars the names of the columns (variables) for which labels-on
#' versions of the variable will be added to the returned data.frame.
#' @param suffix a suffix that will be added to the names of all labels-on
#' variables added to the data.frame (the non-suffix portion of the variable
#' name will be identical to the original variable, e.g., the values-on version
#' of "x1" will be "x1_lab" (or whatever alternative suffix you supply).
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
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # ensure value labels are sorted
  data <- sort_val_labs(data)


  if (nrow(data) > 300000) {
    message("
\nNote: labelr is not optimized for data.frames this large.")
  }

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
      var_old <- irregular2v(var_old, "NA")
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
calls to add_lab_cols() and/or use_val_labs().
           ")
  }
  return(data)
}
