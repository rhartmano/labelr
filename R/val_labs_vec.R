#' Replace a Variable's Values with Its Value Labels and Return as a Vector
#'
#' @description
#' Select a single, value-labeled data.frame column (variable), replace each of
#' its values with the corresponding value labels, and return the result as a
#' vector.
#'
#' @details
#' Note 1: `vlv` is a compact alias for `val_labs_vec`: they do the same thing,
#' and the former is easier to type.
#'
#' Note 2: This command is intended exclusively for interactive use. In
#' particular, the var argument must be the literal name of a single variable
#' (column) found in the supplied data.frame and may NOT be, e.g., the name of a
#' character vector that contains the variable (column name) of interest.
#'
#' `val_labs_vec` works with other labelr functions to facilitate creation,
#' modification, accessing, use, and destruction of variable-specific value
#' labels. This functionality is equivalent to calling `use_val_labs` with a
#' single variable passed to the vars argument, except that the latter returns
#' the entire data.frame with that variable modified, while `val_labs_vec`
#' returns only that single modified variable itself (as a vector)
#'
#' @param data a data.frame.
#' @param var the unquoted name of the column (variable) whose values will be
#' converted to the associated value labels in the returned vector.
#'
#' @return A vector containing the original data.frame variable (var), after
#' its values have been converted to their corresponding value labels.
#' @export
#' @examples
#' df <- mtcars
#' # add value labels
#' df <- add_val_labs(
#'   data = df,
#'   var = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' am_labs <- val_labs_vec(df, am)
#'
#' length(df$am)
#'
#' class(df$am)
#'
#' df$am
#'
#' length(am_labs)
#'
#' class(am_labs)
#'
#' am_labs
#'
val_labs_vec <- function(data, var) {
  # use numeric range labs for numeric variables
  use_q_labs <- function(data, var) {
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
    data[[var]] <- x_out
    return(data)
  }

  # make var character value
  var <- deparse(substitute(var))
  test_quote <- any(grepl("\"", var))
  if (test_quote && is.character(var)) var <- gsub("\"", "", var)
  var <- gsub("c\\(", "", var)
  var <- gsub("\\(", "", var)
  var <- gsub("\\)", "", var)

  # test for presence of var in data.frame
  if (!all(var %in% names(data)) || length(var) != 1) {
    stop("
\nInvalid var argument specification: var arg should be a single, unquoted
name of a variable that is present in the data.frame.
         ")
  }

  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # subset down to var of interest
  data <- sbrac(data, , var)

  val_labs_att <- paste0("val.labs.", var)
  if (!check_labs_att(data, val_labs_att)) {
    stop(sprintf(
      "
No value labels found for supplied var --%s--.",
      var
    ))
  }

  # ensure value labels are sorted
  data <- sort_val_labs(data)

  if (nrow(data) > 300000) {
    warning("
Note: labelr is not optimized for data.frames this large.")
  }

  # get value labs
  val.labs <- get_val_labs(data)

  # capture variable names
  if (!all(var %in% names(data)) && !all(is.na(var))) {
    stop("\n
var supplied to val_labs_vec() not found in the supplied data.frame.

Did you drop or rename the column (var) after value-labeling it? If so, val_labs_vec()
may be trying to use a variable (column) that no longer exists -- at least not by the
column name it had when you labeled it.

Explore commands like get_val_labs(), drop_val_labs(), and srename() for tools to
prevent and troubleshoot these sorts of issues.
         ")
  } else if (all(is.na(var))) {
    warning("
\nNo value-labeled var with supplied name found. Run get_val_labs() on your
data.frame to see which, if any, variables have value labels.
         ")
  } else {
    # use the labels (recode from vals to labels)

    # handle any labeled numerical values
    val_lab_name <- paste0("val.labs.", var)

    # handle value-labeled numerical variables
    # test for whether variable could be numeric
    num_test <- is_numable(names(attributes(data)[[val_lab_name]]))

    # test for presence of many-to-one (m1) labels
    this_var_val_lab <- get_labs_att(data, val_lab_name)[[1]]

    not_m1_test <- length(unique(names(this_var_val_lab))) == length(unique(unname(this_var_val_lab)))

    # if not m1 and is numable, use use_q_labs() vals-to-labs conversion
    if (num_test && not_m1_test) {
      data <- use_q_labs(data, var)

      # handle other nominal value-labeled variables
    } else {
      val_labv <- unlist(attributes(data)[val_lab_name])
      names(val_labv) <- gsub(paste0(val_lab_name, "."), "", names(val_labv))
      var_old <- data[[var]]
      var_old <- as.character(var_old)
      var_old <- irregular2v(var_old, NA)
      var_new <- val_labv[var_old]
      var_new <- unname(var_new)
      var_new <- as_numv(var_new)
      data[[var]] <- var_new
      vals_to_fix <- which(is.na(var_new) & !is.na(var_old))
      data[vals_to_fix, var] <- var_old[vals_to_fix]
    }

    attributes(data)[[val_lab_name]] <- NULL
  }

  data <- clean_data_atts(data)
  x <- data[[var]]
  attributes(x) <- NULL
  return(x)
}

#' @export
#' @rdname val_labs_vec
vlv <- val_labs_vec
