#' Swap Variable Value Labels for Variable Values
#'
#' @description
#' Replace the actual values of data.frame variables with the corresponding
#' value labels (previous assigned using `add_val_labs` or a related function).
#'
#' @details
#' Note: `uvl` is a compact alias for `use_val_labs`: they do the same thing,
#' and the former is easier to type.
#'
#' Warning: `use_val_labs` will replace existing variable values with value
#' labels and cannot be undone. If you wish to preserve variable values, be sure
#' to assign the result of `use_val_labs` to a new object (vs. overwriting the
#' data.frame you supply to the data argument), OR use `add_lab_cols` to add
#' variables containing value labels to the supplied data set without replacing
#' the original variables, OR use `val_abs_vec` to replace a single column's
#' values with its value labels and return the result as a stand-alone character
#' vector. For other ways to leverage value labels for common data management or
#' inspection tasks, while preserving raw data values in returned object, see
#' `flab` ("filter using labels"), `slab` ("subset using labels"), `tabl`
#' (tabulate frequencies using labels), `somel`, `headl`, and `taill`.
#'
#' `use_val_labs` works with `add_val_labs`, `add_val1`, `add_quant_labs`,
#' `add_q1`, `add_m1_lab`, `add1m1`, `get_val_labs`, `drop_val_labs`, and
#' `drop_val1` to facilitate creation, modification, accessing, use, and
#' destruction of variable-specific value labels.
#'
#' `use_val_labs` takes a variable value-labeled data.frame and substitutes each
#' (labeled) variable's labels for its values, returning a data.frame whose
#' dimensions, names, and members are the same as the inputted data.frame. This
#' may be useful if one wishes to inspect the data.frame (using, e.g., head(),
#' tail(), View()) or labeled value frequencies (e.g., table()) using the
#' (potentially) more intuitively meaningful value labels (e.g., gender=1 values
#' displayed as "Male" instead of 1).
#' @param data a data.frame.
#' @param vars the names of the columns (variables) for which labels-on
#' versions of the variable will replace the original variable in the returned
#' data.frame.
#'
#' @return A data.frame, with (all or the select) variable value labels "turned
#' on" (i.e., substituted for original variable values), and any affected
#' variables coerced to character if they were not already.
#' @export
#' @examples
#' # Example #1 - mtcars example, one variable at a time
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
#' head(use_val_labs(df), 3) # they're there
#'
#' # Example #2 - (Fake) Likert Data
#' # add val labs to multiple variables at once
#' # make a "Likert"-type fake data set to demo
#' # note, by default, add_val_labs() "vars" arg will do partial matching
#' # in this case, we catch all vars with "x" in their name
#' set.seed(272)
#' dflik <- make_likert_data(scale = 1:7)
#' vals2label <- 1:7
#' labs2use <- c(
#'   "VSD",
#'   "SD",
#'   "D",
#'   "N",
#'   "A",
#'   "SA",
#'   "VSA"
#' )
#'
#' dflik <- add_val_labs(
#'   data = dflik, vars = c("x", "y3"), # note the vars args
#'   vals = vals2label,
#'   labs = labs2use,
#'   partial = TRUE
#' )
#'
#' # note, all "x" vars get the labs, as does "y3"
#' # see vars = args above
#' lik1 <- use_val_labs(dflik)
#' head(lik1)
#' # keep a copy
#' dflik_conv <- use_val_labs(dflik)
#' head(dflik_conv, 3)
use_val_labs <- function(data, vars = NULL) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # ensure value labels are sorted
  data <- sort_val_labs(data)

  if (nrow(data) > 300000) {
    message("
Note: labelr is not optimized for data.frames this large.")
  }

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

  # check systematically for all found values being NA
  size <- 5000
  if (nrow(data) < size) size <- nrow(data)
  inds2check <- unique(floor(seq(1, nrow(data), length.out = size)))
  any_all_na_init <- any(sapply(data[inds2check, ], function(x) all(is.na(x))))

  # get value labs
  val.labs <- get_val_labs(data)

  # capture variable names
  if (is.null(vars)) vars <- unique(get_val_labs(data)[["var"]])

  if (!all(vars %in% names(data)) && !all(is.na(vars))) {
    stop("\n
One or more vars supplied to use_val_labs() not found in the supplied data.frame.

Did you drop or rename a column (var) after value-labeling it? If so, use_val_labs()
may be trying to modify a variable (column) that no longer exists -- at least not by
the column name it had when you labeled it.

Explore commands like get_val_labs(), drop_val_labs(), and srename() for tools to
prevent and troubleshoot these sorts of issues.
         ")
  } else if (all(is.na(vars))) {
    warning("
\nNo value-labeled vars found. Run get_val_labs() on your data.frame to see which,
if any, variables have value labels.
         ")
  } else {
    # use the labels (recode from vals to labels)
    for (i in seq_along(vars)) {
      var_name <- vars[i]

      # handle any labeled numerical values
      val_lab_name <- paste0("val.labs.", var_name)

      # handle value-labeled numerical variables
      # test for whether variable could be numeric
      num_test <- is_numable(names(attributes(data)[[val_lab_name]]))

      # test for presence of many-to-one (m1) labels
      this_var_val_lab <- get_labs_att(data, val_lab_name)[[1]]

      not_m1_test <- length(unique(names(this_var_val_lab))) == length(unique(unname(this_var_val_lab)))

      # if not m1 and is numable, use use_q_labs() vals-to-labs conversion
      if (num_test && not_m1_test) {
        data <- use_q_labs(data, var_name)

        # handle other nominal value-labeled variables
      } else if (var_name %in% val.labs$var) {
        val_labv <- unlist(attributes(data)[val_lab_name])
        names(val_labv) <- gsub(paste0(val_lab_name, "."), "", names(val_labv))
        var_old <- data[[var_name]]
        var_old <- as.character(var_old)
        var_old <- irregular2v(var_old, "NA")
        var_new <- val_labv[var_old]
        var_new <- unname(var_new)
        var_new <- as_numv(var_new)
        data[[var_name]] <- var_new
        vals_to_fix <- which(is.na(var_new) & !is.na(var_old))
        data[vals_to_fix, var_name] <- var_old[vals_to_fix]
      } else {
        next
      }

      attributes(data)[[val_lab_name]] <- NULL
    }

    # check systematically for columns that lost many values to NA
    inds2check <- unique(floor(seq(1, nrow(data), length.out = size)))
    any_all_na_end <- any(sapply(data[inds2check, ], function(x) all(is.na(x))))

    # throw an error if some column acquired new NA values based on
    # non-comprehensive but systematic test
    if (!any_all_na_init && any_all_na_end) {
      stop("
\nThis application of use_val_labs() would lead a column to be coerced to all NA values,
which is not allowed. This may result from attempting multiple nested or redundant
calls to use_val_labs().
           ")
    }

    data <- clean_data_atts(data)
  }

  return(data)
}

#' @export
#' @rdname use_val_labs
uvl <- use_val_labs
