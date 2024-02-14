#' Add A Dummy Variable for Each Value Label
#'
#' @description
#' For one or more value-labeled data.frame columns, create a dummy (aka
#' indicator) variable for each unique value label.
#'
#' @details
#' If the default of simple.names is used, dummy variable column names will be
#' the "parent" variable column name, followed by a separator character (by
#' default, "_"), followed by a number, to differentiate each dummy variable
#' from the others in the set. If one of the automatically generated dummy
#' column names is already "taken" by a pre-existing data.frame column, an error
#' to this effect will be thrown. If simple.names = FALSE, then prefix.length
#' and suffix.length arguments will be used to construct dummy variable column
#' names using the leading characters of the parent column name, followed by a
#' separator character, followed by the leading characters of the value label.
#' (white spaces in the value label will be replaced with the separator
#' character).
#'
#' Note: `ald()` is an alias function that behaves identically to
#' `add_lab_dummies`.
#'
#' @param data a data.frame with at least one value-labeled variable (column).
#' @param data vars the value-labeled variable or variables from which dummy
#' variable columns will be generated (variable names must be quoted).
#' @param data sep the separator character to use in constructing dummy variable
#' column names (appears between the dummy variable name prefix and suffix).
#' @param simple.names if TRUE (the default), dummy variable names will be the
#' parent variable's name, followed by the sep separator (see above), followed
#' by an automatically generated numerical id suffix. For example two dummy
#' variable columns created from value-labeled column "tacos" using the sep
#' argument of "." would be given the respective names "tacos.1" and "tacos.2").
#' @param data prefix.length (NOTE: This argument is ignored if
#' simple.names = TRUE). A 1L integer indicating the number of leading
#' characters of the parent column's name to use in constructing dummy variable
#' column names. For example, if simple.names = FALSE, if prefix.length = 2, and
#' for a parent column named "tacos", each derivative dummy variable column name
#' will begin with the prefix string "ta," (corresponding to the first two
#' characters of "tacos"), followed by the sep separator character (see sep
#' param, above), followed by the suffix string (see suffix.length param, below).
#' @param suffix.length (NOTE: This argument is ignored if simple.names = TRUE).
#' A 1L integer indicating the number of leading characters of each variable
#' value label to use use in constructing dummy variable column names. For
#' example, consider the following setup: parent column name is "tacos";
#' prefix.length = 3; sep = "_", and suffix.length = 2. In this case, if
#' simple.names = FALSE, then a dummy variable column named "tac.so" would be
#' created to represent those values of the tacos" column that have the value
#' label "soft" (because "tac" are the first three letters of the parent column
#' name, the separator is ".", and "so" are the first two characters in "soft").
#' @return A data.frame with dummy variables added for all value labels of the
#' value-labeled columns supplied to the vars argument.
#' @export
#' @examples
#' # one variable at a time, mtcars
#' df <- mtcars
#'
#' # now, add 1-to-1 value labels
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
#' # add many-to-1 value labels
#' df <- add_m1_lab(
#'   data = df,
#'   vars = "gear",
#'   vals = 4:5,
#'   lab = "4+"
#' )
#'
#' # add quartile-based numerical range value labels
#' df <- add_quant_labs(
#'   data = df,
#'   vars = "disp",
#'   qtiles = 4
#' )
#'
#' # add "pretty" cut-based numerical range value labels
#' (mpg_bins <- pretty(range(df$mpg, na.rm = TRUE)))
#'
#' df <- add_quant_labs(data = df, vars = "mpg", vals = mpg_bins)
#'
#' # add dummy variables for the labels of column "am"
#' df2 <- add_lab_dummies(df, "am",
#'   sep = ".", simple.names = FALSE,
#'   prefix.length = 2, suffix.length = 6
#' )
#' df2
#'
#' # add dummy variables for the labels of columns "mpg", "gear", and "cyl
#' df3 <- add_lab_dummies(df, c("mpg", "gear", "cyl"), simple.names = TRUE) # default
#' df3
add_lab_dummies <- function(data, vars,
                            simple.names = TRUE,
                            sep = "_",
                            prefix.length = 4,
                            suffix.length = 7) {
  # ---------------------------------------------------------------------------#
  # begin **gen_lab_dummies()**
  # ---------------------------------------------------------------------------#
  # function to generate data.frame of dummy variables for one value-labeled var
  # ---------------------------------------------------------------------------#
  gen_lab_dummies <- function(data, var,
                              simple.names = TRUE,
                              sep = "_",
                              prefix.length = 4,
                              suffix.length = 7) {
    # subset down to var of interest
    data <- sbrac(data, , var)

    # ensure value labels are sorted
    data <- sort_val_labs(data)

    # function to use numeric range labs for numeric variables
    q_labs_vec <- function(data, var) {
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

    # get value labs
    val.labs <- get_val_labs(data)

    # capture variable names
    if (!all(var %in% names(data)) && !all(is.na(var))) {
      stop("\n
var supplied to add_lab_dummies() not found in the supplied data.frame.

Did you drop or rename the column (var) after value-labeling it? If so, add_lab_dummies()
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

      # if not m1 and is numable, use q_labs_vec() vals-to-labs conversion
      if (num_test && not_m1_test) {
        var_new <- q_labs_vec(data, var)
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
        vals_to_fix <- which(is.na(var_new) & !is.na(var_old))
        var_new[vals_to_fix] <- var_old[vals_to_fix]
      }
    }

    k_dummies <- length(unique(var_new[!is.na(var_new)]))
    all_dummies <- vector(mode = "list", length = k_dummies)
    var_new_length <- length(var_new)
    counter <- 0
    dumm_pref <- substr(var, 1, prefix.length)


    for (i in unique(var_new[!is.na(var_new)])) {
      dumm_suff <- substr(i, 1, suffix.length)
      this_dummmy <- rep(NA, var_new_length)
      counter <- counter + 1
      this_dummmy[var_new == i] <- 1
      this_dummmy[!is.na(var_new) & var_new != i] <- 0
      all_dummies[[counter]] <- this_dummmy
      names(all_dummies)[[counter]] <- paste0(dumm_pref, sep, dumm_suff)
    }

    all_dummies <- as.data.frame(do.call("cbind", all_dummies))
    all_dummies <- all_dummies[, sort(names(all_dummies)), drop = FALSE]

    if (simple.names) {
      # add necessary leading zeros to simple suffixes
      simple_suffixes <- as.character(seq_len(k_dummies))
      while (max(nchar(simple_suffixes)) != min(nchar(simple_suffixes))) {
        short_suffixes <- which(nchar(simple_suffixes) < max(nchar(simple_suffixes)))
        simple_suffixes[short_suffixes] <- paste0("0", simple_suffixes[short_suffixes])
      }

      names(all_dummies) <- paste0(var, sep, simple_suffixes)
    }

    names(all_dummies) <- tolower(gsub(" ", "\\.", names(all_dummies)))
    names(all_dummies) <- gsub("\\.", sep, names(all_dummies))
    return(all_dummies)
  }
  # ---------------------------------------------------------------------------#
  # end **gen_lab_dummies()**
  # ---------------------------------------------------------------------------#

  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # get label attributes
  initial_lab_atts <- get_all_lab_atts(data)

  if (nrow(data) > 300000) {
    warning("
Note: labelr is not optimized for data.frames this large.")
  }

  for (var in vars) {
    # subset down to var of interest
    data_var <- sbrac(data, , var)

    # ensure value labels are sorted
    data_var <- sort_val_labs(data_var)

    # generate the dummies to be added to data.frame
    var_dummies <- gen_lab_dummies(
      data = data_var,
      simple.names = simple.names,
      var = var,
      sep = sep,
      prefix.length = prefix.length,
      suffix.length = suffix.length
    )

    # make sure these dummy variables (or other vars with same names) not
    # ...already present in supplied data.frame

    if (any(names(var_dummies) %in% names(data))) {
      bad_dummy <- names(var_dummies)[names(var_dummies) %in% names(data)][1]
      stop(sprintf(
        "
One or more requested dummy vars (including --%s--) already present in data.frame.",
        bad_dummy
      ))
    } else {
      data <- cbind(data, var_dummies)
      data <- add_lab_atts(data, initial_lab_atts, num.convert = FALSE)
    }
  }

  return(data)
}

#' @export
#' @rdname add_lab_dummies
ald <- add_lab_dummies
