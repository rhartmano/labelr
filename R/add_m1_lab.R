#' Apply One Label to Multiple Values
#'
#' @description
#' Apply a single variable value label to multiple values of a variable
#' ("m1" is shorthand for "many values get one label").
#'
#' @details'
#' add_m1_lab` (and `add1m1`) allows the user to assign the same value label to
#' multiple distinct values of a variable ("m1" is short for "many-to-one").
#' This is in contrast to `add_val_labs` and `add_val1`, which require a strict
#' one-to-one mapping of distinct variable values and distinct value labels.
#'
#' Note 1: Each call to `add_m1_lab` accepts only one value label, which may be
#' applied to multiple distinct values of the specified column(s). Additional
#' labels can be applied to other values of the same column(s) by making
#' additional calls to `add_m1_lab` (see the example).
#'
#' Note 2: `am1l` is a compact alias for `add_m1_lab`: they do the same thing,
#' and the former is easier to type
#'
#' @param data a data.frame.
#' @param vars a character vector that corresponds to the name(s) of one or more
#' variables to which value labels will be added.
#' @param vals a vector of distinct values of the actual variable, each of which
#' is to be associated with the single label supplied to the lab argument. Note:
#' NA and other "irregular" (e.g., NaN, Inf) values all are automatically
#' assigned the label "NA", and this cannot be overridden.
#' @param lab a single distinct label that will be associated with all values
#' specified in your vals argument. Note: NA and other "irregular" (e.g.,
#' NaN, Inf) values are automatically assigned the label "NA" and may not be
#' assigned another label.
#' @param partial To apply the same value labeling scheme to many variables
#' at once, you can provide those variable names explicitly (e.g., vars =
#' c("x1","x2", "x3") or vars = paste0("x", 1:3), or you can provide a substring
#' only and set partial = TRUE (default is FALSE). For example, to apply the
#' same labeling scheme to vars "x1", "x2" ... sequentially through "x10",
#' you could use vars = c("x"), along with partial = TRUE. Be careful with
#' this, as it also will attempt to apply the scheme to "sex" or "tax.bracket",
#' etc.
#' @param not.vars use of the partial argument can result in situations where
#' you inadvertently attempt to value-label a variable. For example, if vars="x"
#' and partial=TRUE, then `add_m1_lab` will attempt to label not only "x1",
#' "x2","x3", and "x4", but also "sex", "tax.bracket.", and other "x"-containing
#' variable names. Use of not.vars allows you to indicate variables that match
#' your vars argument that you do not wish to attempt to value-label. Note that
#' not.vars gets priority: setting vars="x", partial=TRUE, and not.vars="x" is
#' tantamount to telling add_m1_lab() that you actually do not wish to label
#' any of the variables that you specified in vars, resulting in no variables
#' receiving value labels.
#' @param max.unique.vals `add_m1_lab`() will not assign value labels to non-
#' integer (i.e., decimal-having) numeric variables. The max.unique.vals
#' argument further constrains the variables that may receive value labels to
#' those whose total unique values do not exceed the integer value supplied to
#' this argument. Note that labelr sets a hard ceiling of 5000 on the total
#' number of unique value labels that any variable is permitted to have under
#' any circumstance, as labelr is primarily intended for interactive use with
#' moderately-sized (<=~1M-row) data.frames.
#' @param init assign placeholder labels for variables that lack decimals
#' and meet the max.unique.vals threshold.
#'
#' @return A data.frame, with new variable value labels added (call
#' `get_val_labs` to see them), other provisional/default labelr label
#' information added, and previous user-added labelr label information
#' preserved.
#' @export
#' @examples
#' df <- mtcars
#'
#' df <- add_m1_lab(df,
#'   vars = "carb",
#'   vals = 1:3,
#'   lab = "<=3",
#'   max.unique.vals = 10
#' )
#'
#' df <- add_m1_lab(df,
#'   vars = "carb",
#'   vals = c(4, 6, 8),
#'   lab = ">=4",
#'   max.unique.vals = 10
#' )
#'
#' get_val_lab1(df, carb)
#'
#' head(use_val_labs(df), 8) # they're there
add_m1_lab <- function(data, vars, vals, lab,
                       partial = FALSE, not.vars = NULL,
                       max.unique.vals = 10,
                       init = FALSE) {
  # function to streamline a data.frame while preserving prior labelr attributes
  sunique <- function(data, vars = NULL) {
    lab_atts <- get_all_lab_atts(data)
    if (!is.null(vars)) {
      data <- data[vars]
      data <- as.data.frame(data)
      names(data) <- vars
    }

    data_unique <- unique(data)
    data_unique <- add_lab_atts(data_unique, lab_atts,
      num.convert = FALSE,
      clean = FALSE
    )
    return(data_unique)
  }

  # function to recode many to one
  recode_m21 <- function(x, bef, aft, unique = FALSE) {
    x <- as.character(x)
    xuni <- sort(unique(x))
    oldxuni <- xuni
    bef <- as.character(bef)
    aft <- as.character(aft)
    if (!length(bef) >= 1) stop("vals argument must contain at least one element.")
    if (length(aft) != 1) stop("lab argument must contain exactly one element.")
    for (i in seq_along(bef)) {
      xuni[oldxuni == bef[i]] <- aft
    }

    names(xuni) <- oldxuni

    if (!unique) {
      x <- xuni[x]
      na_start <- sum(is.na(x))
      na_end <- sum(is.na(suppressWarnings(as.numeric(as.character(x)))))
      numable <- na_end == na_start
      if (numable) x <- as.numeric(x)
      x <- unname(x)
    } else {
      x <- xuni
    }
    return(x)
  }

  # check for one lab
  if (length(lab) != 1) stop("
lab argument must contain exactly one element.")

  # check max vals
  if (max.unique.vals > 5000) {
    stop("
    \n max.unique.vals may not exceed 5000.")
  }

  # capture data.frame name and coerce to Base R data.frame
  dfname <- deparse(substitute(data))
  data <- as_base_data_frame(data)

  # capture variable names substrings
  if (partial) vars <- gremlr(vars, names(data), vals = TRUE)

  # drop any vars in not.vars
  if (!is.null(not.vars)) {
    if (partial) not.vars <- gremlr(not.vars, names(data), vals = TRUE)
    vars <- base::setdiff(vars, not.vars)
  }

  # check again for no valid vars found
  if (!any(vars %in% names(data))) {
    stop("
Taken together, your inputs do not identify any vars to value-label. Possibilities include:
1. you meant to but did not set partial = TRUE;
2. you supplied not.vars input that \"cancels out\" (e.g., identifies the same var(s) as) your vars input;
3. your vars arg input requests a variable that simply does not exist in your data.frame, because
   you've previously dropped it or you've specified its name incorrectly.\n")
  }

  # check for incompatible vars
  if (any(!sapply(data[vars], function(x) check_class(x)))) {
    incomp_vars <- names(which(!sapply(data[vars], function(x) check_class(x))))[1]
    stop(sprintf("
One or more vars (including --%s--) are of class() that is not supported by labelr.
variable (column) vector classes must be numeric, integer, character, logical, or factor.", incomp_vars))
  }

  # vars that exceed max.unique.vals limit
  vars_exceed <- sapply(
    data[vars],
    function(x) length(unique(x)) > max.unique.vals
  )

  vars_exceed <- names(vars_exceed)[vars_exceed]

  if (length(vars_exceed) != 0) {
    for (i in seq_along(vars_exceed)) {
      this_val <- names(vars_exceed)[i]

      warning(sprintf("
    \n Var --%s-- exceeds  your max.unique.vals limit and will not be labeled.\n", this_val))
    }
  }

  # keep only variables that stay within max.unique.vals
  elig_vars <- names(data)[sapply(
    data,
    function(x) length(unique(x)) <= max.unique.vals
  )]

  if (!is.null(vars)) {
    if (any(!vars %in% elig_vars)) {
      stop("
\nOne or more of the vars supplied exceeds the max.unique.vals limit.
1. Increase your max.unique.vals argument and/or
2. If var is numerical:
   a. Round your var values or
   b. Use add_quant_labs() or add_quant1() to apply numerical range
      labels to the var.
    ")
    }
  }

  ### streamline your data.frame
  data_unique <- sunique(data, vars = elig_vars)

  # check again for no valid vars found
  if (!any(vars %in% names(data_unique))) {
    stop("
Taken together, your inputs do not identify any vars to value-label. Possibilities include:
1. you meant to but did not set partial = TRUE;
2. you selected a var whose unique values exceed the limit you've set with your max.unique.vals arg;
3. your vars arg input requests a variable that simply does not exist in your data.frame, because
   you've previously dropped it or you've specified its name incorrectly.\n")
  }

  if (nrow(data) > 300000) {
    warning("
\nNote: labelr is not optimized for data.frames this large.")
  }

  na.test <- check_irregular(c(vals, lab), any = TRUE)
  if (na.test) {
    stop("
Cannot supply NA, NaN, Inf, or character variants as a val or lab arg.
These are handled automatically.")
  }

  dupe_vars_test <- length(vars) != length(unique(vars))

  if (dupe_vars_test) {
    stop("
  \nThe same var name appears multiple times in your vars argument (not allowed). Try again.\n")
  }

  dupe_vals_test <- length(vals) != length(unique(vals))

  if (dupe_vals_test) {
    stop("
  \nThe same val appears multiple times in your vals argument (not allowed). Try again.\n")
  }

  dupe_labs_test <- length(lab) != length(unique(lab))

  if (dupe_labs_test) {
    stop("
  \nThe same lab appears multiple times in your lab argument (not allowed). Try again.\n")
  }

  # initialize value labels for eligible variables if requested
  if (init) {
    data_unique <- init_labs(data_unique,
      max.unique.vals = max.unique.vals
    )
  }

  # begin main loop
  for (i in seq_along((vars))) {
    var <- vars[i]

    # get var
    if (!var %in% vars) {
      stop(sprintf(
        "\n variable name %s not found in your data.frame\n", var
      ))
    }

    # see if this variable already has any val.labs
    # if so, check for already-assigned labels: each label can have only one value
    this_val_label <- paste0("val.labs.", var)
    this_var_have_val_labs <- check_labs_att(data_unique, this_val_label)
    if (this_var_have_val_labs) {
      used_lab_test <- any(lab %in% unname(get_labs_att(data_unique, this_val_label)[[1]]))
      if (used_lab_test) {
        # free up val lab(s) to be re-applied to other vals
        labs_to_overwrite <- lab[which(lab %in% attributes(data_unique)[[this_val_label]])]
        var_val_labs <- get_labs_att(data_unique, this_val_label)[[1]]
        var_val_labs[var_val_labs %in% labs_to_overwrite] <- names(var_val_labs)[var_val_labs %in% labs_to_overwrite]
        attributes(data_unique)[[this_val_label]] <- var_val_labs

        warning(sprintf(
          "
  You are re-assigning at least one value label(s) previously applied to other values of -- %s --.\n",
          var
        ))
      }
    }

    x <- data_unique[[var]]

    # handle factors and misc tests
    x <- data_unique[[var]]
    if (is.factor(x)) {
      data_unique <- add_factor_info(data_unique)
      x <- as.character(x)
    }

    if (!check_class(x)) {
      stop(sprintf(
        "\n\nVar --%s-- is of class() that is not supported by labelr. Its class
must be one of: numeric, integer, character, logical, or factor.", var
      ))
    }

    if (has_decv(x)) {
      stop(sprintf(
        "\n\nVar --%s-- is numeric with decimal values.\n
Round to whole number and/or coerce to character and try again.\n
Alternatively, use add_quant_labs() or add_quant1() to apply
numerical range labels to the variable in its current form.", var
      ))
    }

    if (length(unique(x)) > max.unique.vals) {
      stop(sprintf(
        "\nVar --%s-- has more unique vals than allowed.
Adjust max.unique.vals arg?", var
      ))
    }

    all_in <- all(unique(vals) %in% unique(x))
    if (!all_in) warning(sprintf("\n  Var --%s-- does not currently possess all of the vals you have specified.\n", var))

    # NA, Inf, NAN handling
    x <- as.character(x)
    x <- irregular2v(x, to = "NA", inf.include = TRUE, nan.include = TRUE)

    this_var_val_label <- paste0("val.labs", ".", var)
    if (check_labs_att(data_unique, this_var_val_label)) {
      vals_vec <- recode_m21(vals,
        bef = vals,
        aft = lab,
        unique = TRUE
      )

      for (i in seq_along(vals_vec)) {
        name_to_change <- names(attr(data_unique, this_var_val_label)) %in% names(vals_vec)[i]

        if (!any(name_to_change)) {
          current_val_labs <- attr(data_unique, this_var_val_label)
          new_val_label_to_add <- vals_vec[i]
          names(new_val_label_to_add) <- names(vals_vec)[i]
          current_plus_new <- attr(data_unique, this_var_val_label) <- c(current_val_labs, new_val_label_to_add)
          attr(data_unique, this_var_val_label) <- current_plus_new
        }
        attr(data_unique, this_var_val_label)[name_to_change] <- vals_vec[i]
      }
    } else {
      vals_vec <- recode_m21(x,
        bef = vals,
        aft = lab,
        unique = TRUE
      )

      na_element <- "NA"
      names(na_element) <- "NA"
      vals_vec <- c(vals_vec, na_element)
      attr(data_unique, this_var_val_label) <- vals_vec
    }

    # ensure no literal NA values as val.labs
    na_names_lab_att <- any(is.na(names(attributes(data_unique)[[this_var_val_label]])))
    if (na_names_lab_att) {
      na_names <- which(is.na(names(attributes(data_unique)[[this_var_val_label]])))
      attributes(data_unique)[[this_var_val_label]] <- attributes(data_unique)[[this_var_val_label]][-na_names]
    }

    # de-duplicate as needed
    final_names <- names(get_labs_att(data_unique, this_var_val_label)[[1]])
    final_vals <- unname(get_labs_att(data_unique, this_var_val_label)[[1]])

    labs_changed <- rep(lab, length(vals))
    names(labs_changed) <- vals

    final_names <- final_names[!final_names %in% vals]
    final_vals <- final_vals[!final_vals %in% c(lab)]

    names(final_vals) <- final_names
    final_vals <- c(final_vals, labs_changed)
    final_vals <- final_vals[sort(names(final_vals))]
    final_vals <- final_vals[unique(names(final_vals))]
    attributes(data_unique)[[this_var_val_label]] <- final_vals
  }

  # end main loop

  lab_atts <- get_all_lab_atts(data_unique)

  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}

#' @export
#' @rdname add_m1_lab
am1l <- add_m1_lab
