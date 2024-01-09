#' (Alias for) `add_quant_labs()`
#'
#' @description
#' Add variable-specific value labels based on threshold cuts of a numerical variable.
#'
#' @details
#' Note: `aql` is an alias for `add_quant_labs`. See that command's documentation
#' for further information.
#'
#' @param data a data.frame.
#' @param vars a character vector that corresponds to the name(s) of one or more
#' variables to which value threshold-based labels will be added.
#' @param qtiles the number of quantile categories to employ (e.g., 4 would
#' indicate quartiles, 5 would indicate quintiles, 10 for deciles, etc.). If
#' NULL, vals must be non-NULL.
#' @param vals one more values of vars that will define range cutpoints, such
#' that all values at or below a given number and above the preceding val will
#' be treated as part of the same numerical range for labeling purposes. If
#' NULL, qtiles must be non-NULL.
#' @param labs a character vector of distinct labels to identify the
#' quantiles. If left NULL, convention "q" + quantile (e.g., "q10") will be used
#' for qtile-based labels (i.e., if qtiles arg is non-NULL), and convention
#' "<=" + val will be used for vals argument-based labels (i.e., if vals arg is
#' non-NULL). Note that the labels "NA" and "Other" are (non-case-sensitively)
#' reserved and may not be user-supplied.
#' @param partial To apply the same numerical value labeling scheme to many
#' variables at once, you can provide those variable names explicitly (e.g.,
#' vars = c("x1","x2", "x3") or vars = paste0("x", 1:3), or you can provide a
#' substring only and set partial = TRUE (default is FALSE). For example, to
#' apply the same labeling scheme to vars "x1", "x2" ... sequentially through
#' "x10", you could use vars = c("x"), along with partial = TRUE. Be careful with
#' this, as it also will attempt to apply the scheme to "sex" or "tax.bracket",
#' etc. (See not.vars argument for a way to mitigate this.)
#' @param not.vars use of the partial argument can result in situations where
#' you inadvertently attempt to value-label a variable. For example, if vars="x"
#' and partial=TRUE, then `aql` will attempt to label not only "x1",
#' "x2","x3", and "x4", but also "sex", "tax.bracket.", and other "x"-containing
#' variable names. Use of not.vars allows you to indicate variables that match
#' your vars argument that you do not wish to attempt to value-label. Note that
#' not.vars gets priority: setting vars="x", partial=TRUE, and not.vars="x" is
#' tantamount to telling add_val_labs() that you actually do not wish to label
#' any of the variables that you specified in vars, resulting in no variables
#' receiving value labels.
#'
#' @return A data.frame, with new variable value labels added (call
#' `get_val_labs` to see them), other provisional/default labelr label
#' information added, and previous user-added labelr label information
#' preserved.
#' @importFrom stats quantile
#' @export
#' @examples
#' # mtcars demo
#' df <- mtcars
#' # now, add value labels
#' df <- add_val_labs(
#'   data = df,
#'   vars = "am",
#'   vals = c(0, 1),
#'   labs = c("automatic", "manual")
#' )
#'
#' # label variable "mpg" in terms of 5 quintiles
#' df <- aql(data = df, vars = "mpg", qtiles = 5)
#'
#' # label variable "disp" in terms of "pretty" cutpoints
#' vals2use <- pretty(c(min(df$disp), max(df$disp)))[-1] # establish cutpoints
#' df <- aql(data = df, vars = "disp", vals = vals2use)
#' df_labson <- use_val_labs(df)
#' head(df_labson)
aql <- function(data, vars, qtiles = NULL, vals = NULL, labs = NULL,
                partial = FALSE, not.vars = NULL) {
  # capture data.frame name and coerce to Base R data.frame
  dfname <- deparse(substitute(data))
  data <- as_base_data_frame(data)

  # test for NULL - one of qtiles and vals must be NULL, other must be non-NULL
  null_qtiles <- is.null(qtiles)
  null_vals <- is.null(vals)
  null_both <- all(null_qtiles, null_vals)
  null_neither <- !any(null_qtiles, null_vals)

  if (null_both || null_neither) {
    stop("
Either qtiles or vals argument must be NULL, and the other must not be NULL.\n")
  }

  if (!is.null(vals)) {
    if (!all(sort(vals) == vals)) {
      stop("
vals argument values must appear in strict ascending order:
(e.g., c(10,50,100), NOT c(10,100,50)).\n")
    }

    # prohibit irregular vals argument elements
    na.test <- check_irregular(vals, any = TRUE)
    if (na.test) {
      stop("
Cannot supply NA, NaN, Inf, or character variants as a vals arg.
These are handled automatically.")
    }
  }

  # capture variable names substrings
  if (partial) {
    vars <- gremlr(vars, names(data), vals = TRUE)
  }

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

  if (nrow(data) > 300000) {
    message("
\nNote: labelr is not optimized for data.frames this large.")
  }

  dupe_labs_test <- length(labs) != length(unique(labs))

  if (dupe_labs_test) {
    stop("
  \nThe same lab appears multiple times in your labs argument (not allowed). Try again.\n")
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

    # drop value labels for var if any exist
    this_val_label <- paste0("val.labs.", var)
    this_var_have_val_labs <- check_labs_att(data, this_val_label)
    if (this_var_have_val_labs) {
      data <- drop_val_labs(data, var)
    }

    # grab var from data.frame and call it x
    x <- data[[var]]

    # remove NA values for labeling purposes
    x <- irregular2v(x, to = NA, inf.include = TRUE, nan.include = TRUE)
    x <- x[!is.na(x)]

    # verify that var is numeric
    if (!is.numeric(x)) {
      stop(sprintf(
        "\n\nVar --%s-- is not a numeric variable. Try add_val_labs()?\n", var
      ))
    }

    # Approch #1 - qtiles
    if (!is.null(qtiles)) {
      # check for excessive qtiles
      if (qtiles > 100 || qtiles < 2) {
        stop("
qtiles argument must be >1 and cannot exeed 100.\n")
      }

      # get mapping labs to percentile max vals
      qtiles_clean <- seq(1:qtiles)
      qtiles_expand <- quantile(x, probs = seq(0, 1, by = 1 / qtiles))[-1]
      qtiles_unique <- unique(qtiles_expand)
      qtiles_clean <- gsub("%", "", names(qtiles_expand))

      # second check for excessive qtiles
      if (length(qtiles_unique) != length(qtiles_expand)) {
        stop("
Too many qtiles specified (same raw value spans multiple qtiles).
Try fewer qtiles or switch to add_val_labs().\n")
      }
      # default labs if none supplied
      if (is.null(labs)) {
        these_labels <- paste0("q", c(qtiles_clean))

        these_labels[nchar(these_labels) == 3] <- gsub(
          "q",
          "q0",
          these_labels[nchar(these_labels) == 3]
        )
        these_labels[nchar(these_labels) == 2] <- gsub(
          "q",
          "q00",
          these_labels[nchar(these_labels) == 2]
        )
        # if labs are supplied
      } else if (any(c(toupper("na"), toupper("other")) %in% toupper(as.character(labs)))) {
        stop("
The value labels \"NA\" and \"Other\" are reserved and may not be used.
Please select other value labels in place of these.\n")
      } else {
        these_labels <- labs
      }

      # final labs with vals-as-names vector, with "NA" added
      final_vals <- these_labels
      final_vals[length(final_vals) + 1] <- "NA"
      final_names <- c(qtiles_expand, "NA")

      # Approach #2
      # alternative to qtiles - user-supplied <= value cut-offs
    } else if (!is.null(vals)) {
      if (any(vals < min(x)) || any(vals > max(x))) {
        warning(
          sprintf(
            "\n\nSome of the supplied vals argument values are outside
the observed range of var --%s-- values\n", var
          )
        )
      }

      final_names <- c(as.character(vals), "NA")
      # if no user-supplied labs (just paste <= val cut-offs as labs)
      if (is.null(labs)) {
        final_vals <- paste0("<=", final_names)
        final_vals <- gsub("<=NA", "NA", final_vals)

        # check for unacceptable user-supplied labels
      } else if (any(c(
        toupper("na"),
        toupper("other")
      ) %in% toupper(as.character(labs)))) {
        stop("
The value labels \"NA\" and \"Other\" are reserved and may not be used.
Please select other value labels in place of these.\n")

        # else accept, for now, user-supplied labels
      } else {
        final_vals <- c(as.character(labs), "NA")
      }
    }
    # catch problems in user-supplied labs
    if (length(final_vals) != length(final_names)) {
      stop("
Too many or too few labs supplied.\n")
    } else {
      # get sort order for original underlying values
      names_order <- order(suppressWarnings(as.numeric(final_names)))
      names(final_vals) <- final_names

      # sort by original values
      final_vals <- final_vals[names_order]
    }

    # assign value labels as data.frame attribute
    this_var_val_label <- paste0("val.labs", ".", var)
    attributes(data)[[this_var_val_label]] <- final_vals

    # Show user what value label meta-data is being added
    cat("\n")
    message(
      sprintf(
        "Labeling variable --%s--: \n", var
      )
    )
    print(get_val_labs(data, var))
  }

  # end main loop over vars being value-labeled
  cat("\n")

  # notify user if partial vars arg name matching is being used
  if (partial) {
    message(
      "Remember: This command uses partial vars arg matching.
Use a more specific vars argument or set partial = FALSE
...if you want to restrict labeling to fewer/more specific variables.\n"
    )
  }

  lab_atts <- get_all_lab_atts(data)

  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
