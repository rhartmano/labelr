#' Add or Modify a Variable's Value Labels
#'
#' @description
#' Add variable value-specific, descriptive value labels to a data.frame.
#'
#' @details
#' Note: `avl` is a compact alias for `add_val_labs`: they do the same thing,
#' and the former is easier to type
#'
#' `add_val_labs` is intended for associating value labels with binary,
#' nominal, or ordinal (e.g., integer) variables, where each of a limited number
#' of distinct values is to be associated one-to-one with a distinct value label.
#' To assign labels to ranges of numerical variables, see `add_quant_labs` (or
#' `add_quant1`). To apply the same label to multiple distinct values of a
#' variable, see `add_m1_lab` or `add1m1`.
#'
#' `add_val_labs` works with other labelr functions (e.g., `add_val1`,
#' `drop_val_labs`, `get_val_labs`, `use_val_labs`, `add_lab_cols`) to
#' facilitate the creation, accessing, modification, use, or deletion of
#' variable value labels, each of which is uniquely associated with a
#' specific distinct value of a specific variable (e.g., "Manual Transmission"
#' might be the value label for the distinct value mtcars$am==1).
#'
#' When using `add_val_labs` or `add_val1`, each distinct variable value can
#' receive one and only one value label, and for any given variable, each unique
#' label can be assigned to only one unique value (e.g., mtcars$gear==3 and
#' mtcars$gear==4 cannot both share a single "3 or 4 gears" label: each of these
#' two distinct values must have its own label). This latter constraint may be
#' relaxed by using `add_m1_lab`.
#'
#' If partial = TRUE, `add_val_labs` will apply the specified labeling scheme to
#' all variables that contain a key variable name substring of interest
#' (supplied to the vars argument), which may be one or more variables found in
#' the data.frame (see Example #2).
#'
#' @param data a data.frame.
#' @param vars a character vector that corresponds to the name(s) of one or more
#' variables to which value labels will be added.
#' @param vals a vector of distinct values of the actual variable, each of which
#' is to be associated with a label supplied to the labs argument in the same
#' positional order (e.g., vals = c(1,0), labs = c("manual", "automatic") will
#' associate lab "manual" with val 1 and lab "automatic" with val 0.). Note:
#' NA and other "irregular" (e.g., NaN, Inf) values all are automatically
#' assigned the label "NA", and this cannot be overridden. Note that you do not
#' need to specify all unique vals of var, and you can supply value labels
#' incrementally, one (or a few, or all) unique vals of var at a time. Once
#' you've added the value label, it is bound to that value until you drop it
#' (see `drop_val_labs`) or some other action (intentional or otherwise) strips
#' or overwrites it.
#' @param labs a character vector of distinct label values, each of which
#' is to be associated with exactly one corresponding distinct value (vals
#' argument element) of the variable(s) identified in the vars argument. The
#' order of labs argument must match that of vals argument entries (e.g., if a
#' three-element vector of values is supplied to vals, then a three-element
#' vector of proposed labels must be supplied to labs, and the first value of
#' vals will get the first label of labs, the second value of vals will get the
#' second label of labs, etc.). Note: NA and other "irregular" (e.g., NaN, Inf)
#' values are automatically assigned the label "NA" and may not be assigned
#' another label.
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
#' and partial=TRUE, then `add_val_labs` will attempt to label not only "x1",
#' "x2","x3", and "x4", but also "sex", "tax.bracket.", and other "x"-containing
#' variable names. Use of not.vars allows you to indicate variables that match
#' your vars argument that you do not wish to attempt to value-label. Note that
#' not.vars gets priority: setting vars="x", partial=TRUE, and not.vars="x" is
#' tantamount to telling add_val_labs() that you actually do not wish to label
#' any of the variables that you specified in vars, resulting in no variables
#' receiving value labels.
#' @param max.unique.vals `add_val_labs`() will not assign value labels to non-
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
#' # note that this is not add_val_labs(); add_val1() has "var" (not "vars" arg)
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
#' dflik <- make_likert_data(scale = 1:7, seed = 272)
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
add_val_labs <- function(data, vars, vals, labs,
                         partial = FALSE, not.vars = NULL,
                         max.unique.vals = 10,
                         init = FALSE) {
  # check max vals
  if (max.unique.vals > 5000) {
    stop("
    \n max.unique.vals may not exceed 5000.")
  }

  # use character version of vals as labs if latter is null
  if (is.null(labs) & !is.null(vals)) labs <- as.character(vals)

  if (length(vals) != length(labs)) {
    stop("
vals and labs arguments must be of equal length.\n")
  }


  # find cases where the same observation (coerced to character)
  # appears in both vals and labs but in different places
  # not allowed
  val_labs_conflict <- function(vals, labs) {
    vals <- as.character(vals)
    labs <- as.character(labs)

    vals_along <- seq_along(vals)

    contradict <- function(vals, labs, val.ind) {
      # any vals in val.ind also in labs other than val.ind?
      vals_i <- vals[val.ind]
      labs_not_i <- labs[-val.ind]
      test_forward <- any(vals_i %in% labs_not_i)

      # any vals in val.ind also in labs other than val.ind?
      labs_i <- labs[val.ind]
      vals_not_i <- vals[-val.ind]
      test_backward <- any(labs_i %in% vals_not_i)

      # both
      test_both <- any(test_forward, test_backward)
      return(test_both)
    }

    # test both for all indices
    test_all <- any(sapply(
      vals_along,
      function(zz) contradict(vals, labs, zz)
    ))

    return(test_all)
  }

  # find any contradictions like this and throw an error if we find them
  conflict_check <- val_labs_conflict(vals, labs)

  if (conflict_check) {
    stop("
At least one item in your vals argument also appears in your labs argument, but
in a different position. An example would be if \"dog\" appeared as your first
val but also as your third lab. This is not allowed: One observation's val
cannot be another observation's lab.\n")
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

      message(sprintf("
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

  # streamline the data.frame
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
    message("
\nNote: labelr is not optimized for data.frames this large.")
  }

  na.test <- check_irregular(c(vals, labs), any = TRUE)
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

  dupe_labs_test <- length(labs) != length(unique(labs))

  if (dupe_labs_test) {
    stop("
  \nThe same lab appears multiple times in your labs argument (not allowed). Try again.\n")
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
      used_lab_test <- any(labs %in% unname(get_labs_att(data_unique, this_val_label)[[1]]))
      if (used_lab_test) {
        # free up val lab(s) to be re-applied to other vals
        labs_to_overwrite <- labs[which(labs %in% attributes(data_unique)[[this_val_label]])]
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
      vals_vec <- recode_vals(vals,
        bef = vals,
        aft = labs,
        default.lab = "bef",
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
      # in case any vals not already present in x
      x <- c(vals, x)

      vals_vec <- recode_vals(x,
        bef = vals,
        aft = labs,
        default.lab = "bef",
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

    # put labels and corresponding character-coerced values in vector
    # labels are the vector's values and original var values are the names
    final_names <- names(get_labs_att(data_unique, this_var_val_label)[[1]])
    final_vals <- unname(get_labs_att(data_unique, this_var_val_label)[[1]])

    if (length(final_names) != length(final_vals)) {
      stop(sprintf(
        "\nConcerning Var --%s--
Var-specific error in specification of vals or labs.\n
Use get_val_labs() to see which value labels are currently applied to this
var and consider first dropping extant labels (using drop_val_labs()) and
then select the appropriate value-labeling approach for your var and preferred
value label end state: \n
(1) add_val_labs() is for one-to-one labels;
(2) add_m1_lab() is for applying the same label to more than one value; and
(3) add_quant_labs() is for applying labels to value ranges of a numeric var.",
        var
      ))
    }

    names(final_vals) <- final_names

    attributes(data_unique)[[this_var_val_label]] <- final_vals

    cat("\n")
    message(
      sprintf(
        "Labeling variable --%s--: \n", var
      )
    )
    print(get_val_labs(data_unique, var))
  }

  # end main loop
  cat("\n")
  if (partial) {
    message(
      "Remember: This command uses partial vars arg matching.
Use a more specific vars argument or set partial = FALSE
...if you want to restrict labeling to fewer/more specific variables.\n"
    )
  }

  lab_atts <- get_all_lab_atts(data_unique)

  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)
  return(data)
}
