#' Add or Modify Data Frame Variable Name Labels
#'
#' @description
#' Add descriptive variable name labels (up to one per variable) to a data.frame.
#'
#' @details
#' Note: `anl` is a compact alias for `add_name_labs`: they do the same thing,
#' and the former is easier to type
#'
#' `add_name_labs` works with `get_name_labs`, `use_name_labs`, and
#' `use_var_names` to facilitate the creation, accessing, and substitution of
#' variable name labels for variable names.
#'
#' Each variable (column) of a data.frame can receive one and only one "name
#' label," which typically is a noun phrase that expounds the meaning of
#' contents of the variable's name (e.g., "Weight in ounces at birth" might be a
#' name label for a column called "wgt"). `add_name_labs `takes a data.frame and
#' either a named character vector (names are current variable names, values are
#' proposed name labels) supplied to the name.labs arg or two separate character
#' vectors (one each for current variable names and proposed variable name
#' labels, respectively) supplied to vars and labs args, respectively. If using
#' the second approach, the order of each entry matters (e.g., the first
#' variable name entry to the vars argument will be given the label of the first
#' name label entry to the labs argument, and so on).
#'
#' Note that any non-name-labeled columns will receive their own names as
#' default name labels (e.g., if var "mpg" of mtcars is not assigned a name
#' label, it will be given the default name label of "mpg"). Note also that
#' other labelr functions (e.g., `add_val_labs`) will initialize name labels
#' and other labelr attribute meta-data in this same fashion. Name labels can
#' be removed with `drop_name_labs`.
#'
#' @param data the data.frame you wish to begin labeling.
#' @param name.labs a named character vector, where names are current data.frame
#' column (variable) names, and where values are the proposed labels. If this is
#' NULL, vars and labs arguments may not be NULL. If latter are not NULL, this
#' (name.labs) argument must be NULL.
#' @param vars the names of the columns (variables) to which name labels will be
#' applied. If NULL, labs arg must also be NULL and name.labs cannot be NULL.
#' @param labs the proposed variable name labels to applied to the columns
#' (variables). If non-NULL, vars arg must also be non-NULL.
#' @param init.max If non-NULL, this must be a 1L integer, indicating the maximum
#' number of unique values that a variable may have for it to receive placeholder
#' value labels, which will consist of the variable's actual values coerced to
#' character values. If NULL, or if the variable is numeric with decimal values,
#' the variable will not be given initialized variable value labels.
#'
#' @return A data.frame, with new name labels added (call `get_name_labs` to
#' see them), other provisional/default labelr label information added, and
#' previous user-added labelr label information preserved.
#' @export
#' @examples
#' # create a data set
#' df <- mtcars
#'
#' # variable names and their labels
#' names_labs_vec <- c(
#'   "mpg" = "Miles/(US) gallon",
#'   "cyl" = "Number of cylinders",
#'   "disp" = "Displacement (cu.in.)",
#'   "hp" = "Gross horsepower",
#'   "drat" = "Rear axle ratio",
#'   "wt" = "Weight (1000 lbs)",
#'   "qsec" = "1/4 mile time",
#'   "vs" = "Engine (0 = V-shaped, 1 = straight)",
#'   "am" = "Transmission (0 = automatic, 1 = manual)",
#'   "gear" = "Number of forward gears",
#'   "carb" = "Number of carburetors"
#' )
#'
#' # assign variable labels
#' df <- add_name_labs(df,
#'   vars = names(names_labs_vec),
#'   labs = names_labs_vec
#' )
#'
#' # see what we have
#' get_name_labs(df)
#'
#' # use these
#' df_labs_as_names <- use_name_labs(df)
#' head(df_labs_as_names)[1:3] # these are verbose, so, only show first three
#' head(df)[1:3]
#'
#' # now revert back
#' df_names_as_before <- use_var_names(df_labs_as_names)
#' head(df_names_as_before)[1:3] # indeed, they are as before
#' identical(head(df), head(df_names_as_before))
#'
#' # strip name label meta-data information from df
#' # NOT same as use_var_names(), which preserves the info but "turns it off"
#' # this strips the name labels meta-data from df altogether
#' df <- drop_name_labs(df)
#'
#' # see what we have
#' get_name_labs(df) # they're gone
#'
#' # alternative syntax (if you have a named vector like names_labs_vec)
#' # assign variable name labels
#' df <- add_name_labs(df,
#'   name.labs = c(
#'     "mpg" = "Miles/(US) gallon",
#'     "cyl" = "Number of cylinders",
#'     "disp" = "Displacement (cu.in.)",
#'     "hp" = "Gross horsepower",
#'     "drat" = "Rear axle ratio",
#'     "wt" = "Weight (1000 lbs)",
#'     "qsec" = "1/4 mile time",
#'     "vs" = "Engine (0 = V-shaped, 1 = straight)",
#'     "am" = "Transmission (0 = automatic, 1 = manual)",
#'     "gear" = "Number of forward gears",
#'     "carb" = "Number of carburetors"
#'   )
#' )
#'
#' # replace two variable name labels, keeping the others
#' df <- add_name_labs(df,
#'   name.labs = c(
#'     "disp" = toupper("displacement"),
#'     "mpg" = toupper("miles per gallon")
#'   )
#' )
#'
#' attributes(df) # show all attributes
#' get_name_labs(df) # show only the variable name labels
#' get_name_labs(df, var = c("disp", "mpg"))
#'
#' # again, strip name label meta-data information from df
#' # NOT same as use_var_names(), which preserves the info but "turns it off"
#' df <- drop_name_labs(df)
#'
#' # see what we have
#' get_name_labs(df) # they're gone
#'
#' # alternative syntax to add name labels
#' df <- add_name_labs(df,
#'   vars = c("carb", "am"),
#'   labs = c("how many carburetors?", "automatic or stick?")
#' )
#'
#' # see what we have
#' get_name_labs(df) # they're back! (and placeholders for others)
#'
#' # add another
#' df <- add_name_labs(df,
#'   vars = c("mpg"),
#'   labs = c("miles per gallon, of course")
#' )
#'
#' # see what we have
#' get_name_labs(df) # it's been added, and others preserved
#'
#' head(use_name_labs(df)[c(1, 9, 11)]) # verbose, but they're there
add_name_labs <- function(data,
                          name.labs = NULL,
                          vars = NULL,
                          labs = NULL,
                          init.max = NULL) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # initialize lab information if there is none
  if (!check_any_lab_atts(data) && !is.null(init.max)) {
    data <- init_labs(data, max.unique.vals = init.max)
  }

  if (!is.null(name.labs)) {
    if (!is.null(vars) || !is.null(labs)) {
      stop("
    \n If arguments are supplied to name.labs, no args may be supplied to vars or labs.")
    }

    vars <- names(name.labs)
    labs <- unname(name.labs)
  }

  if (is.null(attributes(data)[["name.labs"]])) {
    vals_vec <- names(data)
    names(vals_vec) <- names(data)
    attr(data, "name.labs") <- vals_vec
  } else {
    vals_vec <- unname(attributes(data)[["name.labs"]])
    names(vals_vec) <- names(attributes(data)[["name.labs"]])
  }

  specified_labs <- labs
  names(specified_labs) <- vars

  initial_vars <- names(attributes(data)[["name.labs"]])
  initial_labs <- unname(attributes(data)[["name.labs"]])
  names(initial_labs) <- initial_vars

  initial_labs <- initial_labs[!names(initial_labs) %in% names(specified_labs)]
  specified_labs <- c(specified_labs, initial_labs)

  vars <- names(specified_labs)
  labs <- unname(specified_labs)

  vals_vec <- recode_vals(names(data),
    bef = vars,
    aft = labs,
    default.lab = "bef"
  )

  names_vec <- names(data)
  names(vals_vec) <- names_vec
  attr(data, "name.labs") <- vals_vec

  # update and resort attributes
  lab_atts <- get_all_lab_atts(data)
  data <- add_lab_atts(data, lab_atts, num.convert = FALSE)

  return(data)
}
