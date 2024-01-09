#' (Alias for) `use_var_names()`
#'
#' @description
#' "Undo" or reverse `use_name_labs` operation, restoring the data.frame's
#' original variable names (assuming they were previously swapped out for labels
#' using `use_name_labs`).
#'
#' @details
#' `uvn` is an alias for `use_var_names`. See the `use_var_names` documentation
#' for further information.
#'
#' @param data a data.frame.
#' @param vars the names of the columns (variables) to which the action will be
#' applied. If NULL, all current variable names (presumably, the labels you
#' turned on using `use_name_labs` will be swapped out for their original
#' variable names available names.
#'
#' @return A data.frame.
#' @export
#' @examples
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
#' # add the above name labeling scheme
#' mt2 <- add_name_labs(mtcars, name.labs = names_labs_vec)
#'
#' # use the name labeling scheme (i.e., swap out column/variable names for
#' # ...their name labels)
#' mt2 <- use_name_labs(mt2)
#'
#' # compare these two - concision vs. informativeness
#' as.data.frame(sapply(mtcars, mean))
#' as.data.frame(sapply(mt2, mean))
#'
#' # compare the plot labeling we get with mtcars
#' with(mtcars, hist(mpg))
#'
#' get_name_labs(mt2) # get the lab of interest, and paste it into `` below
#' with(mt2, hist(`Miles/(US) gallon`))
#'
#' # regression - this is easier to type
#' lm(mpg ~ cyl, data = mtcars)
#'
#' # regression with name labs - more painful to type/copy-paste, but maybe
#' # ...the more informative labels are worth it (your mileage may vary)
#' # let's see the name labels, then copy paste mpg and cyl labs from console to
#' # ...where we need them in the lm() call
#' get_name_labs(mt2) # copy from this call's console output
#' lm(`Miles/(US) gallon` ~ `Number of cylinders`, data = mt2) # paste into `` here
#'
#' # same results, more informative labels, more steps/hand-jamming pain
#' # can also turn them on (semi) permanently
#' # ...then you can use mt2$ syntax in Rstudio, and Rstudio will autocomplete,
#' # then you can backspace delete the "mt2$"
#' # if you like
#' mt2 <- use_name_labs(mt2)
#' lm(`Miles/(US) gallon` ~ `Number of cylinders`, data = mt2)
#' lm(mpg ~ cyl, data = uvn(mt2))
#'
#' # let's turn them back off
#' mt2 <- uvn(mt2) # uvn() as "undo" of use_name_labs()
#'
#' # back to our previous variable names
#' head(mt2)
#' # even with name labels "off," mt2 retains labelr attribute meta-data
#' # ...which we can strip away using strip_labs()
#' identical(strip_labs(mt2), mtcars) # and we're back
uvn <- function(data, vars = NULL) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  labs_pres <- check_labs_att(data, "name.labs")
  if (!is.null(vars)) {
    names_data <- gremlr(vars, names(data), vals = TRUE)
  } else {
    names_data <- names(data)
  }
  if (labs_pres) {
    vldf <- get_name_labs(data)
    for (i in seq_along(names_data)) {
      this_name <- names_data[i]
      if (this_name %in% vldf$lab) {
        names(data)[names(data) %in% this_name] <- vldf$var[vldf$lab == this_name]
      }
    }
  }
  return(data)
}
