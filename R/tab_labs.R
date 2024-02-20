#' Generate a Value Labels Frequency Table with Ad Hoc Numeric Coercion
#'
#' @description
#' `tab_labs` is largely a wrapper to `labelr::tabl`, with the labs.on argument
#' hard-coded to TRUE.
#'
#' @details
#' `tab_labs` calculates raw or weighted frequency counts (or proportions)
#' over an arbitrary number of variables, with tabulations calculated and
#' expressed in terms of value labels for any value-labeled variables, and with
#' various options available.
#'
#' Note: Unlike `tabl`, `tab_labs` forces any value-labeled variable tabulations
#' to be expressed in terms of value labels, not raw values themselves.
#'
#' @param data a data.frame.
#' @param vars a quoted character vector of variable names of categorical (to
#' include integer) variables you wish to include in the table. If left NULL and
#' labs.on = FALSE,`tab_labs` will attempt to construct a table over all combinations
#' of all variables in the data.frame that do not exceed your max.unique.vals
#' threshold.
#' @param qtiles the number of quantile categories to employ in auto-labeling
#' numeric columns that exceed the max.unique.vals threshold. Note: Numeric
#' variables that already have value labels as part of the supplied data.frame
#' will not be relabeled (i.e., the pre-existing value labels will be used).
#' @param wt an optional vector that includes cell counts or some other
#' idiosyncratic "importance" weight. If NULL, no weighting will be employed.
#' @param prop.digits if non-NULL, cell percentages (proportions) will be
#' returned instead of counts, and these will be rounded to the digit specified
#' (e.g., prop.digits = 3 means a value of 0.157 would be returned for a cell
#' that accounted for 8 observations if the total number of observations were
#' 51).
#' @param div.by Divide the returned counts by a constant for scaling purposes.
#' This may be a number (e.g., div.by = 10 to divide by 10) or a character that
#' follows the convention "number followed by 'K', 'M', or 'B'", where, e.g.,
#' "10K" is translated as 10000, "1B" is translated as 1000000000, etc.
#' @param max.unique.vals Integer to specify the maximum number of unique values
#' of a variable that may be observed for that variable to be included in
#' tabulations. Note that labelr sets a hard ceiling of 5000 on the total number
#' of unique value labels that any variable is permitted to have under any
#' circumstance, as labelr is primarily intended for interactive use with
#' moderately-sized (<=~1M-row) data.frames.
#' @param sort.freq By default, returned table rows are sorted ascending by
#' distinct values of vars (in the order vars are specified). If TRUE, the table
#' will instead be sorted in descending order of cell frequency (most frequent
#' categories/combinations first).
#' @param zero.rm If TRUE, zero-frequency vars categories/combinations (i.e.,
#' those not observed in the data.frame) will be filtered from the table.
#' @param irreg.rm If TRUE, tabulations exclude cases where any applicable
#' variable (see vars argument) features any of the following "irregular"
#' values: NA, NaN, Inf, -Inf, or any non-case-sensitive variation on "NA",
#' "NAN", "INF", or "-INF." If FALSE, all "irregular" values (as just defined)
#' are assigned to a "catch-all" category of NA that is featured in the
#' returned table (if/where present).
#' @param wide.col If non-NULL, this is the quoted name of a single column / var
#' of supplied data.frame whose distinct values you wish to be columns of the
#' returned table. For example, if you are interested in a cross-tab of
#' "edu" (highest level of education) and "race" (a race/ethnicity variable),
#' you could supply vars= c("edu", "race") and wide.col = "race", and the
#' different racial-ethnic group categories would appear as distinct columns,
#' with "edu" category levels appearing as distinct rows, and cell values
#' representing the cross-tabbed cell frequencies (perhaps easier demonstrated
#' than described: see examples). You may supply at most one wide.col.
#'
#' @return a data.frame.
#' @importFrom stats reshape aggregate
#' @export
#'
#' @examples
#' # assign mtcars data.frame to df data.frame
#' df <- mtcars
#'
#' # add value labels
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
#' tab_labs(df, vars = c("am", "mpg", "disp", "carb"), qtiles = 3) # runs quickly
#'
#' # show how tab_labs() behaves with a non-value-labeled data.frame
#' tab_labs(iris)
#'
#' tab_labs(mtcars, vars = c("am", "mpg", "disp", "gear", "cyl"))
tab_labs <- function(data,
                     vars = NULL,
                     qtiles = 4,
                     wt = NULL,
                     prop.digits = NULL,
                     div.by = NULL,
                     max.unique.vals = 10,
                     sort.freq = TRUE,
                     zero.rm = FALSE,
                     irreg.rm = FALSE,
                     wide.col = NULL) {
  # make this a Base R data.frame
  data <- as_base_data_frame(data)

  # get nrow
  nrow_data <- nrow(data)

  # grab weights value if present
  if (!is.null(wt)) {
    if (!wt %in% names(data)) {
      stop("
wt arg must be a colname of supplied data.frame. No such column found.")
    }

    wts <- data[[wt]]

    # check weights variable (numeric)
    if (!is.numeric(wts)) {
      stop("
wt argument must be a numeric variable.")
    }

    # check weights variable (irregular)
    if (any(unname(check_irregular(wts)))) {
      stop("
wt variable may not include NA or other irregular (e.g., NaN) values.")
    }

    # check weights values (all >=0)
    if (any(wts < 0)) {
      stop("
All weights must be numeric values >=0.")
    }
  }

  # make sure only one wide.col supplied
  if (!is.null(wide.col)) {
    if (length(wide.col) != 1) {
      stop("
You may not specify more than one wide.col.")
    }
  }

  # safely drop out of scope columns
  if (!is.null(vars) && !is.null(wide.col)) {
    vars <- unique(c(vars, wide.col))
  } else if (!is.null(vars) && is.null(wide.col)) {
    vars <- vars
  } else {
    vars <- names(data)
  }

  # make sure all selected vars are found in data.frame
  if (any(!vars %in% names(data))) {
    stop("
At least one colname arg to vars or wide.col not found in supplied data.frame.")
  } else {
    data <- sbrac(data, , vars) # subset, preserving labels
  }

  # check for prohibited variable names (those w/ @ in name)

  if (any(grepl("@", names(data)))) {
    stop("
A variable name contains the \"@\" character, which is not permitted.")
  }

  # check max vals - 5000 unique value labels for a variable is a hard cap:
  # Under no circumstances can a variable with 5000 distinct values receive value
  # ...labels
  if (max.unique.vals > 5000) {
    stop("
    \n max.unique.vals may not exceed 5000.")
  }

  # add quantile labels if specified
  if (!is.null(qtiles)) {
    data <- all_quant_labs(data,
      qtiles = qtiles,
      unique.vals.thresh = max.unique.vals
    )
  }

  # turn on value labels
  data <- use_val_labs(data)

  # drop vars with decimal points or too many unique values
  num_vars_to_drop <- sapply(
    data,
    function(x) {
      length(unique(x)) > max.unique.vals |
        has_decv(x)
    }
  )

  if (any(num_vars_to_drop)) {
    names_to_drop <- names(num_vars_to_drop)[unname(num_vars_to_drop)]
    for (i in seq_along(names_to_drop)) {
      this_name <- names_to_drop[i]
      warning(sprintf("
Excluding variable --%s-- (includes decimals or exceeds max.unique.vals).\n", this_name))
    }
    data <- data[!num_vars_to_drop]
    data <- as.data.frame(data)
    vars <- names(data)
  }

  # combinations
  combos <- prod(sapply(data, function(x) length(unique(x, na.rm = TRUE))))

  # zero.rm
  if (combos > 10000 && !zero.rm) {
    zero.rm <- TRUE
    warning("
Requested table would be >10000 rows. Excluding zero-frequency (unobserved) combinations")
  }

  # find a safe name to use (one not already in vars)
  if (!"vars" %in% vars) {
    the_name <- "vars"
  } else {
    the_name <- NULL
    found_it <- FALSE
    count <- 0
    while (!found_it) {
      count <- count + 1
      the_name <- paste0("vars", "_", count)
      if (!the_name %in% vars) found_it <- TRUE
    }
  }

  # convert factors to character
  data <- as.data.frame(data)
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)

  # convert irregular values to "NA" function over all remaining variables
  data <- lapply(data, irregular2v, to = "NA")
  data <- do.call("cbind", data)
  data <- as.data.frame(data)

  # do weighted counts if wt arg is not NULL (see sapply() call below w/ sum())
  if (!is.null(wt)) {
    last_col_name <- "n.wtd"
    data <- cbind(data, wts) # restore weights
    data <- as.data.frame(data)
    names(data)[ncol(data)] <- wt
    data_split <- split(data, data[vars], sep = "@")
    freq <- sapply(data_split, function(x) sum(x[[wt]]))
    rm(data_split)
    data2 <- data.frame(names(freq), freq)
    names(data2) <- c(the_name, last_col_name)
    rownames(data2) <- NULL
    vars_split <- lapply(data2[[the_name]], function(v) unlist(strsplit(v, "@")))
    vars_split <- do.call("rbind", vars_split)
    vars_split <- as.data.frame(vars_split)
    names(vars_split) <- vars
    data2 <- cbind(vars_split, data2)
    data2 <- as.data.frame(data2)
    data2[[the_name]] <- NULL

    # do unweighted counts if wt arg is NULL (see sapply() call below w/ nrow())
  } else {
    last_col_name <- "n"
    data_split <- split(data, data[vars], sep = "@")
    freq <- sapply(data_split, function(x) nrow(x))
    rm(data_split)
    data2 <- data.frame(names(freq), freq)
    names(data2) <- c(the_name, last_col_name)
    rownames(data2) <- NULL
    vars_split <- lapply(data2[[the_name]], function(v) unlist(strsplit(v, "@")))
    vars_split <- do.call("rbind", vars_split)
    vars_split <- as.data.frame(vars_split)
    names(vars_split) <- vars
    data2 <- cbind(vars_split, data2)
    data2 <- as.data.frame(data2)
    data2[[the_name]] <- NULL
  }

  # remove irregular values, as requested
  if (irreg.rm) {
    irreg_rows <- unname(which(apply(data2, 1, function(x) any(x == "NA"))))

    if (length(irreg_rows) != 0) data2 <- data2[-c(irreg_rows), ]
  }
  # remove rows with zero counts, as requested
  if (zero.rm) {
    zero_rows <- unname(which(data2[[last_col_name]] == 0))

    if (length(zero_rows) != 0) data2 <- data2[-c(zero_rows), ]
  }

  # sort by frequency counts if that option is TRUE
  # else by var values
  # sort table results
  # sort by frequency counts, then by vars for var combinations that
  # share the same frequency count
  if (sort.freq) {
    sort_vars <- c(last_col_name, vars)
    desc_args <- c(TRUE, rep(FALSE, length(vars)))
    data2 <- suppressWarnings(
      suppressMessages(
        ssort(data2, sort_vars, desc_args)
      )
    )

    # else, just sort by vars
  } else {
    data2 <- suppressWarnings(
      suppressMessages(
        ssort(data2, vars)
      )
    )
  }

  # use percents (proportions) instead of counts if prop.digits is not NULL
  if (!is.null(prop.digits)) {
    name_x <- names(data2)[ncol(data2)]
    data2[[name_x]] <- data2[[name_x]] / sum(data2[[name_x]])
    data2[[name_x]] <- round(data2[[name_x]], digits = prop.digits)
  }

  # allow for dividing totals by some constant, if one is specified
  # and if prop.digits argument is NULL
  if (!is.null(div.by) && is.null(prop.digits)) {
    if (is.character(div.by)) {
      div.by <- sub("B", "000000000", toupper(div.by))
      div.by <- sub("M", "000000", toupper(div.by))
      div.by <- sub("K", "000", toupper(div.by))
      div.by <- as.integer(div.by)
    }

    data2[[ncol(data2)]] <- data2[[ncol(data2)]] / div.by
  }

  rownames(data2) <- 1:nrow(data2)

  # "cast" / "pivot wider" by wide.col, if we've specified one
  if (!is.null(wide.col)) {
    vals.var <- names(data2)[ncol(data2)]
    other.vars <- names(data2)[!names(data2) %in% c(wide.col, vals.var)]

    data2 <- stats::reshape(data2,
      timevar = wide.col,
      idvar = other.vars,
      direction = "wide"
    )
    vals.var <- paste0(vals.var, "\\.")
    names(data2) <- gsub(vals.var, "", names(data2))
    data2 <- as.data.frame(data2)
    data2 <- data2[names(data2)]
    data2 <- as.data.frame(data2)

    # convert NA to 0 (counts) in new pivoted-wider cols
    orig_vars <- base::setdiff(vars, wide.col)
    new_vars <- base::setdiff(names(data2), orig_vars)
    for (i in new_vars) {
      data2[[i]] <- as_numv(data2[[i]])
      data2[is.na(data2[[i]]), i] <- 0
    }
  }

  # restore numeric status to any variables for which this makes sense
  data2 <- as_num(data2)

  # convert irregular values to NA values
  data2 <- lapply(data2, irregular2v, to = NA)
  data2 <- do.call("cbind", data2)
  data2 <- as.data.frame(data2)
  data2[[ncol(data2)]] <- as.numeric(data2[[ncol(data2)]])

  return(data2)
}

#' @export
#' @rdname tab_labs
tabl2 <- tab_labs
