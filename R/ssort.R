#' Safely Sort (Re-order) a Labeled Data Frame
#'
#' @description
#' `ssort` allows one to sort (after the fashion of base::order or
#' dplyr::arrange) the rows of a data.frame based on column values.
#'
#' @details
#' This function accepts a data.frame, followed by a quoted vector of column
#' names (or an integer vector of column position indices), followed by
#' an indication of which are to be sorted ascending (default) or descending.
#' If multiple columns are supplied to vars, sorting prioritizes the columns
#' that appear earlier, with values of subsequent columns being sorted within
#' distinct values of earlier columns. Note: `ssort` is fast enough on small
#' data.frames and very slow on "larger" (>500K records) data.frames,
#' particularly for more complex or demanding sort requests. Other R packages
#' may provide faster sorting while preserving labelr attributes.
#'
#' @param data the data.frame to be sorted.
#' @param vars the variables to be sorted on, specified as a quoted character
#' vector of variable names or an integer vector of column position indices.
#' @param descending whether to sort the given variable of vars in descending
#' or ascending order. Default is FALSE, which will be recycled to all vars
#' arguments.
#' @param na.last force NA values to appear last in a variable's sort order if
#' TRUE (default).
#' @param fact.to.char coerce all factor variables to character variables.
#' @return a labelr label attribute-preserving data.frame consisting of the
#' re-sorted data.frame.
#'
#' @export
#' @examples
#' # make toy demographic (gender, raceth, etc.) data set
#' set.seed(555)
#' df <- make_demo_data(n = 1000) # another labelr:: function
#'
#' # let's add variable VALUE labels for variable "raceth"
#' df <- add_val_labs(df,
#'   vars = "raceth", vals = c(1:7),
#'   labs = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multi", "Other"),
#'   max.unique.vals = 50
#' )
#'
#' head(df, 3)
#' check_labs_att(df, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsort1 <- ssort(df, c("raceth", "gender", "age"), descending = c(TRUE, FALSE, FALSE))
#'
#' head(dfsort1, 20)
#'
#' check_labs_att(dfsort1, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsort2 <- ssort(df, c("age", "gender"))
#'
#' head(dfsort2, 20)
#'
#' check_labs_att(dfsort2, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
#' dfsort3 <- ssort(df, c("raceth"))
#'
#' head(dfsort3, 10)
#'
#' check_labs_att(dfsort3, "val.labs.raceth") # "raceth" lab specifically TRUE
#'
ssort <- function(data, vars, descending = FALSE, na.last =
                    TRUE, fact.to.char = TRUE) {
  ##############################################################################
  # begin supporting functions -
  # add_leading0, rank_code, rep_paste, rebaseline_rc, replace_with,
  # safe_char_val, classes, fact2char, na_row
  ##############################################################################
  # paste a leading character - for number sorting
  ##############################################################################
  add_leading0 <- function(x) {
    # prepend variable-specific numbers of a leading character
    rep_paste <- function(x, reps, bindchar = "-") {
      z <- mapply(rep, x, reps)
      qq <- lapply(z, paste, collapse = bindchar)
      ff <- do.call("rbind", qq)
      x <- as.vector(unlist(ff))
      return(x)
    }

    x <- as.character(x)
    char_x <- nchar(x)
    max_char_x <- max(char_x)
    chars_needed_x <- max_char_x - char_x + 1
    x_0 <- rep_paste("0", chars_needed_x, bindchar = "")

    new_x <- paste0(x_0, x)
    return(new_x)
  }

  # rank() wrapper that uses ties.method="average" and na.last=TRUE
  # requires rebaseline_rc() and replace_with()
  # to give integer ranks with ties allowed,
  # ...no decimals, all ranks incrementing by one integer
  # ...except for ties, which share the same integer, and with
  # ...all integers min-max represented (no gaps)
  # Eliminates decimal pointed ranks for ties
  # so c(1.5,1.5,3) ranks become ranks c(1,1,2)
  # ascending by default (else descending=TRUE)
  # with na.leave=TRUE, values that are NA in x will be
  # returned as NA in output, else they'll be given largest rank,
  # regardless of whether ascending or descending
  # ...(i.e., the na.last=TRUE treatment)
  #####################################################
  rank_code <- function(x, descending = FALSE, na.leave = FALSE) {
    ############################################################################
    # rebaseline_rc() - to be used with rank_code()
    ############################################################################
    # rebaseline a rank variable to
    # ...ensure ranks start at 1 and move incrementally by integers of 1
    # NAs will either be the largest ranks or will be left as na (if na.leave=TRUE)
    ############################################################################
    rebaseline_rc <- function(x, na.leave = TRUE) {
      x_new <- x - 1
      x_new_max <- max(x_new, na.rm = TRUE)
      x_new[is.na(x_new)] <- x_new_max + 1
      if (min(x_new) == 0) x_new <- x_new + 1
      if (na.leave) x_new[is.na(x)] <- NA
      return(x_new)
    }

    ############################################################################
    # replace_with() - replace one set of values with another
    # for use with rank_code() to give integer ranks with ties
    # ...allowed, no decimals, all ranks incrementing by one integer
    # except for ties, which share the same integer, and with
    # all integers min-max represented (no gaps)
    ############################################################################
    replace_with <- function(from, to, x) {
      codes_df <- data.frame(from, to)
      new_x <- rep(NA, length(x))
      for (i in seq_len(nrow(codes_df))) {
        from_i <- codes_df[i, 1]
        to_i <- codes_df[i, 2]
        new_x[x == from_i] <- to_i
      }
      new_x
    }

    ranks_x <- rank(x, ties.method = "average", na.last = TRUE)
    uni_ranks <- unique(ranks_x)
    uni_new <- seq_len(length(uni_ranks))
    sort_ranks <- sort(uni_ranks)
    revsort_ranks <- rev(sort(uni_ranks))
    if (descending) {
      new_x <- replace_with(revsort_ranks, uni_new, ranks_x)
    } else {
      new_x <- replace_with(sort_ranks, uni_new, ranks_x)
    }

    if (na.leave) {
      new_x[is.na(x)] <- NA
      new_x <- rebaseline_rc(new_x, na.leave = TRUE)
    }
    new_x
  }

  #############################################################################
  # safe_char_val() - search for presence of character stub in x
  # ...and identify the first variant of stub that is not already in x
  # "variant of stub" means the leading stub characters with numbers affixed
  # ..afterward
  #############################################################################
  safe_char_val <- function(x, stub) {
    if (is.data.frame(x)) {
      x <- as.data.frame(x)
      x <- names(x)
    }
    if (!stub %in% x) {
      the_name <- stub
    } else {
      the_name <- NULL
      found_it <- FALSE
      count <- 0
      while (!found_it) {
        count <- count + 1
        the_name <- paste0(stub, "_", count)
        if (!the_name %in% x) found_it <- TRUE
      }
    }

    return(the_name)
  }

  ############################################################################
  # get classes of all elements in a data.frame
  # ...return as vector
  ############################################################################
  classes <- function(data) {
    if (!is.data.frame(data)) stop("classes_data() call only takes data.frames")
    data_classes <- sapply(data, class)
    return(data_classes)
  }

  #############################################################################
  # Change all factors in a data frame to characters#
  #############################################################################
  fact2char <- function(data) {
    data <- as.data.frame(data)
    i <- sapply(data, is.factor)
    data[i] <- lapply(data[i], as.character)

    return(data)
  }

  ##############################################################################
  # na_row() - determine if there are any missing values, per row
  # var.only to return only a logical indicator of whether row has any NA
  # ...vals -- TRUE if a value is missing
  # drop=TRUE to remove offending n
  ##############################################################################
  na_row <- function(data, var.only = FALSE, drop = FALSE) {
    data <- as.data.frame(data)
    data_orig <- data
    # work around if only one column is supplied to dots
    if (ncol(data) == 1) {
      name2 <- paste0(names(data), "_z")
      data2 <- data
      names(data2) <- name2
      data <- cbind_x(data, data2)
    }

    data <- t2(apply(data, 1, function(x) is.na(x)))
    data$na_count <- apply(data, 1, function(x) sum(x))
    data$na_count[data$na_count > 0] <- 1
    data$na_count <- as.logical(data$na_count)
    data$na_any <- data$na_count
    data$na_count <- NULL
    data_orig$na_any <- data$na_any

    if (var.only) data_orig <- data$na_any

    # ignore drop=TRUE if var.only is TRUE
    if (!var.only && drop) {
      data_orig <- data_orig[data_orig$na_any == FALSE, ]
      data_orig$na_any <- NULL
      data_orig <- as.data.frame(data_orig)
    }

    return(data_orig)
  }

  ##############################################################################
  # cbind_x() cbind objects, convert to data.frame, coerce factors
  ##############################################################################
  cbind_x <- function(...) {
    df <- cbind(...)
    df <- as.data.frame(df)
    df <- fact2char(df)
    df <- as.data.frame(df)
    return(df)
  }

  ##############################################################################
  # transpose into a data.frame
  # will return a data.frame whether R4 or pre-R4
  ##############################################################################
  t2 <- function(x) {
    x <- data.matrix(x)
    x <- t(x)
    x <- as.data.frame(x)
    return(x)
  }

  ##############################################################################
  # function to coerce all "irregular" values to "NA"
  ##############################################################################
  irreg2na_all <- function(data, vars = NULL) {
    data <- as.data.frame(data)
    if (is.null(vars)) vars <- names(data)
    irreg2na_vec <- function(x) {
      x <- as.character(x)
      xu <- toupper(x)
      x[xu == "NA"] <- "NA"
      x[xu == "NAN"] <- "NA"
      x[xu == "-INF"] <- "NA"
      x[xu == "INF"] <- "NA"
      x[is.na(xu)] <- "NA"
      return(x)
    }
    data <- lapply(data, irreg2na_vec)
    data <- do.call("cbind", data)
    data <- as.data.frame(data)
    return(data)
  }

  ####################################################################
  # end supporting functions, begin primary sort_df operations     #
  ####################################################################

  # warn user that this function is not for large data.frames
  # (it may still work without crashing, but it will be slow)
  if (nrow(data) > 50000) {
    warning("
\nNote: ssort() is not optimized for data.frames of this size or larger. This may take awhile.")
  }

  # handle var column number indices, if supplied
  if (is.numeric(vars)) vars <- names(data)[vars]

  # coerce/create data.frames
  data_orig <- data <- as_base_data_frame(data)

  # capture labelr attributes, so, they can be re-associated at the end
  these_atts <- get_all_lab_atts(data)

  if (fact.to.char) {
    data_orig <- fact2char(data_orig)
    if (any(classes(data_orig) == "factor")) {
      warning("Factor variables converted to character (before sort).")
    }
  }

  if (length(descending) == 1 && length(vars) > 1) {
    descending <- rep(descending, length(vars))
    warning("Note: Only one descending argument found; it has been applied (recycled) to all vars arguments.")
  }

  if (length(descending) != length(vars)) stop("Number of args to vars and descending do not match.")

  if (length(vars) == 1) {
    sort.ord2 <- order(data_orig[, vars], decreasing = descending, na.last = na.last)
    data <- data_orig[sort.ord2, ]
  } else {
    data <- data_orig
    data <- data[, vars, drop = FALSE]
    data_i <- data
    data_i[!is.na(data_i)] <- NA
    for (i in seq(length(vars))) {
      name_i <- names(data)[i]
      descend_i <- descending[i]
      var_i <- data[, name_i, drop = TRUE]
      sort.ord2 <- rank_code(var_i, descending = descend_i, na.leave = FALSE)
      sort.ord2 <- add_leading0(sort.ord2)
      data_i[[name_i]] <- sort.ord2
    }

    smoosh_val <- as.data.frame(data_i)
    smoosh_val <- apply(smoosh_val, 1, paste, collapse = "-")
    smoosh_val <- as.data.frame(smoosh_val)
    na_df <- na_row(data_orig[, vars], var.only = TRUE)
    smoosh_val[na_df, ] <- NA
    nm <- safe_char_val(names(data_orig), "sort.ord")
    data <- cbind_x(smoosh_val, data_orig)
    names(data)[1] <- nm
    sort.ord2 <- order(data[, nm], na.last = na.last)
    data <- data_orig[sort.ord2, ]
    data[[nm]] <- NULL
  }

  # convert NaN values to NA
  if (any(sapply(data, function(x) any(is.nan(x))))) {
    nan_vars_to_fix <- names(data)[sapply(
      data,
      function(x) any(is.nan(x))
    )]

    for (i in seq_along(nan_vars_to_fix)) {
      this_var <- nan_vars_to_fix[i]
      x <- data[[this_var]]
      x[is.nan(x)] <- NA
      data[[this_var]] <- x
    }
    warning("
\nNaN values converted to NA values.")
  }

  # re-associate labelr attributes
  data <- add_lab_atts(data, these_atts, num.convert = FALSE)
  return(data)
}
