#' Overlay Variable Name Labels Onto Arbitrary R Function Calls
#'
#' @description
#' `with_name_labs` instructs R function calls to swap in variable name labels
#' for column names in the objects they return or side effects they produce.
#'
#' @details
#' Note: `wnl` is a compact alias for `with_name_labs`: they do the same thing,
#' and the former is easier to type
#'
#' `with_name_labs` is intended for interactive use. With it, you pass a
#' name-labeled data.frame, followed by a comma, followed by an unquoted R
#' expression (function call) to be evaluated within that data.frame, and name
#' labels will be substituted for column names in any returned object or side
#' effects. Your function call (expression) should refer to columns of the
#' data.frame, NOT their name labels, as the intent is to allow you to pass
#' functions in terms of the (typically much more concise and familiar) column
#' names while having the results displayed / presented in terms of the more
#' informative (but more verbose and typically non-standard) name labels.
#' See examples.
#'
#' Caution: `with_name_labs` is a rudimentary function that leverages basic,
#' fairly literal (and potentially brittle!) regular expressions and
#' `eval(parse(text=))` to substitute name labels for variable names behind the
#' scenes. It appears to be robust to a range of common commands and operators
#' (e.g., ~ , * , +, :, ::, =, and |). However, it is intended strictly as a
#' convenience for relatively simple, interactive, single-line-expression use
#' cases, involving data exploration, description, or simple model-fitting. It
#' is expressly NOT intended for: (1) multi-step workflows or pipes, (2)
#' expressions that require or make  reference to objects existing outside the
#' supplied data.frame, or (3) data management operations aimed at modifying
#' (e.g., subsetting, merging, renaming) -- as opposed to merely describing or
#' analyzing -- the supplied data.frame. Again, see the examples for the types
#' of expressions/use cases envisioned.
#'
#' @param data a data.frame with name labels.
#' @param ... an additional expression passed to dots (quotes and dollar signs
#' are not needed or permitted).
#' @export
#'
#' @examples
#' # assign mtcars to new data.frame mt2
#' mt2 <- mtcars
#'
#' # add name labs
#' mt2 <- add_name_labs(mt2,
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
#'
#' with_name_labs(mt2, t.test(mpg ~ am))
#' with_name_labs(mt2, lm(mpg ~ am))
#' with_name_labs(mt2, summary(mt2))
#' with_name_labs(mt2, cor(mt2, use = "pairwise.complete.obs"))
#' with_name_labs(mt2, xtabs(~gear))
#' xtabs(~ mt2$gear)
#' with_name_labs(mt2, cor(mpg, carb))
#' with_name_labs(mt2, hist(mpg))
#' with_name_labs(mt2, plot(mpg, carb))
#' with_name_labs(mt2, head(gear))
#' with_name_labs(mt2, summary(mt2))
with_name_labs <- function(data, ...) {
  tryCatch(
    {
      # data.frame name
      data_name <- deparse(substitute(data))

      # make this a Base R data.frame
      data <- as_base_data_frame(data)

      # capture dots as character
      call_as_char <- as.character((as.list(substitute(...()))))

      # compress
      call_as_char <- gsub(" ", "", call_as_char)

      problem_1 <- grepl("\\$", call_as_char)
      problem_2 <- grepl("!!", call_as_char)
      problem_3 <- grepl("\\.\\(", call_as_char)
      problem_4 <- grepl("with", call_as_char)
      problem_5 <- grepl("(with|merg|bind|subset)", call_as_char)

      # error checking
      if (any(c(
        problem_1, problem_2, problem_3, problem_4,
        problem_5
      ))) {
        stop("ERROR")
      }

      # detect any invalid data arg elements in a "data=[your object here])" construction
      this_match <- regmatches(call_as_char, regexpr("data=\\S+\\)", call_as_char))

      if (length(this_match) > 0) {
        if (this_match != paste0("data=", data_name, ")")) {
          stop("ERROR")
        }
      }

      # a data.frame cannot have a column of the same name
      if (any(names(data) == data_name)) {
        stop("ERROR")
      }

      # data.frames cannot have the name "data"
      if (data_name == "data") {
        stop("ERROR")
      }

      if (!check_labs_att(data, "name.labs")) {
        stop("ERROR")
      }

      call_as_char <- gsub(
        "([\\&\\+()\\^\\:\\*\\!\\)\\|\\$\\~\\,\\=\\-\\/])",
        " \\1 ", call_as_char
      )

      call_as_char <- gsub(" \\(", "\\(", call_as_char)
      call_as_char <- gsub(" \\. ", "\\.", call_as_char)
      call_as_char <- gsub(" _ ", "_", call_as_char)
      call_as_char <- gsub(" :  : ", "::", call_as_char)

      for (i in c(0, 1:9)) {
        call_as_char <- gsub(paste0(" ", i, " "), i, call_as_char)
      }

      # get info to substitute variable name labels for variable names
      data <- as.data.frame(data)
      vars2names <- get_name_labs(data)

      for (i in seq_along(vars2names$var)) {
        this_var <- vars2names$var[i]
        this_var <- paste0(" ", this_var, " ")
        this_lab <- vars2names$lab[i]
        this_lab <- paste0("`", this_lab, "`")
        call_as_char <- gsub(this_var, this_lab, call_as_char)
      }

      call_as_char <- gsub(" )", ")", call_as_char)
      call_as_char <- gsub(data_name, "data", call_as_char)

      data <- use_name_labs(data)

      evout <- eval(parse(text = call_as_char), envir = data)

      if (is.atomic(evout) & length(evout) == 1) {
        if (is.na(evout) && any(check_irregular(data))) {
          warning("WARNING")
        }
      }

      result <- evout
      return(result)
    },
    error = function(e) {
      message('
Error: Invalid argument or syntax passed to with_name_labs(). \n

with_name_labs() is for simple, one-step descriptive/exploratory data analysis
calls, not multi-step workflows or data management / modification efforts. See
examples and documentation for guidance and consider double-checking your syntax
using a standard R call. \n

Errors may result from calls featuring:
*dollar signs [omit these],
*quoted column names [leave unquoted],
*data management operations, e.g., filtering, merging [not recommended],
*non-standard syntax/operators from external packages [not supported],
*pipe operators, base R (|>) or other (e.g., %>%) [not supported],
*references to objects outside supplied data.frame [not supported],
*Lack of NA-supporting arguments [e.g., lack of \", na.rm = TRUE\"],
*use of " ~ .)" in a formula [add a data argument after " ~ ."],
*referencing a column w/ same name as data.frame itself [not supported],
*attempts to pass data.frames named simply "data" [not supported].\n')
      return(NA)
    },
    warning = function(w) {
      message('
One or more warnings were generated by your call. If returned value is NA, check
data.frame for NA values and provide appropriate arguments (e.g.,\"na.rm = TRUE\").

Also consider double-checking your results using a standard R call (i.e., one that
does not use with_name_labs())\n')
      return(NA)
    }
  )
}
