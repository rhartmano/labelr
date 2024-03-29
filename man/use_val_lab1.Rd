% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_val_lab1.R
\name{use_val_lab1}
\alias{use_val_lab1}
\alias{uvl1}
\title{Replace a Single Data Frame Column's Values with Its Value Labels}
\usage{
use_val_lab1(data, var)

uvl1(data, var)
}
\arguments{
\item{data}{the data.frame.}

\item{var}{the unquoted name of the column (variable) whose values you wish
to replace with the corresponding value labels.}
}
\value{
A data.frame consisting of the originally supplied data.frame, with
the var argument variable's values replaced with its value labels.
}
\description{
For a single value-labeled column of a data.frame, replace
all of its values with the corresponding value labels and return the modified
data.frame.
}
\details{
Note 1: \code{use_val_lab1} is a variant of \code{use_val_labs} that allows you to
specify only one variable at a time but that allows you to pass its name
without quoting it (compare use_val_lab1(mtcars, am) to
use_val_labs(mtcars, "am")).

Note 2: \code{uvl1} is a compact alias for \code{use_val_lab1}: they do the same thing,
and the former is easier to type.

Note 3: This command is intended exclusively for interactive use. In
particular, the var argument must be the literal name of a single variable
(column) found in the supplied data.frame and may NOT be, e.g., the name of a
character vector that contains the variable (column name) of interest. If you
wish to supply a character vector with the names of variables (columns) of
interest, use \code{use_val_labs()}.

\code{use_val_lab1} replaces a single, value-labeled data.frame column with a
"value labels-on" version of that column. Here, "labels-on" means that the
column's original values are replaced with the corresponding value labels.
Note that the modified column is a simple, self-contained character variable
that cannot itself be converted or reverted back to the original ("labels-off")
values of its parent/source column.
}
\examples{
# swap in "am" value labels for values in mtcars
df <- mtcars # copy of mtcars

# now, add value labels
df <- add_val1(
  data = df,
  var = am,
  vals = c(0, 1),
  labs = c("automatic", "manual")
)

# switch out "am" values for value labels, assign to df_plus
df_plus <- use_val_lab1(df, am)
head(df_plus[c("am")])
}
