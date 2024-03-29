% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/has_m1_labs.R
\name{has_m1_labs}
\alias{has_m1_labs}
\alias{hm1l}
\title{Is This an \code{add_m1_lab()} Many-to-One-Style Value-labeled Variable (Column)?}
\usage{
has_m1_labs(data, var)

hm1l(data, var)
}
\arguments{
\item{data}{a data.frame.}

\item{var}{the unquoted name of the variable (column) to check for the
presence of many-to-one-style value labels.}
}
\value{
A 1L logical.
}
\description{
Determine whether a specific variable of a data.frame has many-to-one-style
value labels associated with it (i.e., via \code{add_m1_lab()} or \code{add1m1()}).
}
\details{
\code{hm1l} is a compact alias for \code{has_m1_labs}: they do the same thing, and the
former is easier to type
}
\examples{
# add many-to-one style labels for "carb" and one-to-one style for "am"
df <- mtcars

df <- add_m1_lab(df,
  vars = "carb",
  vals = 1:3,
  lab = "<=3",
  max.unique.vals = 10
)

df <- add_m1_lab(df,
  vars = "carb",
  vals = c(4, 6, 8),
  lab = ">=4",
  max.unique.vals = 10
)

df <- add_val_labs(df,
  vars = "am",
  vals = c(0, 1),
  labs = c("autom", "manu"),
  max.unique.vals = 10
)

has_m1_labs(df, carb) # TRUE, carb has m1-style value labels

has_val_labs(df, am) # TRUE, am does have value labels

has_m1_labs(df, am) # FALSE, am's value labels are not not m1-style labels
}
