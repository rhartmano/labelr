% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis_lab.R
\name{axis_lab}
\alias{axis_lab}
\alias{alb}
\title{Retrieve Variable's Name Label for Plot Labeling}
\usage{
axis_lab(data, var)

alb(data, var)
}
\arguments{
\item{data}{a data.frame.}

\item{var}{the unquoted name of a variable that exists in the data.frame and
is name-labeled (using \code{add_name_labs()}).}
}
\value{
a 1L character vector with var's name label.
}
\description{
\code{axis_lab} accepts a data.frame and single unquoted variable name and returns
that variable's name label for use in axis labeling or plot labeling function
options.
}
\details{
Note: \code{alb} is a compact alias for \code{axis_lab}: they do the same thing,
and the former is easier to type.
}
\examples{
# copy mtcars to df
# create a data set
df <- mtcars

# variable names and their labels
names_labs_vec <- c(
  "mpg" = "Miles/(US) gallon",
  "cyl" = "Number of cylinders",
  "wt" = "Weight (1000 lbs)"
)

df <- add_name_labs(df, name.labs = names_labs_vec)

# ggplot example of axis_lab()
library(ggplot2)
p <- ggplot(df, aes(mpg, wt, color = cyl)) +
  geom_point()
p <- p +
  labs(color = axis_lab(df, cyl)) +
  xlab(axis_lab(df, mpg)) +
  ylab(axis_lab(df, wt))

# Base R plot example (using alb() alias)
with(df, plot(mpg, wt,
  xlab = alb(df, mpg),
  ylab = alb(df, wt)
))

}
