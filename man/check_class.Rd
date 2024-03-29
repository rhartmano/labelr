% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_class.R
\name{check_class}
\alias{check_class}
\title{Determine If Vector Belongs to Any of Specified Classes}
\usage{
check_class(
  x,
  classes = c("numeric", "integer", "logical", "character", "factor", "ordered"),
  strict = TRUE
)
}
\arguments{
\item{x}{the vector to check against specified classes.}

\item{classes}{a character vector of classes against which x is checked.}

\item{strict}{If TRUE, all of x's classes must be among those specified
in the classes argument. If FALSE, at least one but not necessarily all of
x's classes must be among those specified in the classes argument.}
}
\value{
a 1L logical vector indicating whether x's class is found among those
passed to the classes argument.
}
\description{
\code{check_class} determines whether a vector's class is among those specified.
}
\details{
By default (strict = TRUE), if a vector is of multiple classes, all of its
classes must be among those specified via the classes argument.
}
\examples{
check_class(mtcars$mpg) # TRUE
check_class(mtcars$mpg, classes = c("numeric", "factor")) # TRUE
check_class(iris$Species) # TRUE
check_class(iris$Species, classes = c("logical", "numeric")) # FALSE
check_class(mtcars$mpg, classes = c("logical", "character", "factor")) # FALSE
}
