% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row_sums.R
\name{row_sums}
\alias{row_sums}
\alias{row_means}
\alias{row_min}
\alias{row_max}
\alias{row_apply}
\title{Apply simple rowwise functions}
\usage{
row_sums(data, ..., na_rm = FALSE, varname = "sum")

row_means(data, ..., na_rm = FALSE, varname = "mean")

row_min(data, ..., na_rm = FALSE, varname = "min")

row_max(data, ..., na_rm = FALSE, varname = "max")

row_apply(data, ..., .fun, varname = "var")
}
\arguments{
\item{data}{A data frame.}

\item{...}{The columns to sum, take the mean of, etc. \emph{Required}.}

\item{na_rm}{Whether to remove \code{NA} values or not. Default is \code{FALSE}.}

\item{varname}{The column name of the sums means etc. as a character string.}

\item{.fun}{The function to apply.}
}
\value{
A data frame with a new column representing the sums, means or
  whatever.
}
\description{
Get means, sums and possibly other functions for a select number
  of columns using \code{dplyr::select}-style column selection.
}
\details{
Simple wrappers for applying rowwise operations only for selected
  columns within the tidyverse approach to data processing.  The
  \code{row_apply} function requires a function that already works on rows.
}
\examples{
library(tidyext)
library(dplyr)

d = data.frame(x = 1:3, y = 4:6, z = 7:9)

d  \%>\%
  row_sums(x:y)

d  \%>\%
  row_means(matches('x|z'))

d  \%>\%
  row_max(matches('x|y'))

row_apply(
  d ,
  everything(),
  .fun = function(x)
    apply(x, 1, paste, collapse = '')
)

}
