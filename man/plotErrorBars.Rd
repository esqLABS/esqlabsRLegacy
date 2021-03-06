% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-figures.R
\name{plotErrorBars}
\alias{plotErrorBars}
\title{A function to add error bars on the chart.}
\usage{
plotErrorBars(
  x,
  y,
  upper,
  lower = upper,
  length = par()$cin[[1]]/2,
  axis = "y",
  ...
)
}
\arguments{
\item{x}{Numerical array of x-values}

\item{y}{Numerical array of y-values}

\item{upper}{Numerical array of upper y-error values}

\item{lower}{Numerical array of lower y-error values. Optional.
If not specified, same values as for \code{upper} are used}

\item{length}{Numerical value specifying the width of the error bars.
Optional. Default is 0.1.}

\item{axis}{Dimension to which the error bars are added. If "y" (default),
vertical error bars are drawn. If "x", horizontal error bars are drawn}

\item{...}{Graphical parameters (see \code{\link[=par]{par()}})}
}
\description{
Taken from \url{https://www.r-graph-gallery.com/4-barplot-with-error-bar.html}
}
\keyword{internal}
