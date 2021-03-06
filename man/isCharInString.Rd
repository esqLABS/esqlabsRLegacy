% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{isCharInString}
\alias{isCharInString}
\title{Is a character part of string?}
\usage{
isCharInString(char, string)
}
\arguments{
\item{char}{Character to find in the string}

\item{string}{String that should contain the character}
}
\value{
TRUE if the \code{character} is a substring if \code{string}, FALSE otherwise
}
\description{
Is a character part of string?
}
\examples{
isCharInString("a", "bsdalk")
}
