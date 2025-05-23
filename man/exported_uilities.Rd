% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exported_utilities.R
\name{indx_x}
\alias{indx_x}
\alias{indx_wo}
\title{Exported Utilities}
\usage{
indx_x(i, x, xnames, xsize)

indx_wo(i, x, xnames, xsize)
}
\arguments{
\item{i}{See \link{squarebrackets_indx_args}.}

\item{x}{a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.}

\item{xnames}{names or dimension names}

\item{xsize}{length or dimension size}
}
\value{
The subsetted object.
}
\description{
Exported utilities. \cr
Usually the user won't need these functions. \cr \cr
}
\examples{
x <- 1:10
names(x) <- letters[1:10]
indx_x(1:5, x, names(x), length(x))
indx_wo(1:5, x, names(x), length(x))


}
