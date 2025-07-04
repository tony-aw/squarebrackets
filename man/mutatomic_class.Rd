% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutatomic_class.R
\name{mutatomic_class}
\alias{mutatomic_class}
\alias{mutatomic}
\alias{as.mutatomic}
\alias{is.mutatomic}
\alias{couldb.mutatomic}
\title{A Class of Mutable Atomic Objects}
\usage{
mutatomic(data, names = NULL, dim = NULL, dimnames = NULL)

as.mutatomic(x, ...)

is.mutatomic(x)

couldb.mutatomic(x)
}
\arguments{
\item{data}{atomic vector giving data to fill the \code{mutatomic} object.}

\item{names, dim, dimnames}{see \link[stats]{setNames} and \link[base]{array}.}

\item{x}{an atomic object.}

\item{...}{method dependent arguments.}
}
\value{
For \code{mutatomic()}, \code{as.mutatomic()}: \cr
Returns a \code{mutatomic} object. \cr
\cr
For \code{is.mutatomic()}: \cr
Returns \code{TRUE} if the object is \code{mutatomic},
and returns \code{FALSE} otherwise. \cr
\cr
For \code{couldb.mutatomic()}: \cr
Returns \code{TRUE} if the object is one of the following types: \cr
\link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
Returns \code{FALSE} otherwise. \cr \cr
}
\description{
The \code{mutatomic} class is a mutable version of atomic classes. \cr
It works exactly the same in all aspects as regular atomic classes. \cr
There is only one real difference: \cr
Pass-by-reference functions in 'squarebrackets'
only accept atomic objects when they are of class \code{mutatomic},
for greater safety. \cr
In all other aspects, \code{mutatomic} objects are the same as R's regular atomic objects,
including the behaviour of the \verb{[<-} operator . \cr
\cr
Exposed functions (beside the S3 methods):
\itemize{
\item \code{mutatomic()}: create a \code{mutatomic} object from given data.
\item \code{couldb.mutatomic()}: checks if an object could become \code{mutatomic}. \cr
An objects can become \code{mutatomic} if it is one of the following types: \cr
\link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
Factors can never be \code{mutatomic}. \cr \cr
}
}
\note{
Always use
the exported functions given by 'squarebrackets'
to create a \code{mutatomic} object,
as they make necessary safety checks. \cr
\cr
}
\examples{

x <- mutatomic(
  1:20, dim = c(5, 4), dimnames = list(letters[1:5], letters[1:4])
)
x

x <- matrix(1:10, ncol = 2)
x <- as.mutatomic(x)
is.mutatomic(x)
print(x)
x[, 1]
x[] <- as.double(x)
print(x)
is.mutatomic(x)
}
