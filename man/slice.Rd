% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice.R
\name{slice}
\alias{slice}
\alias{slice_x}
\alias{slice_x.default}
\alias{slice_wo}
\alias{slice_wo.default}
\alias{slice_set}
\alias{slice_set.default}
\title{Efficient Sequence-based Subset Methods on (Long) Vectors}
\usage{
slice_x(x, ...)

\method{slice_x}{default}(
  x,
  from = NULL,
  to = NULL,
  by = 1L,
  ...,
  use.names = TRUE,
  sticky = getOption("squarebrackets.sticky", FALSE)
)

slice_wo(x, ...)

\method{slice_wo}{default}(
  x,
  from = NULL,
  to = NULL,
  by = 1L,
  ...,
  use.names = TRUE,
  sticky = getOption("squarebrackets.sticky", FALSE)
)

slice_set(x, ...)

\method{slice_set}{default}(x, from = NULL, to = NULL, by = 1L, inv = FALSE, ..., rp, tf)
}
\arguments{
\item{x}{an atomic object. \cr
For \code{slice_set} it must be a \link{mutatomic} \bold{variable}.}

\item{...}{see \link{squarebrackets_method_dispatch}.}

\item{from, to, by}{see \link{cp_seq}.}

\item{use.names}{Boolean, indicating if flat names should be preserved. \cr
Note that, since the \code{slice_} methods operates on flat indices only,
dimensions and \code{dimnames} are always dropped.}

\item{sticky}{see \link{squarebrackets_options}.}

\item{inv}{Boolean, indicating whether to invert the sequence. \cr
If \code{TRUE},
\code{slice_set()} will apply replacement/transformation on all elements of the vector,
\bold{except} for the elements of the specified sequence.}

\item{rp, tf}{see \link{squarebrackets_modify}.}
}
\value{
Similar to the \code{i_}/\code{ss_} methods.
}
\description{
The \code{slice_} - methods are similar to the \code{i_}/\code{ss_} - methods,
except they don't require an indexing vector,
and are designed for memory efficiency. \cr \cr
}
\examples{


x <- mutatomic(1:1e7)

# extract:
slice_x(x, 1, 10)

# reverse:
slice_x(x, -1i, 1) |> head()

# remove:
slice_wo(x, 1, -11i) # all elements except the last 10

# replace every other element:
x <- mutatomic(1:1e7)
slice_set(x, 2, -1i, 2, rp = -1)
head(x)

# replace all elements except the first element:
x <- mutatomic(1:1e7)
slice_set(x, 1, 1, inv = TRUE, rp = -1)
head(x)

}
