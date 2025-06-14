% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idx_r.R
\name{idx_r}
\alias{idx_r}
\title{Compute Integer Index Range}
\usage{
idx_r(x, m = 0L, from = NULL, to = NULL, by = 1L)
}
\arguments{
\item{x}{the object for which to compute subset indices.}

\item{m, from, to, by}{see \link{cp_seq}. \cr \cr}
}
\value{
If \code{length(m) == 1L}: a vector of numeric indices. \cr
\cr
If \code{length(m) > 1L}: a list of the same length as \code{m},
containing numeric vectors of indices.  \cr \cr
}
\description{
\code{idx_r()} computes integer index range(s). \cr \cr
}
\examples{

x <- data.frame(
  a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = -1:-10
)
print(x)
ind1 <- idx_r(x, 1, 2, 2* -1i) # rows 2:(nrow(x)-1)
ss2_x(x, ind1, 1L) # extract the row range

x <- array(1:125, c(5,5,5))
d <- 1:3
s <- idx_r(x, d, 2, 2* -1i) # 2:(n-1) for every dimension
ss_x(x, s = s, d = d) # same as x[ 2:4, 2:4, 2:4, drop = FALSE]

x <- letters
x[idx_r(x, 0, 2, 2* -1i)]

}
