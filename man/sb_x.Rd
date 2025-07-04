% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_x.R
\name{i_x}
\alias{i_x}
\alias{i2_x}
\alias{ss_x}
\alias{ss2_x}
\alias{i_x.default}
\alias{ss_x.default}
\alias{i2_x.default}
\alias{ss2_x.default}
\alias{ss2_x.data.frame}
\title{Methods to Extract, Exchange, or Duplicate Subsets of an Object}
\usage{
i_x(x, ...)

i2_x(x, ...)

ss_x(x, ...)

ss2_x(x, ...)

\method{i_x}{default}(x, i = NULL, ...)

\method{ss_x}{default}(x, s = NULL, d = 1:ndim(x), ...)

\method{i2_x}{default}(x, i = NULL, red = FALSE, ...)

\method{ss2_x}{default}(x, s = NULL, d = 1:ndim(x), red = FALSE, ...)

\method{ss2_x}{data.frame}(x, s = NULL, d = 1:2, obs = NULL, vars = NULL, ...)
}
\arguments{
\item{x}{see \link{squarebrackets_supported_structures}.}

\item{...}{see \link{squarebrackets_method_dispatch}.}

\item{i, s, d, obs, vars}{See \link{squarebrackets_indx_args}. \cr
Duplicates are allowed, resulting in duplicated indices. \cr
An empty index selection results in an empty object of length 0. \cr}

\item{red}{Boolean, for recursive objects only,
indicating if the result should be reduced. \cr
If \code{red = TRUE},
selecting a single element will give the simplified result,
like using \verb{[[]]}. \cr
If \code{red = FALSE}, a list is always returned regardless of the number of elements. \cr}
}
\value{
Returns a copy of the sub-setted object.
}
\description{
Methods to extract, exchange,
or duplicate (i.e. repeat x times) subsets of an object. \cr \cr
}
\examples{


# atomic objects ====

obj <- matrix(1:16, ncol = 4)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss_x(obj, s = n(1:3), d = 1:ndim(obj))
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
i_x(obj, i = \(x) x > 5)
# above is equivalent to obj[obj > 5]
ss_x(obj, s = n(c("a", "a")), d = 2L)
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(1:64, c(4,4,3))
print(obj)
ss_x(obj, s = n(1:3, 1:2), d = c(1,3))
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
i_x(obj, i = \(x)x > 5)
# above is equivalent to obj[obj > 5]


#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
i2_x(obj, 1) # obj[1]
i2_x(obj, 1, red = TRUE) # obj[[1]]
i2_x(obj, 1:2) # obj[1:2]
i2_x(obj, is.numeric) # obj[sapply(obj, is.numeric)]
# for recursive subsets, see lst_rec()


obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss2_x(obj, s = n(1:3), d = 1:ndim(obj))
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
i2_x(obj, i = is.numeric)
# above is equivalent to obj[sapply(obj, is.numeric)]
ss2_x(obj, s = n(c("a", "a")), d = 2L)
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(as.list(1:64), c(4,4,3))
print(obj)
ss2_x(obj, s = n(1:3, 1:2), d = c(1,3))
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
i2_x(obj, i = \(x)x > 5)
# above is equivalent to obj[sapply(obj, \(x) x > 5)]

#############################################################################

# data.frame-like objects ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
print(obj)
ss2_x(obj, n(1:3)) # obj[1:3, 1:3, drop = FALSE]
ss2_x(obj, obs = ~ (a > 5) & (c < 19), vars = is.numeric)


}
