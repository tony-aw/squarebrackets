% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa5_squarebrackets_modify.R
\name{aaa5_squarebrackets_modify}
\alias{aaa5_squarebrackets_modify}
\alias{squarebrackets_modify}
\title{Regarding Modification}
\description{
This help page describes the main modification semantics available in 'squarebrackets'. \cr \cr
}
\section{Base R's default modification}{

For most average users, R's default copy-on-modify semantics are fine. \cr
The benefits of the indexing arguments from 'squarebrackets' can be combined the \verb{[<-} operator,
through the \link{idx} method. \cr
The result of the \code{idx()} method
can be used inside the regular square-brackets operators. \cr
For example like so:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x <- array(...)
my_indices <- idx(x, sub, dims)
x[my_indices] <- value

y <- data.frame(...)
rows <- idx(y, 1:10, 1, inv = TRUE)
cols <- idx(y, c("a", "b"), 2)
y[rows, cols] <- value
}\if{html}{\out{</div>}}

thus allowing the user to benefit from the convenient index translations from 'squarebrackets',
whilst still using R's default copy-on-modification semantics
(instead of the semantics provided by 'squarebrackets'). \cr
\cr
\cr
}

\section{Explicit Copy}{

'squarebrackets' provides the \link{sb_mod} method to modify through copy. \cr
This method always copies the modification. \cr
For recursive objects, \link{sb_mod} returns the original object, where only the modified subsets are copied,
thus preventing unnecessary usage of memory. \cr
\cr
\cr
}

\section{Pass-by-Reference}{

'squarebrackets' provides the \link{sb_set} method to modify by reference,
meaning no copy is made at all. \cr
Pass-by-Reference is fastest and the most memory efficient. \cr
But it is also more involved than the other modification forms,
and requires more thought. \cr
See \link{squarebrackets_PassByReference} for more information. \cr
\cr
\cr
}

\section{Arguments \verb{rp, tf, .lapply}}{

Using the \code{rp} argument in the modification methods,
corresponds to something like the following: \cr

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x[...] <- rp

}\if{html}{\out{</div>}}

Using the \code{tf} argument (and \code{.lapply} argument, for recursive objects)
in the modification methods,
corresponds to something like the following: \cr

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x[...] <- tf(x[...]) # for atomic objects
x[...] <- .lapply(x, tf) # for recursive objects

}\if{html}{\out{</div>}}

where \code{tf} is a function.
For recursive objects, \code{tf} is accompanied by the \code{.lapply} argument. \cr
By default, \code{.lapply = lapply}. \cr
The user may supply a custom \code{lapply()}-like function
in this argument to use instead. \cr
For example, the perform parallel transformation,
the user may supply \verb{future.apply::}\link[future.apply]{future_lapply}. \cr
The supplied function must use the exact same argument convention as
\link[base]{lapply},
otherwise errors or unexpected behaviour may occur. \cr
\cr
\cr
}

\section{Recycling and Coercion}{

Recycling is not allowed in the modifcation methods. \cr
So, for example, \code{length(rp)} must be equal to the length of the selected subset,
or equal to \code{1}. \cr
The user should also take into account the auto-coercion rules of the object's class. \cr
See \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes} for details. \cr
\cr
}

