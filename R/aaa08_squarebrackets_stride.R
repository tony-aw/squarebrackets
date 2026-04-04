#' Argument stride
#'
#' @description
#' ‘squarebrackets’ provides the \link{long_x} and \link{long_set} methods
#' to perform sub-set operations on the interior of a vector,
#' without any indexing vector at all. \cr
#' Instead of an indexing vector, they use the `stride` argument. \cr
#' The following can be used in the `stride` argument: \cr
#' 
#'  - a \link{stride_pv} object: \cr
#'  Use this `stride` type to specify subsets based on property values,
#'  like `p == v`, where `p` is an atomic vector of properties
#'  (for example names(x)),
#'  and `v` is a value (or range of values) `p` might contain. \cr
#'  This function does not actually allocate an indexing vector.
#'  - a \link{stride_seq} object: \cr
#'  Use this `stride` type to specify a sequence in the form of `seq(from, to, by)`,
#'  without actually allocating a sequence indexing vector.
#'  - a \link{stride_ptrn} object: \cr
#'  Use this `stride` type to specify a patterned sequence in the form of \cr
#'  `(start:end)[pattern]`, \cr
#'  where `start` and `end` are natural scalars and `pattern` is a logical vector. \cr
#'  \link{stride_ptrn} specifies this sequence without actually allocating an indexing vector.
#'  - A formula in the form of ` ~ from:to:by`,
#'  where `from`, `to` and `by` are natural scalars. \cr
#'  This will be interpreted as `stride_seq(from, to, by)`. \cr
#'  - A formula in the form of ` ~ from:to:ptrn`,
#'  where `from` and `to` are natural scalars, and `ptrn` is a logical vector. \cr
#'  This will be interpreted as `stride_ptrn(from, to, ptrn)`. \cr \cr
#'  
#' In both formula forms, the `.N` keyword is available,
#' which equals `length(x)`
#' (where `x` is the input vector). \cr
#' For example, \cr
#' ` ~ 2:(.N - 1):c(TRUE, FALSE, FALSE, TRUE)` \cr
#' is equivalent to ` ~ 2:(length(x) - 1):c(TRUE, FALSE, FALSE, TRUE)`, \cr
#' and will be interpreted as `stride_ptrn(2, length(x) - 1, c(TRUE, FALSE, FALSE, TRUE))`. \cr
#' \cr
#' 
#' @example inst/examples/long.R

#' @rdname aaa08_squarebrackets_stride
#' @name aaa08_squarebrackets_stride
#' @aliases squarebrackets_stride
NULL
