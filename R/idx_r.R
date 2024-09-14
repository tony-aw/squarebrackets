#' Compute Integer Index Range
#' 
#' @description
#' `idx_r()` computes integer index range(s). \cr \cr
#' 
#' @param x the object to index.
#' @param m integer or complex number, giving the margin. \cr
#' For non-dimensional objects or for flat indices, specify `m = 0L`.
#' @param start integer or complex, of the same length as `m` or of length 1,
#' specifying the start point.
#' @param end integer or complex, , of the same length as `m` or of length 1,
#' specifying the end point.
#' @param by integer, of the same length as `m` or of length 1,
#' specifying the step size. \cr
#' 
#' 
#' 
#' @details
#' \bold{start, end, by} \cr
#' If `start, end` are not specified,
#' using `by` will construct the following sequence: \cr
#' If `by` is positive, `seq.int(1L, n, by)`. \cr
#' If `by` is negative, `seq.int(n, 1L, by)`. \cr
#' Where `n` is the maximum index
#' (i.e. `length(x)` or `dim(x)[m]`, depending on the situation). \cr
#' \cr
#' If `start, end, by` are all specified,
#' `by` is stored as `abs(by)`,
#' and the sign  of `by` is automatically adjusted to ensure a sensible sequence is created. \cr
#' \cr
#' So, for example,
#' to specify something like `n:1`, one can simply call `idx_r(x, m, by = -1L)`. \cr \cr
#' 
#' 
#' \bold{Heterogeneous Complex input} \cr
#' The user can specify complex vectors with varying imaginary parts for
#' `start` and `end`. \cr
#' Except for this difference,
#' the complex vectors are translated to integer vectors as explained in
#' \link{squarebrackets_indx_args}. \cr \cr
#' 
#' \bold{Multiple indices at once} \cr
#' The `idx_r()` function can compute indices for multiple dimensions at once,
#' by specifying a vector for `m`. \cr \cr
#' 
#'
#' @returns
#' If `length(m) == 1L`: a vector of numeric indices. \cr
#' \cr
#' If `length(m) > 1L`: a list of the same length as `m`,
#' containing numeric vectors of indices. \cr
#' 
#' @example inst/examples/idx_r.R
#

#' @rdname idx_r
#' @export
idx_r <- function(x, m = 0L, start = NULL, end = NULL, by = 1L) {
  
  lst <- ci_seq(x, m, start, end, by, sys.call())
  
  if(length(m) == 1L) {
    return(seq.int(lst$start, lst$end, lst$by))
  }
  
  start <- lst$start
  end <- lst$end
  by <- lst$by
  
  return(mapply(seq.int, start, end, by, SIMPLIFY = FALSE))
  
}
