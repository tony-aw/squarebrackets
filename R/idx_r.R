#' Compute Integer Index Range
#' 
#' @description
#' `idx_r()` computes integer index range(s). \cr \cr
#' 
#' @param x the object to index.
#' @param m integer, giving the margin. \cr
#' For non-dimensional objects or for flat indices, specify `m = 0L`.
#' @param start integer or complex, of the same length as `m` or of length 1,
#' specifying the start point.
#' @param end integer or complex, , of the same length as `m` or of length 1,
#' specifying the end point.
#' @param by integer scalar, of the same length as `m` or of length 1,
#' specifying the step size. \cr
#' 
#' 
#' 
#' @details
#' \bold{start, stop, by} \cr
#' If `start, stop` are not specified,
#' using `by` will construct the following sequence: \cr
#' If `by` is positive, `seq.int(1L, n, by)`. \cr
#' If `by` is negative, `seq.int(n, 1L, by)`. \cr
#' Where `n` is the maximum index
#' (i.e. `length(x)` or `dim(x)[m]`, depending on the situation). \cr
#' \cr
#' If `start, stop, by` are all specified,
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
#' If `length(m) == 1L`: an integer vector of indices. \cr
#' \cr
#' If `length(m) > 1L`: a list of the same length as `m`,
#' containing integer vectors of indices. \cr
#' 
#' @example inst/examples/idx_r.R
#

#' @rdname idx_r
#' @export
idx_r <- function(x, m = 0L, start = NULL, end = NULL, by = 1L) {
  
  # convert m:
  if(!is.numeric(m)) {
    stop("`m` must be integer")
  }
  if(.C_any_badmargin(m, length(dim(x)))) {
    stop("`m` out of bounds")
  }
  
  # recycle lengths when necessary:
  m.len <- length(m)
  start <- .idx_tempfun(start, m.len)
  end <- .idx_tempfun(end, m.len)
  by <- .idx_tempfun(by, m.len)
  
  # perform checks:
  .idx_check_args(m, start, end, by, sys.call())
  
  
  slice <- .idx_r_construct(start, end, by, sys.call())
  
  lens <- ifelse(m == 0L, length(x), dim(x)[m])
  
  .idx_r_convert(slice, lens, sys.call())
  
}

#' @keywords internal
#' @noRd
.idx_check_args <- function(m, start, end, by, abortcall) {

  arg.list <- list(m, start, end, by)
  cls <- collapse::vclasses(arg.list, use.names = FALSE)
  if(any(!cls %in% c("numeric", "integer", "complex", "NULL"))) {
    stop(simpleError("`m`, `start`, `end` `by` must be numeric or `NULL`"))
  }
  lens <- collapse::vlengths(arg.list[cls != "NULL"], use.names = FALSE)
  if(length(unique(lens)) > 1L) {
    stop(simpleError("`m`, `start`, `end` `by` must be eqaul length or `NULL`"))
  }
  
  
}

#' @keywords internal
#' @noRd
.idx_r_construct <- function(start, end, by, abortcall) {
  
  startend.missing <- is.null(start) + is.null(end)
  
  
  if(startend.missing == 1L) {
    stop(simpleError("either specify both `start` and `end`, or specify neither", call = abortcall))
  }
  
  if(startend.missing == 0L) {
    by <- abs(by)
    out <- list(start = start, end = end, by = by)
    return(out)
  }
  
  
  if(startend.missing == 2L) {
    if(missing(by)) {
      stop(simpleError("`by` missing", call = abortcall))
    }
    out <- list(start = NULL, end = NULL, by = by)
    return(out)
  }
}

#' @keywords internal
#' @noRd
.idx_r_convert <- function(s, n, abortcall) {
  
  start <- s[["start"]]
  end <- s[["end"]]
  by <- s[["by"]]
  
  if(is.null(start) && is.null(end)) {
    
    if(length(by) == 1L) {
      if(by > 0L) {
        return(seq.int(1L, n, by))
      }
      if(by < 0L) {
        return(seq.int(n, 1L, by))
      }
    }
    
    out <- vector("list", length(by))
    for(i in seq_along(by)) {
      if(by[i] > 0L) {
       out[[i]] <- seq.int(1L, n[i], by[i])
      }
      if(by[i] < 0L) {
        out[[i]] <- seq.int(n[i], 1L, by[i])
      }
    }
    return(out)
    
  }
  
  start <- .idx_convert_startend(start, n, abortcall)
  end <- .idx_convert_startend(end, n, abortcall)
  
  by <- ifelse(start > end, -by, by)
  
  if(length(start) == 1L) {
    return(seq.int(start, end, by))
  }
  
  return(mapply(seq.int, start, end, by, SIMPLIFY = FALSE))
}


#' @keywords internal
#' @noRd
.idx_convert_startend <- function(x, n, abortcall) {
  if(is.complex(x)) {
    x <- .indx_convert_complex_multi(x, n, abortcall)
    if(any(x > n | x < 1L)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  else if(is.numeric(x)) {
    if(any(x > n | x < 1L)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  return(x)
}

#' @keywords internal
#' @noRd
.idx_tempfun <- function(x, m.len) {
  if(!is.null(x) && length(x) < m.len && length(x) == 1L) {
    x <- rep_len(x, m.len)
  }
  return(x)
}

#' @keywords internal
#' @noRd
.indx_convert_complex_multi <- function(indx, n, abortcall) {
  im <- Im(indx)
  re <- Re(indx)
  out <- .rcpp_indx_convert_cplx_multi(re, im, n)
  return(out)
}