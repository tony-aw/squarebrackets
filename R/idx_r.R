#' Compute Heterogeneous Index Range
#' 
#' @description
#' The `idx_r()` function computes an index range,
#' where one can combine different types for the start and end point. \cr \cr
#' 
#' @param x the object from which to compute the indices.
#' @param m a single non-negative integer giving the margin for which to compute indices. \cr
#' For flat indices or for non-dimensional objects, use `m = 0L`. \cr
#' @param start,end the start and end of the range. \cr
#' Can be any combination of any of the following: \cr
#'  - A strictly positive integer.
#'  - A complex number (see explanation in \link{squarebrackets_indx_args}).
#'  - A string to refer to a name. \cr
#'  In case of `start`, the first  match will be used. \cr
#'  In case of `end`, the last match will be used. \cr
#'  - a Boolean-returning function to be applied on `indices`. \cr
#'  If `m == 0L`, `indices` is here defined as `setNames(1:length(x), names(x))`. \cr
#'  If `m > 0L`, `indices` is here defined as `setNames(1:dim(x)[m], dimnames(x)[[m]])`. \cr
#'  For `start`, the first `TRUE` match will be used. \cr
#'  For `end`, the last `TRUE` match will be used. \cr
#' @param by an optional single integer, giving the step size. \cr
#' `idx_r()` will automatically make sure the sign of `by` is set correctly.
#'
#' @returns
#' A vector of integer indices.
#'
#'
#'
#' @example inst/examples/idx_r.R

#' @rdname idx_r
#' @export
idx_r <- function(x, m, start, end, by = 1L) {
  
  if(length(m) != 1L || !is.numeric(m) || m < 0L) {
    stop("`m` must be a single non-negative integer")
  }
  
  if(m == 0L) {
    return(.idx_r_internal(x, start, end, by, sys.call()))
  }
  
  indices <- stats::setNames(seq_len(dim(x)[m]), dimnames(x)[[m]])
  return(.idx_r_internal(indices, start, end, by, sys.call()))
}


#' @keywords internal
#' @noRd
.idx_r_internal <- function(x, start, end, by, abortcall) {
  # error handling:
  if(length(start) != 1 || length(end) != 1) {
    stop(simpleError("`start` and `end` must be of length 1", call = abortcall))
  }
  
  # start:
  if(is.complex(start)) {
    start <- .indx_convert_complex(start, length(x), abortcall)
    if(start > length(x) || start < 1L) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
    
  }
  else if(is.numeric(start)) {
    if(start > length(x) || start < 1L) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  else if(is.character(start)) {
    if(is.null(names(x))) {
      stop(simpleError("`x` has no names", call = abortcall))
    }
    start <- collapse::fmatch(start, names(x))
  }
  else if(is.function(start)) {
    indices <- stats::setNames(1:length(x), names(x))
    start <- start(indices)
    if(!is.logical(start)) {
      stop(simpleError("non-Boolean function provided", call = abortcall))
    }
    start <- min(which(start))
  }
  
  # end:
  if(is.complex(end)) {
    end <- .indx_convert_complex(end, length(x), abortcall)
    if(end > length(x) || end < 1L) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
    
  }
  else if(is.numeric(end)) {
    if(end > length(x) || end < 1L) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  else if(is.character(end)) {
    if(is.null(names(x))) {
      stop(simpleError("`x` has no names", call = abortcall))
    }
    if(is.na(end)) {
      end <- max(collapse::whichNA(names(x)))
    }
    else {
      end <- max(collapse::whichv(names(x), end))
    }
  }
  else if(is.function(end)) {
    indices <- stats::setNames(1:length(x), names(x))
    end <- end(indices)
    if(!is.logical(end)) {
      stop(simpleError("non-Boolean function provided", call = abortcall))
    }
    end <- max(which(end))
  }
  
  if(length(by) != 1L || !is.numeric(by)) {
    stop(simpleError("`by` must be a single integer"))
  }
  
  by <- abs(by)
  if(start > end) by <- -by
  
  return(seq.int(start, end, by))
  
}
