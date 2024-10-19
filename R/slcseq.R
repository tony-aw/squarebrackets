#' Efficient Sequence-based Subset Methods on (Long) Vectors
#' 
#' @description
#' The `slcseq_` - methods are similar to the `sb_` - methods,
#' except they don't require an indexing vector,
#' and are designed for memory efficiency. \cr \cr
#' 
#' @param x an atomic object. \cr
#' For `slcseq_set` it must be a \link{mutable_atomic} \bold{variable}.
#' @param from,to,by see \link{cp_seq}.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param inv Boolean, indicating whether to invert the sequence. \cr
#' If `TRUE`,
#' `slcseq_set()` will apply replacement/transformation on all elements of the vector,
#' \bold{except} for the elements of the specified sequence.
#' @param use.names Boolean, indicating if flat names should be preserved. \cr
#' Note that, since `slcseq` operates on flat indices only, `dimnames` are always dropped.
#' @param ... see \link{squarebrackets_method_dispatch}
#' 
#'
#' @returns
#' Similar to the `sb_` methods.
#' 
#' @example inst/examples/slcseq.R
#


#' @rdname slcseq
#' @export
slcseq_x <- function(x, ...) {
  
  UseMethod("slcseq_x", x)
}


#' @rdname slcseq
#' @export
slcseq_x.default <- function(
    x, from = NULL, to = NULL, by = 1L, ...,
    use.names = TRUE
) {
  myslcseq <- cp_seq(x, 0L, from, to, by)
  start <- myslcseq$start
  end <- myslcseq$end
  by <- myslcseq$by
  len <- myslcseq$length
  
  if(len == 0L) {
    out <- vector(typeof(x), length = 0L)
  }
  else if(by > 0) {
    out <- .rcpp_slcseq_x(x, start - 1L, end - 1L, abs(by), len, abortcall = sys.call())
  }
  else if(by < 0) {
    out <- .rcpp_slcseq_xrev(x, start - 1L, end - 1L, abs(by), len, abortcall = sys.call())
  }
  
  if(!is.null(names(x)) && use.names) {
    nms <- slcseq_x(names(x), from, to, by, use.names = FALSE)
    data.table::setattr(out, "names", nms)
  }
  if(is.mutable_atomic(x)) {
    .internal_set_ma(out)
  }
  
  return(out)
}

#' @rdname slcseq
#' @export
slcseq_rm <- function(x, ...) {
  
  UseMethod("slcseq_rm", x)
}


#' @rdname slcseq
#' @export
slcseq_rm.default <- function(
    x, from = NULL, to = NULL, by = 1L, ...,
    use.names = TRUE
) {
  myslcseq <- cp_seq(x, 0L, from, to, by)
  by <- myslcseq$by
  len_rm <- myslcseq$length.out
  len_x <- length(x) - myslcseq$length.out
  
  if(len_rm == 0L) {
    return(x)
  }
  else if(len_rm == length(x)) {
    out <- vector(typeof(x), length = 0L)
  }
  else {
    if(myslcseq$start > myslcseq$end) {
      start <- myslcseq$end
      end <- myslcseq$start
    }
    else {
      start <- myslcseq$start
      end <- myslcseq$end
    }
    out <- .rcpp_slcseq_rm(
      x, start - 1L, end - 1L, abs(by), len_x, abortcall = sys.call()
    )
    
  }
  
  
  if(!is.null(names(x)) && use.names) {
    nms <- slcseq_rm(names(x), from, to, by, use.names = FALSE)
    data.table::setattr(out, "names", nms)
  }
  if(is.mutable_atomic(x)) {
    .internal_set_ma(out)
  }
  
  return(out)
}

#' @rdname slcseq
#' @export
slcseq_set <- function(x, ...) {
  
  UseMethod("slcseq_set", x)
}


#' @rdname slcseq
#' @export
slcseq_set.default <- function(
    x, from = NULL, to = NULL, by = 1L, inv = FALSE,
    ...,
    rp, tf
) {
  
  
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  myslcseq <- cp_seq(x, 0L, from, to, by)
  start <- myslcseq$start
  end <- myslcseq$end
  by <- myslcseq$by
  len <- myslcseq$length
  
  # call correct internal function
  if(!inv && start <= end) {
    .slcseq_set(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else if(!inv && start > end) {
    .slcseq_setrev(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else if(inv) {
    .slcseq_setinv(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else {
    stop("improper input given")
  }
}


#' @keywords internal
#' @noRd
.slcseq_set <- function(x, start, end, by, len, rp, tf, abortcall) {
  
  if(!missing(tf)) {
    rp <- tf(slcseq_x(x, start, end, by))
  }
  
  if(len == 0) {
    return(invisible(NULL))
  }
  
  .rcpp_slcseq_set(
    x, rp, start - 1L, end - 1L, abs(by), len, abortcall = abortcall
  )
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.slcseq_setrev <- function(x, start, end, by, len, rp, tf, abortcall) {
  
  if(len == 0) {
    return(invisible(NULL))
  }
  
  if(!missing(tf)) {
    rp <- tf(slcseq_x(x, start, end, by))
  }
  
  .rcpp_slcseq_setrev(
    x, rp, start - 1L, end - 1L, abs(by), len, abortcall = abortcall
  )
  return(invisible(NULL))
  
  
}


#' @keywords internal
#' @noRd
.slcseq_setinv <- function(x, start, end, by, len, rp, tf, abortcall) {
  
  if(start > end ) {
    oldstart <- start
    oldend <- end
    start <- oldend
    end <- oldstart
  }
  
  len_rm <- len
  len_mod <- length(x) - len_rm
  
  if(len_mod == 0) {
    return(invisible(NULL))
  }
  
  if(!missing(tf)) {
    rp <- tf(slcseq_rm(x, start, end, by))
  }
  
  .rcpp_slcseq_setinv(
    x, rp, start - 1L, end - 1L, abs(by), len_mod, abortcall = abortcall
  )
  return(invisible(NULL))
  
  
}
