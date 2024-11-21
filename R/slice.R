#' Efficient Sequence-based Subset Methods on (Long) Vectors
#' 
#' @description
#' The `slice_` - methods are similar to the `sb_` - methods,
#' except they don't require an indexing vector,
#' and are designed for memory efficiency. \cr \cr
#' 
#' @param x an atomic object. \cr
#' For `slice_set` it must be a \link{mutable_atomic} \bold{variable}.
#' @param from,to,by see \link{cp_seq}.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param inv Boolean, indicating whether to invert the sequence. \cr
#' If `TRUE`,
#' `slice_set()` will apply replacement/transformation on all elements of the vector,
#' \bold{except} for the elements of the specified sequence.
#' @param use.names Boolean, indicating if flat names should be preserved. \cr
#' Note that, since `slice` operates on flat indices only,
#' dimensions and `dimnames` are always dropped.
#' @param sticky see \link{squarebrackets_options}.
#' @param ... see \link{squarebrackets_method_dispatch}.
#' 
#'
#' @returns
#' Similar to the `sb_` methods.
#' 
#' @example inst/examples/slice.R
#

#' @name slice
NULL

#' @rdname slice
#' @export
slice_x <- function(x, ...) {
  
  UseMethod("slice_x", x)
}


#' @rdname slice
#' @export
slice_x.default <- function(
    x, from = NULL, to = NULL, by = 1L, ...,
    use.names = TRUE, sticky = getOption("squarebrackets.sticky", FALSE)
) {
  myslice <- cp_seq(x, 0L, from, to, by)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  len <- myslice$length.out
  
  if(len == 0L) {
    out <- vector(typeof(x), length = 0L)
  }
  else if(by > 0) {
    out <- .rcpp_slice_x_atomic(x, start - 1L, end - 1L, abs(by), len)
  }
  else if(by < 0) {
    out <- .rcpp_slice_xrev_atomic(x, start - 1L, end - 1L, abs(by), len)
  }
  
  if(!is.null(names(x)) && use.names && len != 0L) {
    nms <- slice_x(names(x), from, to, by, use.names = FALSE)
    data.table::setattr(out, "names", nms)
  }
  if(is.factor(x)) {
    data.table::setattr(out, "contrasts", attr(x, "contrasts", exact = TRUE))
    data.table::setattr(out, "levels", attr(x, "levels", exact = TRUE))
    data.table::setattr(out, "class", oldClass(x))
  }
  if(is.mutable_atomic(x)) {
    .internal_set_ma(out)
  }
  if(is.logical(sticky) && length(sticky) == 1L) {
    if(sticky) {
      .internal_set_stickyattr(out, x)
    }
  }
  if(is.character(sticky) && inherits(x, sticky, which = FALSE)) {
    .internal_set_stickyattr(out, x)
  }
  
  
  
  return(out)
}

#' @rdname slice
#' @export
slice_rm <- function(x, ...) {
  
  UseMethod("slice_rm", x)
}


#' @rdname slice
#' @export
slice_rm.default <- function(
    x, from = NULL, to = NULL, by = 1L, ...,
    use.names = TRUE, sticky = getOption("squarebrackets.sticky", FALSE)
) {
  myslice <- cp_seq(x, 0L, from, to, by)
  by <- myslice$by
  len_rm <- myslice$length.out
  len_x <- length(x) - myslice$length.out
  
  if(len_rm == 0L) {
    return(x)
  }
  else if(len_rm == length(x)) {
    out <- vector(typeof(x), length = 0L)
  }
  else {
    if(myslice$start > myslice$end) {
      start <- myslice$end
      end <- myslice$start
    }
    else {
      start <- myslice$start
      end <- myslice$end
    }
    out <- .rcpp_slice_rm_atomic(
      x, start - 1L, end - 1L, abs(by), len_x
    )
    
  }
  
  
  if(!is.null(names(x)) && use.names) {
    nms <- slice_rm(names(x), from, to, by, use.names = FALSE)
    data.table::setattr(out, "names", nms)
  }
  if(is.factor(x)) {
    data.table::setattr(out, "contrasts", attr(x, "contrasts", exact = TRUE))
    data.table::setattr(out, "levels", attr(x, "levels", exact = TRUE))
    data.table::setattr(out, "class", oldClass(x))
  }
  if(is.mutable_atomic(x)) {
    .internal_set_ma(out)
  }
  if(is.logical(sticky) && length(sticky) == 1L) {
    if(sticky) {
      .internal_set_stickyattr(out, x)
    }
  }
  if(is.character(sticky) && inherits(x, sticky, which = FALSE)) {
    .internal_set_stickyattr(out, x)
  }
  
  return(out)
}

#' @rdname slice
#' @export
slice_set <- function(x, ...) {
  
  UseMethod("slice_set", x)
}


#' @rdname slice
#' @export
slice_set.default <- function(
    x, from = NULL, to = NULL, by = 1L, inv = FALSE,
    ...,
    rp, tf
) {
  
  
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  myslice <- cp_seq(x, 0L, from, to, by)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  len <- myslice$length
  
  # call correct internal function
  if(!inv && start <= end) {
    .slice_set(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else if(!inv && start > end) {
    .slice_setrev(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else if(inv) {
    .slice_setinv(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else {
    stop("improper input given")
  }
}


#' @keywords internal
#' @noRd
.slice_set <- function(x, start, end, by, len, rp, tf, abortcall) {
  
  if(!missing(tf)) {
    rp <- tf(slice_x(x, start, end, by))
  }
  
  if(len == 0) {
    return(invisible(NULL))
  }
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_slice_set_atomic(
    x, rp, start - 1L, end - 1L, abs(by), len
  )
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.slice_setrev <- function(x, start, end, by, len, rp, tf, abortcall) {
  
  if(len == 0) {
    return(invisible(NULL))
  }
  
  if(!missing(tf)) {
    rp <- tf(slice_x(x, start, end, by))
  }
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_slice_setrev_atomic(
    x, rp, start - 1L, end - 1L, abs(by), len
  )
  return(invisible(NULL))
  
  
}


#' @keywords internal
#' @noRd
.slice_setinv <- function(x, start, end, by, len, rp, tf, abortcall) {
  
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
    rp <- tf(slice_rm(x, start, end, by))
  }
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_slice_setinv_atomic(
    x, rp, start - 1L, end - 1L, abs(by), len_mod
  )
  return(invisible(NULL))
  
  
}
