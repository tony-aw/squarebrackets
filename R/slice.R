#' Efficient Sequence-based Subset Methods on (Long) Vectors
#' 
#' @description
#' The `slice_` - methods are similar to the `ii_`/`ss_` - methods,
#' except they don't require an indexing vector,
#' and are designed for memory efficiency. \cr \cr
#' 
#' @param x an atomic object. \cr
#' For `slice_set` it must be a \link{mutatomic} \bold{variable}.
#' @param from,to,by see \link{cp_seq}.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param use  either `1` for normal slicing, or `-1` for inverted slicing.
#' `slice_set()` will apply replacement/transformation on all elements of the vector,
#' \bold{except} for the elements of the specified sequence.
#' @param use.names Boolean, indicating if flat names should be preserved. \cr
#' Note that, since the `slice_` methods operates on flat indices only,
#' dimensions and `dimnames` are always dropped.
#' @param sticky see \link{squarebrackets_options}.
#' @param ... see \link{squarebrackets_method_dispatch}.
#' 
#'
#' @returns
#' Similar to the `ii_` methods.
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
    x, from = NULL, to = NULL, by = 1L, use = 1,...,
    use.names = TRUE, sticky = getOption("squarebrackets.sticky", FALSE)
) {
  
  
  if(!.slice_use_OK(use)) {
    stop("improper `use` given")
  }
  .internal_check_dots(list(...), sys.call())
  
  if(use < 0) {
    return(.slice_wo(x, from, to, by, use.names, sticky))
  }
  else if(use > 0) {
    return(.slice_x(x, from, to, by, use.names, sticky))
  }
  
}

#' @keywords internal
#' @noRd
.slice_x <-function(x, from, to, by, use.names, sticky) {
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
    nms <- .slice_x(names(x), from, to, by, use.names = FALSE, sticky = FALSE)
    data.table::setattr(out, "names", nms)
  }
  if(is.factor(x)) {
    data.table::setattr(out, "contrasts", attr(x, "contrasts", exact = TRUE))
    data.table::setattr(out, "levels", attr(x, "levels", exact = TRUE))
    data.table::setattr(out, "class", oldClass(x))
  }
  if(is.mutatomic(x)) {
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


#' @keywords internal
#' @noRd
.slice_wo <- function(
    x, from, to, by, use.names, sticky
) {
  myslice <- cp_seq(x, 0L, from, to, by)
  by <- myslice$by
  len_wo <- myslice$length.out
  len_x <- length(x) - myslice$length.out
  
  if(len_wo == 0L) {
    return(x)
  }
  else if(len_wo == length(x)) {
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
    out <- .rcpp_slice_wo_atomic(
      x, start - 1L, end - 1L, abs(by), len_x
    )
    
  }
  
  
  if(!is.null(names(x)) && use.names) {
    nms <- .slice_wo(names(x), from, to, by, use.names = FALSE, sticky = FALSE)
    data.table::setattr(out, "names", nms)
  }
  if(is.factor(x)) {
    data.table::setattr(out, "contrasts", attr(x, "contrasts", exact = TRUE))
    data.table::setattr(out, "levels", attr(x, "levels", exact = TRUE))
    data.table::setattr(out, "class", oldClass(x))
  }
  if(is.mutatomic(x)) {
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
    x, from = NULL, to = NULL, by = 1L, use = 1,
    ...,
    rp, tf
) {
  
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  if(!.slice_use_OK(use)) {
    stop("improper `use` given")
  }
  .internal_check_dots(list(...), sys.call())
  
  myslice <- cp_seq(x, 0L, from, to, by)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  len <- myslice$length
  
  # call correct internal function
  if(use > 0 && start <= end) {
    .slice_set(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else if(use > 0 && start > end) {
    .slice_setrev(x, start, end, by, len, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  else if(use < 0) {
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
  
  len_wo <- len
  len_mod <- length(x) - len_wo
  
  if(len_mod == 0) {
    return(invisible(NULL))
  }
  
  if(!missing(tf)) {
    rp <- tf(.slice_wo(x, start, end, by, use.names = FALSE, sticky = FALSE))
  }
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_slice_setinv_atomic(
    x, rp, start - 1L, end - 1L, abs(by), len_mod
  )
  return(invisible(NULL))
  
  
}


.slice_use_OK <- function(use) {
  if(!is.numeric(use)) return(FALSE)
  if(length(use) != 1) return(FALSE)
  use <- as.integer(use)
  if(is.na(use)) return(FALSE)
  if(use == 0L) return(FALSE)
  if(abs(use) != 1L) return(FALSE)
  
  return(TRUE)
}
