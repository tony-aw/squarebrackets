#' Index-less Subset Methods on (Long) Vectors
#' 
#' @description
#' The `long_` - methods are similar to the `ii_` - methods,
#' except they don't require an indexing vector,
#' and are designed for memory efficiency. \cr \cr
#' 
#' @param x an atomic object. \cr
#' For `long_x`, `couldb.mutatomic(x)` must be `TRUE`.
#' For `long_set` it must be a \link{mutatomic} \bold{variable}.
#' @param stride see \link{squarebrackets_stride}.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param use  either `1` for normal slicing, or `-1` for inverted slicing.
#' @param use.names Boolean, indicating if flat names should be preserved. \cr
#' Note that, since the `long_` methods operates on flat indices only,
#' dimensions and `dimnames` are always dropped.
#' @param sticky see \link{squarebrackets_options}.
#' @param ... see \link{squarebrackets_method_dispatch}.
#' 
#'
#' @returns
#' Similar to the `ii_` methods.
#' 
#' @example inst/examples/long.R
#

#' @name long
NULL

#' @rdname long
#' @export
long_x <- function(x, ...) {
  
  stopifnot(couldb.mutatomic(x))
  
  UseMethod("long_x", x)
}


#' @rdname long
#' @export
long_x.default <- function(
    x, stride, use = 1,...,
    use.names = TRUE, sticky = getOption("squarebrackets.sticky", FALSE)
) {
  
  if(!.long_use_OK(use)) {
    stop("improper `use` given")
  }
  .internal_check_dots(list(...), sys.call())
  
  if(is.formula(stride)) {
    stride <- formula2stride(stride, x)
  }
  
  if(class(stride)[1] == "stride_pv") {
    return(.long_pv_x(x, stride, use, use.names, sticky, sys.call()))
  }
  else if(class(stride)[1] == "stride_seq") {
    return(.long_seq_x(x, stride, use, use.names, sticky, sys.call()))
  }
  else if(class(stride)[1] == "stride_ptrn") {
    return(.long_ptrn_x(x, stride, use, use.names, sticky, sys.call()))
  }
  else {
    stop("unknown type of `stride` given")
  }
  
  
}

#' @rdname long
#' @export
long_set <- function(x, ...) {
  
  UseMethod("long_set", x)
}


#' @rdname long
#' @export
long_set.default <- function(
    x, stride, use = 1,
    ...,
    rp, tf
) {
  
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  if(!.long_use_OK(use)) {
    stop("improper `use` given")
  }
  .internal_check_dots(list(...), sys.call())
  
  if(is.formula(stride)) {
    stride <- formula2stride(stride, x)
  }
  
  if(class(stride)[1] == "stride_pv") {
    return(.long_pv_set(x, stride, use, rp, tf, sys.call()))
  }
  else if(class(stride)[1] == "stride_seq") {
    return(.long_seq_set(x, stride, use, rp, tf, sys.call()))
  }
  else if(class(stride)[1] == "stride_ptrn") {
    return(.long_ptrn_set(x, stride, use, rp, tf, sys.call()))
  }
  else {
    stop("unknown type of `stride` given")
  }
}



.long_use_OK <- function(use) {
  if(!is.numeric(use)) return(FALSE)
  if(length(use) != 1) return(FALSE)
  use <- as.integer(use)
  if(is.na(use)) return(FALSE)
  if(use == 0L) return(FALSE)
  if(abs(use) != 1L) return(FALSE)
  
  return(TRUE)
}
