#' Index-less Subset Methods on (Long) Vectors
#' 
#' @description
#' The `long_` - methods are similar to the `ii_` - methods,
#' except they don't require an indexing vector,
#' and are designed for memory efficiency. \cr \cr
#' 
#' @param x an atomic object. \cr
#' For `long_x()`, `couldb.mutatomic(x)` must be `TRUE`. \cr
#' For `long_set()` it must be a \link{mutatomic} \bold{variable}.
#' @param stride see \link{squarebrackets_stride}.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param use.names Boolean, indicating if flat names should be preserved. \cr
#' Note that, since the `long_` methods operates on
#' \bold{virtual} \link[=squarebrackets_index_fundamentals]{interior indices}
#' of an array/vector only,
#' dimensions and `dimnames` are always dropped. \cr
#' `r .mybadge_performance_set2("FALSE")`
#' @param sticky see \link{squarebrackets_options}.
#' @param ... see \link{squarebrackets_ellipsis}.
#' 
#'
#' @returns
#' For `long_x()`: returns the sub-setted object. \cr
#' Fr `long_set()`: returns nothing, but modifies the object by reference. \cr
#' 
#' @example inst/examples/long.R
#'
#' @concept long_
#' 
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
    x, stride, ...,
    use.names = TRUE, sticky = getOption("squarebrackets.sticky", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(is.formula(stride)) {
    stride <- formula2stride(stride, x)
  }
  
  if(class(stride)[1] == "stride_v") {
    return(.long_v_x(x, stride, use.names, sticky, sys.call()))
  }
  else if(class(stride)[1] == "stride_seq") {
    return(.long_seq_x(x, stride, use.names, sticky, sys.call()))
  }
  else if(class(stride)[1] == "stride_ptrn") {
    return(.long_ptrn_x(x, stride, use.names, sticky, sys.call()))
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
    x, stride,
    ...,
    rp, tf
) {
  
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .argscheck_rptf(rp, tf, sys.call())
  .internal_check_dots(list(...), sys.call())
  
  if(is.formula(stride)) {
    stride <- formula2stride(stride, x)
  }
  
  if(class(stride)[1] == "stride_v") {
    return(.long_v_set(x, stride, rp, tf, sys.call()))
  }
  else if(class(stride)[1] == "stride_seq") {
    return(.long_seq_set(x, stride, rp, tf, sys.call()))
  }
  else if(class(stride)[1] == "stride_ptrn") {
    return(.long_ptrn_set(x, stride, rp, tf, sys.call()))
  }
  else {
    stop("unknown type of `stride` given")
  }
}
