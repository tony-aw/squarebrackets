#' Get Size Properties of an Object
#'
#' @description
#' `ndim(x)` is short-hand for `length(dim(x))`. \cr
#' \cr
#' `s(x, m)` gets the size of an object `x` along margin `m`. \cr
#' If `m == 0`, `s()` gets the length of an object. \cr
#' If `m > 0`, `s()` gets `dim(x)[m]` of the object. \cr
#' \cr
#' 
#' 
#' @param x a vector, array, or data.frame.
#' @param m the margin.
#' @param ... further arguments passed to or from methods.
#'
#' @returns
#' For `ndim()`: \cr
#' An integer, giving the number of dimensions `x` has. \cr
#' For vectors, gives `0L`. \cr
#' \cr
#' For `rdim()`: \cr
#' An integer vector, giving the range `1:ndim(x)`. \cr
#' For vectors, gives `0L`. \cr
#' \cr
#' For `s()`: \cr
#' A numeric vector giving the size(s) of the object at the given margin(s).
#'
#'
#'
#' @example inst/examples/size.R
#' 
#' 


#' @rdname size
#' @export
ndim <- function(x) {
  length(dim(x))
}

#' @rdname size
#' @export
rdim <- function(x) {
  if(!is.null(dim(x))) {
    return(1:length(dim(x)))
  }
  else {
    return(0L)
  }
  
}


#' @rdname size
#' @export
s <- function(x, m = 0L, ...) {
  UseMethod("s", m)
}

#' @rdname size
#' @export
s.default <- function(x, m, ...) {
  .internal_check_dots(list(...), sys.call())
  if(!is.numeric(m)) {
    stop("`m` must be an integer")
  }
  if(length(m) > 1L && any(m == 0L)) {
    stop("multi-element `m` cannot contain 0")
  }
  if(any(m < 0)) {
    stop("`m` cannot be negative")
  }
  if(m[1L] == 0L) {
    if(is.data.frame(x)) {
      stop("`m = 0` not applicable for data.frames")
    }
    return(length(x))
  }
  else if(m > 0L) {
    return(dim(x)[m])
  }
  else {
    stop("m cannot be negative")
  }
}


