#' Get the Size of an Object Along a Margin
#'
#' @description
#' `s(x, m)` gets the size of an object `x` along margin `m`. \cr
#' If `m == 0`, `s()` gets the length of an object. \cr
#' If `m > 0`, `s()` gets `dim(x)[m]` of the object. \cr
#' 
#' @param x an object
#' @param m the margin.
#' @param ... further arguments passed to or from methods.
#'
#' @returns
#' A numeric scalar.
#'
#'
#'
#' @examples
#' 
#' x <- array(1:64, c(4,4,3))
#' print(x)
#' ss <- n(c(s(x, 1), 1), c(s(x, 3), 1))
#' ss_x(x, ss, c(1,3))
#' 
#' 
#' 


#' @rdname size
#' @export
s <- function(x, m = 0L, ...) {
  UseMethod("s", m)
}

#' @rdname size
#' @export
s.default <- function(x, m, ...) {
  .internal_check_dots(list(...), sys.call())
  if(!is.numeric(m) || length(m) != 1L) {
    stop("`m` must be an integer scalar")
  }
  if(m == 0L) {
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

