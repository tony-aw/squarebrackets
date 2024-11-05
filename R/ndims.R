#' Get Number of Dimensions
#'
#' @description
#' `ndims(x)` is short-hand for `length(dim(x))`. \cr
#' 
#' @param x the object to get the number of dimensions from.
#'
#' @returns
#' An integer, giving the number of dimensions `x` has. \cr
#' For vectors, gives `0L`. \cr \cr
#'
#'
#'
#' @examples
#' 
#' x <- 1:10
#' ndims(x)
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' ndims(obj)
#' 
#' 


#' @rdname ndims
#' @export
ndims <- function(x) {
  length(dim(x))
}

