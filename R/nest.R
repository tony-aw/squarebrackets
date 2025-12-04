#' Nest
#'
#' @description
#' The \link[base]{c}\code{()} function concatenates vectors or lists into a vector
#' (if possible) or else a list. \cr
#' In analogy to that function, the `n()` function \bold{nests} objects into a list
#' (not into an atomic vector, as atomic vectors cannot be nested). \cr
#' It is a short-hand version of the \link[base]{list} function. \cr
#' This is handy because lists are often needed in 'squarebrackets',
#' especially for arrays. \cr
#' 
#' 
#'
#' @returns
#' The list.
#'
#'
#'
#' @examples
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' ss_x(obj, n(1:3, 1:2), c(1,3))
#' # above is equivalent to obj[1:3, , 1:2, drop = FALSE]
#' 
#' 


#' @rdname nest
#' @export
n <- base::list
