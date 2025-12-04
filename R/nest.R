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
#' @example inst/examples/nest.R
#' 
#' 


#' @rdname nest
#' @export
n <- base::list
