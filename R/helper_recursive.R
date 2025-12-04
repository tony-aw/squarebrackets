#' Helper Functions for Sub-Set Operations on Recursive Objects
#'
#' @description
#' 'squarebrackets' provides some helper functions for sub-set operations on recursive objects
#' (lists and data.frames). \cr
#' \cr
#' `dropl(x)` returns `x[[1L]]` if `length(x) == 1`, and returns `x` otherwise. \cr
#' This can be used for the `*_x` and `*_wo` methods on recursive objects,
#' where one would like to extract the contents of the singular selection. \cr
#' \cr
#' 
#' @param x a list.
#'
#' @returns
#' For `dropl()`: \cr
#' If `length(x) == 1L`, `dropl(x)` returns `x[[1L]]`; \cr
#' otherwise it returns the original `x` unchanged. \cr
#' \cr
#'
#'
#'
#' @examples
#' 
#' obj <- as.list(1:10)
#' ii_x(obj, 1) |> dropl() # equivalent to obj[[1L]]
#' 
#' 


#' @rdname helper_recursive
#' @export
dropl <- function(x) {
  if(!is.list(x)) {
    stop("`x` must be a list")
  }
  
  if(length(x) == 1L) {
    return(x[[1L]])
  }
  else {
    return(x)
  }
}




#' @keywords internal
#' @noRd
.funply <- function(tf) {
  if(!is.function(tf)) {
    stop("`tf` must be a function")
  }
  return(function(x) { lapply(x, tf) } )
}
