#' Apply Functions Over mutable_atomic Matrix Margins By Reference
#'
#' @description
#' The `setapply()` function
#' applies a functions over the rows or columns of a 
#' \link{mutable_atomic} matrix,
#' through \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' \cr
#' The `setapply()` is a bit faster and uses less memory than \link[base]{apply}. \cr \cr
#' 
#' 
#' @param x a \link{mutable_atomic} 2-dimensional array (i.e. a matrix). \cr
#' Arrays of other than 2 dimensions are not supported.
#' @param MARGIN a single integer scalar, giving the subscript to apply the function over. \cr
#' `1` indicates rows, `2` indicates columns. \cr
#' @param FUN the function to be applied. \cr
#' The function must return a vector of the same type of `x`,
#' and the appropriate length
#' (i.e. length `ncol(x)` when `MARGIN == 1` or length `nrow(x)` when `MARGIN == 2`). \cr \cr
#' 
#' 
#' 
#' @returns
#' Returns: VOID. This function modifies the object by reference. \cr
#' Do NOT use assignment like `x <- setapply(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#' @example inst/examples/setapply.R
#' 
#' 

#' @rdname setapply
#' @export
setapply <- function(x, MARGIN, FUN) {
  
  if(!is.matrix(x) || !is.mutable_atomic(x)) {
    stop("`x` must be a mutable_atomic matrix")
  }
  
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  if(!is.function(FUN)) {
    stop("`FUN` must be a function")
  }
  
  if(MARGIN == 1) {
    testf1 <- x[1,]
    testf2 <- testf1
    testf1 <- FUN(testf1)
    if(typeof(testf1) != typeof(x) || length(testf1) != ncol(x) || .rcpp_address(testf1) == .rcpp_address(testf2)) {
      stop("improper function given")
    }
    
    .rcpp_setapply_row(x, FUN, abortcall = sys.call())
    return(invisible(NULL))
    
  } else if(MARGIN == 2) {
    testf1 <- x[,1]
    testf2 <- testf1
    testf1 <- FUN(testf1)
    if(typeof(testf1) != typeof(x) || length(testf1) != nrow(x) || .rcpp_address(testf1) == .rcpp_address(testf2)) {
      stop("improper function given")
    }
    
    .rcpp_setapply_col(x, FUN, abortcall = sys.call())
    return(invisible(NULL))
    
  } else {
    stop("`MARGIN` must be 1 or 2")
  }
}
