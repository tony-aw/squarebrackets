#' Apply Functions Over Mutable Atomic Matrix Margins By Reference
#'
#' @description
#' The `setapply()` function
#' applies a functions over the rows or columns of a 
#' \link{mutable_atomic} matrix,
#' through \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' \cr
#' For every iteration, a copy of only a single row or column
#' (depending on the margin)
#' is made,
#' the function is applied on the copy,
#' and the original row/column is replaced by the modified copy through
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' \cr
#' The `setapply()` has about the same speed as \link[base]{apply}. \cr
#' However, `setapply()` uses much less memory than \link[base]{apply}. \cr \cr
#' 
#' 
#' @param x a \link{mutable_atomic} matrix. Arrays are not supported.
#' @param MARGIN a single integer scalar, giving the subscript to apply the function over. \cr
#' `1` indicates rows, `2` indicates columns. \cr
#' @param FUN the function to be applied. \cr
#' In the case of (infix) operators,
#' the function name must be backquoted or quoted.
#' 
#' 
#' 
#' @returns
#' Returns: VOID. This function modifies the object by reference. \cr
#' Do NOT use assignment like `x <- setapply(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#' @example inst/examples/dynamic_setapply.R
#' 
#' 

#' @rdname setapply
#' @export
setapply <- function(x, MARGIN, FUN) {
  
  if(!is.matrix(x) || !is.mutable_atomic(x)) {
    stop("`x` must be a mutable_atomic matrix")
  }
  
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1))
  
  if(!is.function(FUN)) {
    stop("`FUN` must be a function")
  }
  
  if(MARGIN == 1) {
    testf <- FUN(x[1,])
    if(typeof(testf) != typeof(x) || length(testf) != ncol(x)) {
      stop("improper function given")
    }
    
    .rcpp_setapply_row(x, FUN, abortcall = sys.call())
  } else if(MARGIN == 2) {
    testf <- FUN(x[,1])
    if(typeof(testf) != typeof(x) || length(testf) != nrow(x)) {
      stop("improper function given")
    }
    
    .rcpp_setapply_col(x, FUN, abortcall = sys.call())
  } else {
    stop("`MARGIN` must be 1 or 2")
  }
}

 


 

#' @keywords internal
#' @noRd
.rcpp_setapply_row <- function(x, f, abortcall) {
  
  if(is.logical(x)) {
    .rcpp_setapply_row_Logical(x, f)
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_setapply_row_Integer(x, f)
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_setapply_row_Numeric(x, f)
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_setapply_row_Character(x, f)
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_setapply_row_Complex(x, f)
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}

#' @keywords internal
#' @noRd
.rcpp_setapply_col <- function(x, f, abortcall) {
  
  if(is.logical(x)) {
    .rcpp_setapply_col_Logical(x, f)
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_setapply_col_Integer(x, f)
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_setapply_col_Numeric(x, f)
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_setapply_col_Character(x, f)
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_setapply_col_Complex(x, f)
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }
  
}