#' Method to Coercively Transform Subsets of Recursive Objects
#'
#' @description
#' This is an S3 Method to completely transform subsets of
#' recursive objects with explicit coercion. \cr
#' \cr
#' Note that when `x` is a `data.table`,
#' one can coercively transform columns by reference
#' (which is more memory efficient),
#' using \link{dt_setcoe}. \cr \cr
#'
#' @param x a recursive object (list-like or data.frame-like).
#' @param i,col,vars,idx,dims,inv See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param v the coercive transformation function to use.
#' @param .lapply the generic methods use \link[base]{lapply}
#' for list- and data.frame-like objects
#' to compute `tf()` on every list element or dataset column. \cr
#' The user may supply a custom `lapply()`-like function
#' in this argument to use instead. \cr
#' For example, the perform parallel transformation,
#' the user may supply `future.apply::`\link[future.apply]{future_lapply}. \cr
#' The supplied function must use the exact same argument convention as
#' \link[base]{lapply},
#' otherwise errors or unexpected behaviour may occur.
#' @param ... further arguments passed to or from other methods.
#' 
#' 
#' @returns
#' A copy of the coercively transformed object.
#'
#'
#' @example inst/examples/generic_coe.R



#' @rdname sb2_coe
#' @export
sb2_coe <- function(x, ...) {
  
  UseMethod("sb2_coe", x)
}


#' @rdname sb2_coe
#' @export
sb2_coe.default <- function(x, i, inv = FALSE, ..., v, .lapply = lapply) {
  elements <- .indx_make_element(
    i, x, is_list = TRUE, chkdup = FALSE, inv = inv, abortcall = sys.call()
  )
  
  
  if(length(elements) == 0) return(x)
  
  x[elements] <- .lapply(x[elements], v)
  
  return(x)
}


#' @rdname sb2_coe
#' @export
sb2_coe.array <- function(
    x, idx = NULL, dims = NULL, i = NULL, inv = FALSE, ..., v, .lapply = lapply
) {
  
  
  if(!is.null(i)) {
    return(sb2_coe.default(x, i, inv = inv, ..., v = v, .lapply = lapply))
  }
  
  return(.arr_tf_list(x, idx, dims, inv, v, chkdup = FALSE, .lapply, abortcall = sys.call()))
}

#' @rdname sb2_coe
#' @export
sb2_coe.data.frame <- function(x, col = NULL, vars = NULL, inv = FALSE, ..., v) {
  
  .check_args_df(x, row = NULL, col, filter = NULL, vars, abortcall = sys.call())
  
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = FALSE, inv = inv, abortcall = sys.call()
  )}
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = inv, abortcall = sys.call())
  }
  
  if(length(col) == 0) return(x)
  
  return(collapse::ftransformv(x, vars = col, FUN = v, apply = TRUE))
}

