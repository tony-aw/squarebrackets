#' Method to Coercively Transform (Recursive Subsets of) an Object
#'
#' @description
#' This is an S3 Method to completely transform
#' (a recursive subsets of)
#' an object with explicit coercion. \cr
#' \cr
#' Given some coercing function `v()`,
#' the following can be stated about this method. \cr
#' 
#' (1) For atomic objects (vectors, matrices, arrays),
#' this method is \bold{almost} equivalent to:
#' 
#' ```{r eval = FALSE}
#' x[] <- v(x)
#' ```
#' 
#' (2) For factors, this method is equivalent to:
#' 
#' ```{r eval = FALSE}
#' x <- v(x)
#' ```
#' 
#' (3) For lists,
#' with one or multiple elements specified by argument `i`,
#' this method is equivalent to:
#' 
#' ```{r eval = FALSE}
#' \(x, i) { x[i] <- lapply(x[i], v); return(x) }
#' ```
#' 
#' (4) And for data.frame-like objects,
#' with one or multiple columns specified by argument `col`,
#' this method is equivalent to:
#' 
#' ```{r eval = FALSE}
#' collapse::ftransformv(x, col, v)
#' 
#' ```
#' 
#' Note that when `x` is a `data.table`,
#' one can coercively transform columns by reference
#' (which is more memory efficient),
#' using \link{dt_setcoe}. \cr
#' \cr
#' Use `sb_coe(x, ...)` if `x` is a non-recursive object (i.e. atomic or factor). \cr
#' Use `sb2_coe(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,col,vars,idx,dims See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param v the coercive transformation function to use.
#' @param .lapply `sb2_coe()` by default uses \link[base]{lapply}
#' for lists and \link[collapse]{dapply} data.frame-like objects
#' to compute `tf()` on every list element or data.frame column. \cr
#' The user may supply a custom `lapply()/dapply()`-like function
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

#' @rdname sb_coe
#' @export
sb_coe <- function(x, ...) {
  
  if(is.recursive(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  
  UseMethod("sb_coe", x)
}


#' @rdname sb_coe
#' @export
sb_coe.default <- function(x, v, ...) {
  
  temp.attr <- attributes(x)
  out <- v(x)
  return(.fix_attr(out, temp.attr))
}


#' @rdname sb_coe
#' @export
sb_coe.factor <- function(x, v, ...) {
  
  return(v(x))
}


#' @rdname sb_coe
#' @export
sb2_coe <- function(x, ...) {
  
  if(!is.recursive(x)) {
    stop("Use the `sb_` methods for non-recursive objects")
  }
  
  UseMethod("sb2_coe", x)
}


#' @rdname sb_coe
#' @export
sb2_coe.default <- function(x, i, v, ..., .lapply = lapply) {
  elements <- .indx_make_element(
    i, x, is_list = TRUE, chkdup = FALSE, inv = FALSE, abortcall = sys.call()
  )
  
  
  if(length(elements) == 0) return(x)
  
  x[elements] <- .lapply(x[elements], v)
  
  return(x)
}


#' @rdname sb_coe
#' @export
sb2_coe.array <- function(
    x, idx = NULL, dims = NULL, i = NULL, v, ..., .lapply = lapply
) {
  
  
  if(!is.null(i)) {
    return(sb2_coe.default(x, i, v, ..., .lapply = lapply))
  }
  
  return(.arr_tf_list(x, idx, dims, v, chkdup = FALSE, .lapply, abortcall = sys.call()))
}

#' @rdname sb_coe
#' @export
sb2_coe.data.frame <- function(x, col = NULL, vars = NULL, v, ...) {
  
  .check_args_df(x, row = NULL, col, filter = NULL, vars, abortcall = sys.call())
  
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = FALSE, inv = FALSE, abortcall = sys.call()
  )}
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
  }
  
  if(length(col) == 0) return(x)
  
  return(collapse::ftransformv(x, vars = col, FUN = v, apply = TRUE))
}

