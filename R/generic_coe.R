#' Method to Coerce and Transform (Recursive Subsets of) an Object
#'
#' @description
#' This is an S3 Method to completely transform
#' (a recursive subsets of)
#' an object with explicit coercion. \cr
#' 
#' Given some coercing function `v()`,
#' the following can be stated about this method. \cr
#' 
#' (1) For atomic objects (vectors, matrices, arrays), this method is equivalent to:
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
#' one can coercively transform columns BY REFERENCE
#' (which is more memory efficient),
#' using the following code
#' (again with columns specified by `col`, and some coercive transformation function `v`):
#' 
#' ```{r eval = FALSE}
#' col <- ... # some integer/character vector of column indices/names
#' x[, (col) := lapply(.SD, v), .SDcols = col]
#' 
#' ```
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,col,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param v the coercive transformation function to use.
#' @param ... further arguments passed to or from other methods.
#' 
#' @details
#' When replacing values by reference,
#' the (recursive subset of the) object is never coerced, as that requires making a deep copy;
#' instead, the replacement value is coerced. \cr
#' \cr
#' For example: \cr
#' Using `sb_set()` to replacing/transform one or more values of an integer type
#' (`int`)
#' object / list element / data.frame column,
#' to become `1.5`, will NOT coerce the object / list element / data.frame column
#' to a decimal type (`dbl`);
#' instead, the replacement `1.5` is coerced to the integer `1`. \cr
#' \cr
#' For this reason, the `sb_coe()` method can be used to coercively transform an object
#' BEFORE replacing or transforming values by reference. \cr
#' See also the Examples section below.
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
  UseMethod("sb_coe", x)
}


#' @rdname sb_coe
#' @export
sb_coe.default <- function(x, v, ...) {
  x[] <- v(x)
  return(x)
}


#' @rdname sb_coe
#' @export
sb_coe.factor <- function(x, v, ...) {
  return(v(x))
}


#' @rdname sb_coe
#' @export
sb_coe.list <- function(x, i, v, ...) {
  elements <- .indx_make_element(
    i, x, is_list = TRUE, chkdup = FALSE, inv = FALSE, abortcall = sys.call()
  )
  
  if(length(elements) == 0) return(x)
  
  x[elements] <- lapply(x[elements], v)
  
  return(x)
}


#' @rdname sb_coe
#' @export
sb_coe.data.frame <- function(x, col = NULL, vars = NULL, v, ...) {
  
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

