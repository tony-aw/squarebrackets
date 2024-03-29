#' Method to Modify Subsets of a Mutable Object By Reference
#'
#' @description
#' This is an S3 Method to replace or transform a subset of a
#' \link[=squarebrackets_mutable_classes]{supported mutable object}
#' using
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics} \cr
#' Use `sb_set(x, ...)` if `x` is a non-recursive object (i.e. \link{mutable_atomic}). \cr
#' Use `sb2_set(x, ...)` if `x` is a recursive object (i.e. \link{data.table}). \cr \cr
#' 
#'
#' @param x a \bold{variable} belonging to one of the
#' \link[=squarebrackets_mutable_classes]{supported mutable classes}. \cr
#' @param i,row,col,idx,dims,rcl,filter,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... further arguments passed to or from other methods.
#' @param tf the transformation function.
#' @param rp an object of somewhat the same type as the selected subset of \code{x},
#' and the same same length as the selected subset of \code{x} or a length of 1.
#' @param chkdup see \link{squarebrackets_duplicates}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
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
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset.
#' Specifying `rp` will replace the subset.
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' Note that there is no `sb_set()` method for factors: this is intentional. \cr
#' \cr
#' 
#' 
#' @returns
#' Returns: VOID. This method modifies the object by reference. \cr
#' Do not use assignments like `x <- sb_set(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#'
#' @example inst/examples/generic_set.R
#' 

#' @rdname sb_set
#' @export
sb_set <- function(x, ...) {
  
  UseMethod("sb_set", x) ## commentary
}


#' @rdname sb_set
#' @export
sb_set.default <- function(
    x, i, ..., rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  # function:
  elements <- .indx_make_element(
    i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )
  .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
  return(invisible(NULL))
}



#' @rdname sb_set
#' @export
sb_set.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)) {
  
  # error checks:
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  # function:
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    return(.sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call()))
    
  }


  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(invisible(NULL))
  }
  
  if(is.null(row) && is.null(col)) {
    .sb_set_atomic(x, seq_along(x), rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  .set_mat(x, row, col, rp, tf, abortcall = sys.call())
  return(invisible(NULL))

}


#' @rdname sb_set
#' @export
sb_set.array <- function(
    x, idx = NULL, dims = NULL, rcl = NULL, i = NULL, ..., rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  # function:
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  if(!is.null(rcl)) {
    elements <- .sb3d_get_elements(
      x, row = rcl[[1]], col = rcl[[2]], lyr = rcl[[3]], chkdup = chkdup, abortcall = sys.call()
    )
    .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
    return(invisible(NULL))
  }

  if(is.null(idx) && is.null(dims)) {
    .sb_set_atomic(x, seq_along(x), rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .arr_check(x, idx, dims, ndims, abortcall = sys.call())
  lst <- .arr_lst_grid(
    x, ndims, idx, dims, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )
  elements <- sub2ind(lst, x.dim, checks = FALSE)
  
  .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
  return(invisible(NULL))
}

#' @rdname sb_set
#' @export
sb2_set <- function(x, ...) {
  
  UseMethod("sb2_set", x)
}

#' @rdname sb_set
#' @export
sb2_set.default <- function(x, ...) {
  stop("`x` is not a (supported) mutable object")
}

#' @rdname sb_set
#' @export
sb2_set.data.table <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL,
    ..., rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  if(!data.table::is.data.table(x)) {
    stop("`x` is not a (supported) mutable object")
  }
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(invisible(NULL))
  }
  
  if(is.null(col)) col <- collapse::seq_col(x)
  col <- as.integer(col)
  
  
  if(is.null(row)) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- .lapply(collapse::ss(x, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp, abortcall = sys.call())
    data.table::set(x, j = col, value = rp)
  }
  
  if(!is.null(row)) {
    row <- as.integer(row)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- .lapply(collapse::ss(x, i = row, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp, abortcall = sys.call())
    data.table::set(x, i = row, j = col, value = rp)
  }
  
  return(invisible(NULL))
}


#' @keywords internal
#' @noRd
.sb_set_atomic <- function(x, elements, rp, tf, abortcall) {
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
    rp = tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall)
  if(!is.complex(x)) {
    collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
    return(invisible(NULL))
  }
  if(is.complex(x)) {
    .rcpp_setvind_Complex(x, as.integer(elements - 1L), as.complex(rp))
    return(invisible(NULL))
  }
  
}


