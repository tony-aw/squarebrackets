#' Method to Modify Subsets of a Mutable Object By Reference
#'
#' @description
#' This is an S3 Method to replace or transform a subset of a
#' \link[=squarebrackets_supported_structures]{supported mutable object}
#' using
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics} \cr
#' Use `sb_set(x, ...)` if `x` is an atomic object (i.e. \link{mutable_atomic}). \cr
#' Use `sb2_set(x, ...)` if `x` is a recursive object (i.e. \link{data.table}). \cr \cr
#' 
#'
#' @param x a \bold{variable} belonging to one of the
#' \link[=squarebrackets_supported_structures]{supported mutable classes}. \cr
#' @param i,row,col,sub,dims,filter,vars,inv See \link{squarebrackets_indx_args}. \cr
#' An empty index selection leaves the original object unchanged. \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp,tf,.lapply see \link{squarebrackets_modify}.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' 
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset.
#' Specifying `rp` will replace the subset.
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
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
  
  UseMethod("sb_set", x)
}


#' @rdname sb_set
#' @export
sb_set.default <- function(
    x, i = NULL, inv = FALSE, ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  .internal_check_dots(list(...), sys.call())
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  # function:
  if(is.null(i)) {
    .rcpp_set_all(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  elements <- ci_flat(
    x, i, inv, chkdup, .abortcall = sys.call()
  )
  .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
  return(invisible(NULL))
}


#' @rdname sb_set
#' @export
sb_set.matrix <- function(x, row = NULL, col = NULL, i = NULL, inv = FALSE, ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)) {
  
  # error checks:
  .internal_check_dots(list(...), sys.call())
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  # function:
  if(.all_NULL_indices(list(row, col, i))) {
    .rcpp_set_all(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  if(!is.null(i)) {
    elements <- ci_flat(
      x, i, inv, chkdup, .abortcall = sys.call()
    )
    return(.sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call()))
    
  }


  if(!is.null(row)) {
    row <- ci_margin(x, row, 1L, inv, chkdup, .abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- ci_margin(x, col, 2L, inv, chkdup, .abortcall = sys.call())
  }
  
  if(.any_empty_indices(n(row, col))) {
    return(invisible(NULL))
  }
  
  
  .set_mat(x, row, col, rp, tf, abortcall = sys.call())
  return(invisible(NULL))

}


#' @rdname sb_set
#' @export
sb_set.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, inv = FALSE, ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  .internal_check_dots(list(...), sys.call())
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  # function:
  if(.all_NULL_indices(list(sub, dims, i))) {
    .rcpp_set_all(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  if(!is.null(i)) {
    elements <- ci_flat(
      x, i, inv, chkdup, .abortcall = sys.call()
    )
    .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  if(length(sub) == 0 && length(dims) == 0) {
    .rcpp_set_all(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  
  .arr_set(x, sub, dims, chkdup, inv, rp, tf, abortcall = sys.call())
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
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, inv = FALSE,
    ..., rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # error checks:
  .internal_check_dots(list(...), sys.call())
  if(!data.table::is.data.table(x)) {
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  # function:
  if(!is.null(row)) { row <- ci_df(
    x,  row, 1L, inv, chkdup, .abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- ci_df(
    x, col, 2L, inv, chkdup, .abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = inv, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = inv, abortcall = sys.call())
  }
  
  if(.any_empty_indices(n(row, col))) {
    return(invisible(NULL))
  }
  
  if(is.null(col)) col <- collapse::seq_col(x)
  col <- as.integer(col)
  
  
  if(is.null(row)) {
    if(!missing(tf)) {
      rp <- .lapply(collapse::ss(x, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp, abortcall = sys.call())
    data.table::set(x, j = col, value = rp)
  }
  
  if(!is.null(row)) {
    row <- as.integer(row)
    if(!missing(tf)) {
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
  
  .internal_check_rptf(rp, tf, abortcall)
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall)
  
  .rcpp_set_vind(x, elements, rp, abortcall)
  return(invisible(NULL))
  
}



