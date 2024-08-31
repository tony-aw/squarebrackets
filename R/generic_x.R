#' Method to Extract, Exchange, or Duplicate Subsets of an Object
#'
#' @description
#' This is an S3 Method to extract, exchange,
#' or duplicate (i.e. repeat x times) subsets of an object. \cr
#' Use `sb_x(x, ...)` if `x` is a non-recursive object (i.e. atomic). \cr
#' Use `sb2_x(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,row,col,sub,dims,filter,vars See \link{squarebrackets_indx_args}. \cr
#' Duplicates are allowed, resulting in duplicated indices. \cr
#' An empty index selection results in an empty object of length 0. \cr
#' @param drop Boolean, for lists only. \cr
#' If `drop = TRUE`,
#' selecting a single element will give the simplified result,
#' like using `[[]]`. \cr
#' If `drop = FALSE`, a list is always returned regardless of the number of elements.
#' @param ... see \link{squarebrackets_method_dispatch}.
#'
#'
#'
#' @returns
#' Returns a copy of the sub-setted object.
#'
#'
#'
#' @example inst/examples/generic_x.R
#' 

#' @rdname sb_x
#' @export
sb_x <- function(x, ...) {
  
  if(is.recursive(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  
  UseMethod("sb_x", x)
}


#' @rdname sb_x
#' @export
sb_x.default <- function(x, i = NULL, ...) {
  
  .internal_check_dots(list(...), sys.call())
  if(is.null(i)){
    return(x)
  }
  
  elements <- ci_flat(x, i, .abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_x
#' @export
sb_x.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  if(.all_NULL_indices(list(row, col, i))) {
    return(x)
  }
  
  if(!is.null(i)) {
    elements <- ci_flat(x, i, .abortcall = sys.call())
    return(x[elements])
  }
  
  if(!is.null(row)) {
    row <- ci_margin(x, row, 1L, .abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- ci_margin(x, col, 2L, .abortcall = sys.call())
  }
  
  
  if(is.null(row)) {
    if(is.null(names(x))) {
      x <- x[, col, drop = FALSE]
    }
    else {
      x <- .internal_fix_names(x, \(x)x[, col, drop = FALSE])
    }
    return(x)
  }
  if(is.null(col)) {
    if(is.null(names(x))) {
      x <- x[row, , drop = FALSE]
    }
    else {
      x <- .internal_fix_names(x, \(x)x[row, , drop = FALSE])
    }
    return(x)
  }
  
  if(is.null(names(x))) {
    x <- x[row, col, drop = FALSE]
  } else {
    x <- .internal_fix_names(x, \(x)x[row, col, drop = FALSE])
  }
  return(x)
}


#' @rdname sb_x
#' @export
sb_x.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, sub, dims, i, sys.call()))
  
}


#' @rdname sb_x
#' @export
sb2_x <- function(x, ...) {
  
  if(!is.recursive(x)) {
    stop("Use the `sb_` methods for non-recursive objects")
  }
  
  UseMethod("sb2_x", x)
}


#' @rdname sb_x
#' @export
sb2_x.default <- function(x, i = NULL, drop = FALSE, ...) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop("`drop` must be either `TRUE` or `FALSE`")
  }
  if(is.null(i)) {
    if(length(x) == 1L && drop) {
      return(x[[1]])
    }
    else {
      return(x)
    }
  }
  
  elements <- ci_flat(x, i, .abortcall = sys.call())
  
  n.i <- length(elements)
  
  if(n.i == 1 && drop) {
    return(x[[elements]])
  }
  
  return(x[elements])
}


#' @rdname sb_x
#' @export
sb2_x.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, drop = FALSE, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop("`drop` must be either `TRUE` or `FALSE`")
  }
  
  x <- .sb_x_array(x, sub, dims, i, sys.call())
  
  if(length(x) == 1 && drop) {
    return(x[[1]])
  }
  return(x)
  
}

#' @rdname sb_x
#' @export
sb2_x.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(.all_NULL_indices(list(row, col, filter, vars))) {
    return(x)
  }
  
  if(!is.null(row)) { row <- ci_df(
    x, row, 1L, .abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- ci_df(
    x, col, 2L, uniquely_named = TRUE, .abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  x <- collapse::ss(x, row, col, check = FALSE)
  
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}



#' @keywords internal
#' @noRd
.sb_x_array <- function(x, sub = NULL, dims = NULL, i = NULL, abortcall) {
  
  if(.all_NULL_indices(list(sub, dims, i))) {
    return(x)
  }
  
  if(!is.null(i)) {
    elements <- ci_flat(x, i, .abortcall = sys.call())
    return(x[elements])
  }
  
  if(length(sub) == 0L && length(dims) == 0L) {
    return(x)
  }
  
  
  lst <- ci_sub(x, sub, dims, .abortcall = sys.call())
  
  if(is.null(names(x))) {
    x <- .arr_x(x, lst, abortcall = sys.call())
  }
  else {
    x <- .internal_fix_names(x, \(x).arr_x(x, lst, abortcall = sys.call()))
  }
  
  return(x)
}
