#' Method to Extract, Exchange, or Duplicate Subsets of an Object
#'
#' @description
#' This is an S3 Method to extract, exchange,
#' or duplicate (i.e. repeat x times) subsets of an object. \cr
#' Use `sb_x(x, ...)` if `x` is a non-recursive object (i.e. atomic or factor). \cr
#' Use `sb2_x(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,lvl,row,col,idx,dims,filter,vars See \link{squarebrackets_indx_args}. \cr
#' Duplicates are allowed, resulting in duplicated indices. \cr
#' An empty index selection results in an empty object of length 0. \cr
#' @param drop Boolean.
#'  * For factors: If `drop = TRUE`, unused levels are dropped, if `drop = FALSE` they are not dropped.
#'  * For lists: if `drop = TRUE`,
#'  and sub-setting is done using argument `i`,
#'  selecting a single element will give the simplified result,
#'  like using `[[]]`.
#'  If `drop = FALSE`, a list is always returned regardless of the number of elements.
#' @param ... further arguments passed to or from other methods.
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
sb_x.default <- function(x, i, ...) {
  elements <- .indx_make_element.sb_x(i, x, is_list = FALSE, abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_x
#' @export
sb_x.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, ...
) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element.sb_x(i, x, is_list = FALSE, abortcall = sys.call())
    return(x[elements])
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim.sb_x(row, x,  1, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim.sb_x(col, x,  2, abortcall = sys.call())
  }
  
  if(is.null(row) && is.null(col)) {
    return(x)
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
    x, idx = NULL, dims = NULL, i = NULL, ...
) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element.sb_x(i, x, is_list = FALSE, abortcall = sys.call())
    return(x[elements])
  }
  
  if(is.null(names(x))) {
    x <- .arr_x(x, idx, dims, abortcall = sys.call())
  }
  else {
    x <- .internal_fix_names(x, \(x).arr_x(x, idx, dims, abortcall = sys.call()))
  }
  
  return(x)
  
}

#' @rdname sb_x
#' @export
sb_x.factor <- function(x, i = NULL, lvl = NULL, drop = FALSE, ...) {
  
  .check_args_factor(i, lvl, drop, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element.sb_x(i, x, is_list = FALSE, abortcall = sys.call())
    return(x[elements, drop = drop])
  }
  if(!is.null(lvl)) {
    indx <- .lvl2indx.sb_x(lvl, x, abortcall = sys.call())
    return(x <- x[indx, drop = drop])
  }
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
sb2_x.default <- function(x, i, drop = FALSE, ...) {
  
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop("`drop` must be either `TRUE` or `FALSE`")
  }
  
  elements <- .indx_make_element.sb_x(i, x, is_list = TRUE, abortcall = sys.call())
  
  n.i <- length(elements)
  
  if(n.i == 1 && drop) {
    return(x[[elements]])
  }
  
  return(x[elements])
}


#' @rdname sb_x
#' @export
sb2_x.array <- function(
    x, idx = NULL, dims = NULL, i = NULL, drop = FALSE, ...
) {
  
  if(!is.null(i)) {
    return(sb2_x.default(x, i, drop = drop, ...))
  }
  
  if(is.null(names(x))) {
    x <- .arr_x(x, idx, dims, abortcall = sys.call())
  }
  else {
    x <- .internal_fix_names(x, \(x).arr_x(x, idx, dims, abortcall = sys.call()))
  }
  
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
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind.sb_x(
    row, x,  1, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind.sb_x(
    col, x,  2, abortcall = sys.call()
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

