#' Method to Extract, Exchange, or Duplicate Subsets of an Object
#'
#' @description
#' This is an S3 Method to extract, exchange,
#' or duplicate (i.e. repeat x times) subsets of an object. \cr
#' Use `sb_x(x, ...)` if `x` is an atomic object. \cr
#' Use `sb2_x(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,row,col,sub,dims,filter,vars See \link{squarebrackets_indx_args}. \cr
#' Duplicates are allowed, resulting in duplicated indices. \cr
#' An empty index selection results in an empty object of length 0. \cr
#' @param red Boolean, for lists only, indicating if the result should be reduced. \cr
#' If `red = TRUE`,
#' selecting a single element with non-empty arguments will give the simplified result,
#' like using `[[]]`. \cr
#' If `red = FALSE`, a list is always returned regardless of the number of elements.
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
  
  if(is.list(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  if(!is.atomic(x)) {
    stop("unsupported object")
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
  return(.sb_x_matrix(x, row, col, i, FALSE, FALSE, FALSE, sys.call()))
}


#' @rdname sb_x
#' @export
sb_x.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, sub, dims, i, FALSE, FALSE, FALSE, sys.call()))
  
}


#' @rdname sb_x
#' @export
sb2_x <- function(x, ...) {
  
  if(is.atomic(x)) {
    stop("Use the `sb_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb2_x", x)
}


#' @rdname sb_x
#' @export
sb2_x.default <- function(x, i = NULL, red = FALSE, ...) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  if(is.null(i)) {
    return(x)
  }
  
  elements <- ci_flat(x, i, .abortcall = sys.call())
  
  n.i <- length(elements)
  
  if(n.i == 1 && red) {
    return(x[[elements]])
  }
  
  return(x[elements])
}


#' @rdname sb_x
#' @export
sb2_x.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, red = FALSE, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_matrix(x, row, col, i, FALSE, red, FALSE, sys.call()))
}


#' @rdname sb_x
#' @export
sb2_x.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, red = FALSE, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  return(.sb_x_array(x, sub, dims, i, FALSE, red, FALSE, abortcall = sys.call()))
  
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
.sb_x_array <- function(
    x, sub = NULL, dims = NULL, i = NULL, inv = FALSE, red = FALSE, chkdup = FALSE, abortcall
) {
  
  # empty arguments:
  if(.all_NULL_indices(list(sub, dims, i))) {
    return(x)
  }
  
  
  # argument i:
  if(!is.null(i)) {
    elements <- ci_flat(x, i, inv = inv, chkdup = chkdup, .abortcall = abortcall)
    if(red && length(elements) == 1L) {
      return(x[[elements]])
    }
    return(x[elements])
  }
  
  
  # zero-length subscripts:
  if(length(sub) == 0L && length(dims) == 0L) {
    return(x)
  }
  
  
  # sub, dims arguments:
  lst <- ci_sub(x, sub, dims, inv = inv, chkdup, .abortcall = abortcall)
  
  if(red && collapse::allv(collapse::vlengths(lst), 1L)) {
    x <- .arr_x(x, lst, abortcall = abortcall)
    return(x[[1L]])
  }
  else if(is.null(names(x))) {
    return(.arr_x(x, lst, abortcall = abortcall))
  }
  else {
    return(.internal_fix_names(x, \(x).arr_x(x, lst, abortcall = abortcall)))
  }
}


#' @keywords internal
#' @noRd
.sb_x_matrix <- function(
    x, row = NULL, col = NULL, i = NULL, inv = FALSE, red = FALSE, chkdup = FALSE, abortcall
) {
  
  # empty arguments:
  if(.all_NULL_indices(list(row, col, i))) {
    return(x)
  }
  
  
  # argument i:
  if(!is.null(i)) {
    elements <- ci_flat(x, i, inv = inv, chkdup = chkdup, .abortcall = abortcall)
    if(red && length(elements) == 1L) {
      return(x[[elements]])
    }
    return(x[elements])
  }
  
  # prep row, col:
  if(!is.null(row)) {
    row <- ci_margin(x, row, 1L, inv = inv, chkdup = chkdup, .abortcall = abortcall)
  }
  if(!is.null(col)) {
    col <- ci_margin(x, col, 2L, inv = inv, chkdup = chkdup, .abortcall = abortcall)
  }
  
  
  # col:
  if(is.null(row)) {
    if(red && length(col) == 1L && nrow(x) == 1L) {
      return(x[[, col]])
    }
    else if(is.null(names(x))) {
      return(x[, col, drop = FALSE])
    }
    else {
      return(.internal_fix_names(x, \(x)x[, col, drop = FALSE]))
    }
  }
  
  
  # row:
  if(is.null(col)) {
    if(red && length(row) == 1L && ncol(x) == 1L) {
      return(x[[row, ]])
    }
    else if(is.null(names(x))) {
      return(x[row, , drop = FALSE])
    }
    else {
      return(.internal_fix_names(x, \(x)x[row, , drop = FALSE]))
    }
  }
  
  
  # row & col:
  if(red && length(row) == 1L && length(col) == 1L) {
    return(x[[row, col]])
  }
  else if(is.null(names(x))) {
    return(x[row, col, drop = FALSE])
  }
  else {
    return(.internal_fix_names(x, \(x)x[row, col, drop = FALSE]))
  }
}