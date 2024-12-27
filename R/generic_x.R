#' Method to Extract, Exchange, or Duplicate Subsets of an Object
#'
#' @description
#' This is an S3 Method to extract, exchange,
#' or duplicate (i.e. repeat x times) subsets of an object. \cr
#' Use `sb_x(x, ...)` if `x` is an atomic object. \cr
#' Use `sb2_x(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,s,d,obs,vars See \link{squarebrackets_indx_args}. \cr
#' Duplicates are allowed, resulting in duplicated indices. \cr
#' An empty index selection results in an empty object of length 0. \cr
#' @param red Boolean, for recursive objects only,
#' indicating if the result should be reduced. \cr
#' If `red = TRUE`,
#' selecting a single element will give the simplified result,
#' like using `[[]]`. \cr
#' If `red = FALSE`, a list is always returned regardless of the number of elements. \cr
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
sb_x.array <- function(
    x, s = NULL, d = 1:ndims(x), i = NULL, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, s, d, i, FALSE, FALSE, FALSE, sys.call()))
  
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
  
  return(.flat_x(x, i, FALSE, red, FALSE, sys.call()))
}



#' @rdname sb_x
#' @export
sb2_x.array <- function(
    x, s = NULL, d = 1:ndims(x), i = NULL, red = FALSE, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  return(.sb_x_array(x, s, d, i, FALSE, red, FALSE, abortcall = sys.call()))
  
}

#' @rdname sb_x
#' @export
sb2_x.data.frame <- function(
    x, s = NULL, d = 1:2, obs = NULL, vars = NULL, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  .check_args_df(x, s, d, obs, vars, abortcall = sys.call())
  
  # all missing arguments:
  if(.all_missing_s_d(s, d) && .all_NULL_indices(list(obs, vars))) {
    return(x)
  }
  
  # make arguments:
  rowcol <- .dt_make_args(x, s, d, obs, vars, FALSE, FALSE, sys.call())
  row <- rowcol[[1L]]
  col <- rowcol[[2L]]
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  x <- collapse::ss(x, row, col, check = FALSE)
  
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}



#' @keywords internal
#' @noRd
#' @keywords internal
#' @noRd
.sb_x_array <- function(
    x, s = NULL, d = 1:ndims(x), i = NULL, inv = FALSE, red = FALSE, chkdup = FALSE, abortcall
) {
  
  # check arguments:
  .check_args_array(x, s, d, abortcall)
  
  # empty arguments:
  if(.all_NULL_indices(list(s, i))) {
    return(x)
  }
  
  # argument i:
  if(!is.null(i)) {
    return(.flat_x(x, i, inv, red, chkdup, abortcall))
  }
  
  # zero-length subscripts:
  if(length(d) == 0L) {
    return(x)
  }
  
  # 1d:
  if(ndims(x) == 1L) {
    i <- .flat_s2i(x, s, d, abortcall)
    return(.flat_a1d_x(x, i, inv, red, chkdup, abortcall))
  }
  
  # matrix:
  if(is.matrix(x)) {
    return(.mat_x(x, s, d, inv, red, chkdup, sys.call()))
  }
  
  
  # s, d arguments:
  lst <- ci_sub(x, s, d, inv = inv, chkdup, .abortcall = abortcall)
  
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


