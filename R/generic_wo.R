#' Method to Return Object Without Specified Subset
#'
#' @description
#' This is an S3 Method to return an object \bold{without} the specified subset. \cr
#' `sb_wo()`/ `sb2_wo()` is essentially the inverse of \link{sb_x}/\link{sb2_x}. \cr
#' Use `sb_wo(x, ...)` if `x` is an atomic object. \cr
#' Use `sb2_wo(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,row,col,sub,dims,filter,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection results in nothing being removed,
#' and the entire object is returned. \cr
#' @param red Boolean, for recursive objects only,
#' indicating if the result should be reduced. \cr
#' If `red = TRUE`,
#' selecting a single element will give the simplified result,
#' like using `[[]]`. \cr
#' If `red = FALSE`, a list is always returned regardless of the number of elements. \cr
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#'
#'
#'
#' @returns
#' A copy of the sub-setted object.
#'
#'
#' @example inst/examples/generic_wo.R
#'
#'

#' @rdname sb_wo
#' @export
sb_wo <- function(x, ...) {
  
  if(is.list(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  if(!is.atomic(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb_wo", x)
}


#' @rdname sb_wo
#' @export
sb_wo.default <- function(
    x, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(is.null(i)){
    return(x)
  }
  
  elements <- ci_flat(x, i, inv = TRUE, chkdup = chkdup, .abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_wo
#' @export
sb_wo.array <- function(
    x, sub = NULL, dims = 1:ndims(x), i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, sub, dims, i, TRUE, FALSE, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
sb2_wo <- function(x, ...) {
  
  if(is.atomic(x)) {
    stop("Use the `sb_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb2_wo", x)
}


#' @rdname sb_wo
#' @export
sb2_wo.default <- function(
    x, i = NULL, red = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  if(is.null(i)) {
    return(x)
  }
  
  return(.flat_x(x, i, TRUE, red, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
sb2_wo.array <- function(
    x, sub = NULL, dims = 1:ndims(x), i = NULL, red = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  return(.sb_x_array(x, sub, dims, i, TRUE, red, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
sb2_wo.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(.all_NULL_indices(list(row, col, filter, vars))) {
    return(x)
  }
  
  
  if(!is.null(row)) { row <- ci_df(
    x, row, 1L, inv = TRUE, chkdup = chkdup, .abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- ci_df(
    x, col, 2L, inv = TRUE, chkdup = chkdup, .abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = TRUE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = TRUE, abortcall = sys.call())
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  out <- collapse::ss(x, row, col, check = FALSE)
  
  names(out) <- make.names(names(out), unique = TRUE)
  return(out)
}


