#' Method to Un-Select/Remove Subsets of an Object
#'
#' @description
#' This is an S3 Method to un-select/remove subsets from an object. \cr
#' Use `sb_rm(x, ...)` if `x` is an atomic object. \cr
#' Use `sb2_rm(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,row,col,sub,dims,filter,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection results in nothing being removed,
#' and the entire object is returned. \cr
#' @param red Boolean, for list only. \cr
#' I f `red = TRUE`,
#' selecting a single element with non-empty arguments will give the simplified result,
#' like using `[[]]`. \cr
#' If `red = FALSE`, a list is always returned regardless of the number of elements.
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
#' @example inst/examples/generic_rm.R
#'
#'

#' @rdname sb_rm
#' @export
sb_rm <- function(x, ...) {
  
  if(is.list(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  if(!is.atomic(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb_rm", x)
}


#' @rdname sb_rm
#' @export
sb_rm.default <- function(
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


#' @rdname sb_rm
#' @export
sb_rm.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_matrix(x, row, col, i, TRUE, FALSE, chkdup, sys.call()))
}


#' @rdname sb_rm
#' @export
sb_rm.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, sub, dims, i, TRUE, FALSE, chkdup, sys.call()))
}


#' @rdname sb_rm
#' @export
sb2_rm <- function(x, ...) {
  
  if(is.atomic(x)) {
    stop("Use the `sb_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb2_rm", x)
}


#' @rdname sb_rm
#' @export
sb2_rm.default <- function(
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
  
  elements <- ci_flat(x, i, inv = TRUE, chkdup = chkdup, .abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && red) {
    return(x[[elements]])
  }
  else {
    return(x[elements])
  }
}

#' @rdname sb_rm
#' @export
sb2_rm.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, red = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_matrix(x, row, col, i, TRUE, red, chkdup, sys.call()))
}


#' @rdname sb_rm
#' @export
sb2_rm.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, red = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  return(.sb_x_array(x, sub, dims, i, TRUE, red, chkdup, sys.call()))
}


#' @rdname sb_rm
#' @export
sb2_rm.data.frame <- function(
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


