#' Method to Un-Select/Remove Subsets of an Object
#'
#' @description
#' This is an S3 Method to un-select/remove subsets from an object. \cr
#' Use `sb_rm(x, ...)` if `x` is a non-recursive object (i.e. atomic or factor). \cr
#' Use `sb2_rm(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr \cr
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,lvl,row,col,sub,dims,filter,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection results in nothing being removed,
#' and the entire object is returned. \cr
#' @param drop Boolean.
#'  * For factors: If `drop = TRUE`, unused levels are dropped, if `drop = FALSE` they are not dropped.
#'  * For lists: if `drop = TRUE`, selecting a single element will give the simplified result,
#'  like using `[[]]`.
#'  If `drop = FALSE`, a list is always returned regardless of the number of elements.
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
  
  if(is.recursive(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  
  UseMethod("sb_rm", x)
}


#' @rdname sb_rm
#' @export
sb_rm.default <- function(
    x, i, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  elements <- .indx_make_element(
    i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call()
  )
  return(x[elements])
}


#' @rdname sb_rm
#' @export
sb_rm.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
    return(x[elements])
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
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


#' @rdname sb_rm
#' @export
sb_rm.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_rm_array(x, sub, dims, i, chkdup, sys.call()))
}


#' @rdname sb_rm
#' @export
sb_rm.factor <- function(
    x, i = NULL, lvl = NULL, drop = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .check_args_factor(i, lvl, drop, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
    return(x[elements, drop = drop])
  }
  if(!is.null(lvl)) {
    indx <- .lvl2indx(lvl, x, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
    return(x[indx, drop = drop])
  }
}


#' @rdname sb_rm
#' @export
sb2_rm <- function(x, ...) {
  
  if(!is.recursive(x)) {
    stop("Use the `sb_` methods for non-recursive objects")
  }
  
  UseMethod("sb2_rm", x)
}


#' @rdname sb_rm
#' @export
sb2_rm.default <- function(
    x, i, drop = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  elements <- .indx_make_element(i, x, is_list = TRUE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && drop) {
    return(x[[elements]])
  }
  else {
    return(x[elements])
  }
}


#' @rdname sb_rm
#' @export
sb2_rm.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, drop = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  x <- .sb_rm_array(x, sub, dims, i, chkdup, sys.call())
  
  if(length(x) == 1 && drop) {
    return(x[[1]])
  }
  
  return(x)
}


#' @rdname sb_rm
#' @export
sb2_rm.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, chkdup = chkdup, inv = TRUE, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = chkdup, inv = TRUE, abortcall = sys.call()
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



#' @keywords internal
#' @noRd
.sb_rm_array <- function(x, sub, dims, i, chkdup, abortcall) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = abortcall
    )
    return(x[elements])
  }
  
  if(length(sub) == 0L && length(dims) == 0L) {
    return(x)
  }
  
  if(length(dims) == 1L && !is.list(sub)) {
    sub <- list(sub)
  }
  
  lst <- .arr_lst_brackets(x, sub, dims, chkdup, inv = TRUE, abortcall = abortcall)
  
  if(is.null(names(x))) {
    x <- .arr_x(x, lst, abortcall = abortcall)
  }
  else {
    x <- .internal_fix_names(x, \(x).arr_x(x, lst, abortcall = abortcall))
  }
  
  return(x)
}
