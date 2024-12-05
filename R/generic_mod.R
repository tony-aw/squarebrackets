#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' This is an S3 Method to return a copy of an object with modified subsets. \cr
#' Use `sb_mod(x, ...)` if `x` is an atomic object; this returns a full copy. \cr
#' Use `sb2_mod(x, ...)` if `x` is a recursive object
#' (i.e. list or data.frame-like);
#' this returns a partial copy. \cr
#' \cr
#' For modifying subsets using R's default copy-on-modification semantics, see \link{idx}. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,row,col,sub,dims,filter,vars,inv See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp,tf,.lapply see \link{squarebrackets_modify}.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")`
#' 
#' 
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset. \cr
#' Specifying `rp` will replace the subset. \cr
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' \cr
#' 
#' @returns
#' A copy of the object with replaced/transformed values. \cr \cr
#'
#'
#' @example inst/examples/generic_mod.R
#' 

#' @rdname sb_mod
#' @export
sb_mod <- function(x, ...) {
  
  if(is.list(x)) {
    stop("Use the `sb2_` methods for recursive objects")
  }
  if(!is.atomic(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb_mod", x)
}


#' @rdname sb_mod
#' @export
sb_mod.default <- function(
    x, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  if(is.null(i)) {
    return(.all_mod_atomic(x, rp, tf, sys.call()))
  }
  
  return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
}


#' @rdname sb_mod
#' @export
sb_mod.array <- function(
    x, sub = NULL, dims = 1:ndims(x), i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, sub, dims, sys.call())
  
  # all empty indices:
  if(.all_NULL_indices(list(sub, i))) {
    return(.all_mod_atomic(x, rp, tf,sys.call()))
  }
  
  # argument i:
  if(!is.null(i)) {
    return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
  }
  
  # zero-length subscripts:
  if(length(dims) == 0) {
    return(.all_mod_atomic(x, rp, tf, sys.call()))
  }
  
  # 1d:
  if(ndims(x) == 1L) {
    i <- .flat_sub2i(x, sub, dims)
    return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
  }
  
  # matrix:
  if(is.matrix(x)) {
    return(.mat_mod_atomic(x, sub, dims, inv, rp, tf, chkdup, sys.call()))
  }
  
  # sub, dims arguments:
  lst <- ci_sub(x, sub, dims, inv, chkdup, .abortcall = sys.call())

  if(!missing(rp)) {
    if(!is.atomic(rp)) stop("replacement must be atomic")
    return(.arr_repl(x, lst, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    return(.arr_tf(x, lst, tf, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb2_mod <- function(x, ...) {
  
  if(is.atomic(x)) {
    stop("Use the `sb_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb2_mod", x)
}


#' @rdname sb_mod
#' @export
sb2_mod.default <- function(
    x, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  
  if(is.null(i)) {
    return(.all_mod_list(x, rp, tf, .lapply, sys.call()))
  }
  
  return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
}



#' @rdname sb_mod
#' @export
sb2_mod.array <- function(
    x, sub = NULL, dims = 1:ndims(x), i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, sub, dims, sys.call())
  
  # all empty indices:
  if(.all_NULL_indices(list(sub, i))) {
    return(.all_mod_list(x, rp, tf, .lapply, sys.call()))
  }
  
  # argument i:
  if(!is.null(i)) {
    return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  # zero-length subscripts:
  if(length(dims) == 0) {
    return(.all_mod_list(x, rp, tf, .lapply, sys.call()))
  }
  
  # 1d:
  if(ndims(x) == 1L) {
    i <- .flat_sub2i(x, sub, dims)
    return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  # matrix:
  if(is.matrix(x)) {
    return(.mat_mod_list(x, sub, dims, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  lst <- ci_sub(x, sub, dims, inv, chkdup, .abortcall = sys.call())
  
  if(!missing(rp)) {
    if(!is.list(rp)) stop("replacement must be a list")
    return(.arr_repl_list(x, lst, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    return(.arr_tf_list(x, lst, tf, .lapply, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb2_mod.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  # make args:
  if(!is.null(row)) { row <- ci_df(
    x, row, 1L, inv, chkdup, .abortcall = sys.call()
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
  
  # empty return:
  if(.any_empty_indices(n(row, col))) {
    return(x)
  }
  
  # prep col:
  if(is.null(col)) {
    message("copying all columns")
    col <- as.integer(1:ncol(x))
  }
  
  # copy specified columns, but not the rest of the data.frame:
  x <- collapse::ftransformv(x, vars = col, FUN = data.table::copy, apply = TRUE)
  
  # prep replacement just in case:
  if(!missing(rp)) {
    rp <- .dt_prep_rp(rp)
  }
  
  # tramsformation:
  if(!missing(tf)) {
    rp <- .dt_transform(x, row, col, tf, .lapply)
  }
  
  # modify:
  if(is.null(row)) {
    return(.dt_mod_whole(x, col, rp, abortcall = sys.call()))
  }
  needcoe <- .dt_check_needcoe(x, col, rp)
  if(needcoe) {
    return(.dt_mod_partialcoe(x, row, col, rp, sys.call()))
  }
  else {
    return(.dt_mod_partialset(x, row, col, rp, sys.call()))
  }
  
}


