#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' Methods to return a copy of an object with modified subsets. \cr
#' For modifying subsets using R's default copy-on-modification semantics, see \link{idx}. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,s,d,obs,vars,inv See \link{squarebrackets_indx_args}. \cr
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
fi_mod <- function(x, ...) {
  
  .methodcheck.i(x, sys.call())
  
  UseMethod("fi_mod", x)
}

#' @rdname sb_mod
#' @export
fi2_mod <- function(x, ...) {
  
  .methodcheck.i2(x, sys.call())
  
  UseMethod("fi2_mod", x)
}

#' @rdname sb_mod
#' @export
ss_mod <- function(x, ...) {
  
  .methodcheck.ss(x, sys.call())
  UseMethod("ss_mod", x)
}


#' @rdname sb_mod
#' @export
ss2_mod <- function(x, ...) {
  
  .methodcheck.ss2(x, sys.call())
  
  
  UseMethod("ss2_mod", x)
}
#' @rdname sb_mod
#' @export
fi_mod.default <- function(
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
ss_mod.default <- function(
    x, s = NULL, d = 1:ndim(x), inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, s, d, sys.call())
  
  # all empty indices:
  if(.all_NULL_indices(list(s))) {
    return(.all_mod_atomic(x, rp, tf,sys.call()))
  }
  
  # zero-length subscripts:
  if(length(d) == 0) {
    return(.all_mod_atomic(x, rp, tf, sys.call()))
  }
  
  # 1d:
  if(ndim(x) == 1L) {
    i <- .flat_s2i(x, s, d)
    return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
  }
  
  # matrix:
  if(is.matrix(x)) {
    return(.mat_mod_atomic(x, s, d, inv, rp, tf, chkdup, sys.call()))
  }
  
  # s, d arguments:
  lst <- ci_sub(x, s, d, inv, chkdup, .abortcall = sys.call())

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
fi2_mod.default <- function(
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
ss2_mod.default <- function(
    x, s = NULL, d = 1:ndim(x), inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, s, d, sys.call())
  
  # all missing indices:
  if(.all_NULL_indices(list(s))) {
    return(.all_mod_list(x, rp, tf, .lapply, sys.call()))
  }
  
  # zero-length subscripts:
  if(length(d) == 0) {
    return(.all_mod_list(x, rp, tf, .lapply, sys.call()))
  }
  
  # 1d:
  if(ndim(x) == 1L) {
    i <- .flat_s2i(x, s, d)
    return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  # matrix:
  if(is.matrix(x)) {
    return(.mat_mod_list(x, s, d, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  lst <- ci_sub(x, s, d, inv, chkdup, .abortcall = sys.call())
  
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
ss2_mod.data.frame <- function(
    x, s = NULL, d = 1:2, obs = NULL, vars = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_df(x, s, d, obs, vars, abortcall = sys.call())
  
  # make arguments:
  rowcol <- .dt_make_args(x, s, d, obs, vars, inv, chkdup, sys.call())
  row <- rowcol[[1L]]
  col <- rowcol[[2L]]
  # don't use if(is.null(row or col)) row or col <- 1:... -> will mess up the rest of this function
  
  # empty indices:
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


