#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' Methods to return a copy of an object with modified subsets. \cr
#' For modifying subsets using R's default copy-on-modification semantics, 
#' see the `_icom` methods. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,use,s,row,col See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp,tf see \link{squarebrackets_modify}.
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
ii_mod <- function(x, i = NULL, use = 1, ..., rp, tf) {
  
  .methodcheck.ii(x, i, use, sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  UseMethod("ii_mod", x)
}


#' @rdname sb_mod
#' @export
ss_mod <- function(x, s = NULL, use = rdim(x), ..., rp, tf) {
  
  .methodcheck.ss(x, s, use, sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  UseMethod("ss_mod", x)
}


#' @rdname sb_mod
#' @export
sbt_mod <- function(x, row = NULL, col = NULL, use = 1:2, ..., rp, tf) {
  
  .methodcheck.sbt(x, row, col, use, sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  UseMethod("sbt_mod", x)
}





#' @rdname sb_mod
#' @export
ii_mod.default <- function(
    x, i = NULL, use = 1, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  
  
  if(is.list(x) && !missing(tf)) {
    tf <- .funply(tf)
  }
  
  if(.C_is_missing_idx(i)) {
    return(.all_mod(x, rp, tf, sys.call()))
  }
  
  return(.flat_mod(x, i, use, rp, tf, chkdup, sys.call()))
}


#' @rdname sb_mod
#' @export
ss_mod.default <- function(
    x, s = NULL, use = rdim(x), ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  return(.sb_mod_array(x, s, use, chkdup, rp, tf, sys.call()))
}



#' @rdname sb_mod
#' @export
sbt_mod.default <- function(
    x, row = NULL, col = NULL, use = 1:2, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  use <- .internal_make_use_tabular(use, sys.call())
  return(.sb_mod_array(x, n(row, col), use, chkdup, rp, tf, sys.call()))
}


#' @rdname sb_mod
#' @export
sbt_mod.data.frame <- function(
    x, row = NULL, col = NULL, use = 1:2, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  
  if(length(x) == 0L) {
    return(x)
  }
  
  # make arguments:
  use <- .internal_make_use_tabular(use, sys.call())
  if(!.C_is_missing_idx(row)) {
    row <- ci_margin(
      x, row, 1L, use[1], chkdup = FALSE, uniquely_named = TRUE, sys.call()
    )
  }
  if(is.function(col)) {
    col <- collapse::get_vars(x, col, return = "logical")
    if(use[2] > 0) col <- which(col)
    if(use[2] < 0) col <- collapse::whichv(col, FALSE)
  }
  else if(!.C_is_missing_idx(col)) {
    col <- ci_margin(
      x, col, 2L, use[2], chkdup = FALSE, uniquely_named =  TRUE, sys.call()
    )
  }
  
  # empty indices:
  if(.any_empty_indices(n(row, col))) {
    return(x)
  }
  
  # prep col:
  if(.C_is_missing_idx(col)) {
    message("copying all columns")
    col <- seq_len(ncol(x))
  }
  
  # copy specified columns, but not the rest of the data.frame:
  x <- collapse::ftransformv(x, col, data.table::copy, apply = TRUE)
  
  # prep replacement just in case:
  if(!missing(rp)) {
    rp <- .dt_prep_rp(rp)
  }
  
  # tramsformation:
  if(!missing(tf)) {
    tf <- .funply(tf)
    rp <- .dt_transform(x, row, col, tf)
  }
  
  # modify:
  if(.C_is_missing_idx(row)) {
    return(.dt_mod_whole(x, col, rp, abortcall = sys.call()))
  }
  else{
    return(.dt_mod_partialcoe(x, row, col, rp, sys.call()))
  }
  
}



#' @keywords internal
#' @noRd
.sb_mod_array <- function(x, s, use, chkdup, rp, tf, abortcall) {
  
  
  .check_args_array(x, s, use, sys.call())
  
  if(is.list(x) && !missing(tf)) {
    tf <- .funply(tf)
  }
  
  # all empty indices:
  if(.all_missing_indices(s)) {
    return(.all_mod(x, rp, tf,sys.call()))
  }
  
  # zero-length subscripts:
  if(length(use) == 0L || .C_is_missing_idx(use)) {
    return(.all_mod(x, rp, tf, sys.call()))
  }
  
  # s, d arguments:
  lst <- ci_ss(x, s, use, chkdup, .abortcall = sys.call())
  
  if(!missing(rp)) {
    return(.arr_repl(x, lst, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    return(.arr_tf(x, lst, tf, abortcall = sys.call()))
  }
}

