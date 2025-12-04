#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' Methods to return a copy of an object with modified subsets. \cr
#' For modifying subsets using R's default copy-on-modification semantics, see \link{idx}. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,s,d,row,col,obs,vars,inv See \link{squarebrackets_indx_args}. \cr
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
ii_mod <- function(x, ...) {
  
  .methodcheck.ii(x, sys.call())
  
  UseMethod("ii_mod", x)
}


#' @rdname sb_mod
#' @export
ss_mod <- function(x, ...) {
  
  .methodcheck.ss(x, sys.call())
  UseMethod("ss_mod", x)
}


#' @rdname sb_mod
#' @export
sbt_mod <- function(x, ...) {
  .methodcheck.sbt(x, sys.call())
  UseMethod("sbt_mod", x)
}



#' @rdname sb_mod
#' @export
ii_mod.default <- function(
    x, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  if(is.list(x) && !missing(tf)) {
    tf <- .funply(tf)
  }
  
  if(is.null(i)) {
    return(.all_mod(x, rp, tf, sys.call()))
  }
  
  return(.flat_mod(x, i, inv, rp, tf, chkdup, sys.call()))
}


#' @rdname sb_mod
#' @export
ss_mod.default <- function(
    x, s = NULL, d = 1:ndim(x), inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  return(.sb_mod_array(x, s, d, inv, chkdup, rp, tf, sys.call()))
}



#' @rdname sb_mod
#' @export
sbt_mod.default <- function(
    x, row = NULL, col = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  return(.sb_mod_array(x, n(row, col), 1:2, inv, chkdup, rp, tf, sys.call()))
}


#' @rdname sb_mod
#' @export
sbt_mod.data.frame <- function(
    x, obs = NULL, vars = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  # make arguments:
  row <- col <- NULL
  if(!.C_is_missing_idx(obs)) {
    row <- ci_obs(
      x, obs, inv, chkdup, TRUE, sys.call()
    )
  }
  if(!.C_is_missing_idx(vars)) {
    col <- ci_vars(
      x, vars, inv, chkdup, TRUE, sys.call()
    )
  }
  
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
    tf <- .funply(tf)
    rp <- .dt_transform(x, row, col, tf)
  }
  
  if(.C_is_missing_idx(row)) row <- seq_len(nrow(x))
  if(.C_is_missing_idx(col)) col <- seq_len(ncol(x))
  
  # modify:
<<<<<<< Updated upstream
<<<<<<< Updated upstream
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
=======
  return(.dt_mod(x, row, col, rp, sys.call()))
>>>>>>> Stashed changes
=======
  return(.dt_mod(x, row, col, rp, sys.call()))
>>>>>>> Stashed changes
  
}



#' @keywords internal
#' @noRd
.sb_mod_array <- function(x, s, d, inv, chkdup, rp, tf, abortcall) {
  
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, s, d, sys.call())
  
  if(is.list(x) && !missing(tf)) {
    tf <- .funply(tf)
  }
  
  # all empty indices:
  if(.all_missing_indices(list(s))) {
    return(.all_mod(x, rp, tf,sys.call()))
  }
  
  # zero-length subscripts:
  if(length(d) == 0L || .C_is_missing_idx(d)) {
    return(.all_mod(x, rp, tf, sys.call()))
  }
  
  # 1d:
  if(ndim(x) == 1L) {
    i <- .flat_s2i(x, s, d)
    return(.flat_mod(x, i, inv, rp, tf, chkdup, sys.call()))
  }
  
  
  # s, d arguments:
  lst <- ci_ss(x, s, d, inv, chkdup, .abortcall = sys.call())
  
  if(!missing(rp)) {
    return(.arr_repl(x, lst, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    return(.arr_tf(x, lst, tf, abortcall = sys.call()))
  }
}

