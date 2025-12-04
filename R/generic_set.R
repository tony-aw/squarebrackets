#' Methods to Modify Subsets of a Mutable Object By Reference
#'
#' @description
#' Methods to replace or transform a subset of a
#' \link[=squarebrackets_supported_structures]{supported mutable object}
#' using
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' 
#'
#' @param x a \bold{variable} belonging to one of the
#' \link[=squarebrackets_supported_structures]{supported mutable classes}. \cr
#' @param i,use,s,row,col See \link{squarebrackets_indx_args}. \cr
#' An empty index selection leaves the original object unchanged. \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' 
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset.
#' Specifying `rp` will replace the subset.
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' \cr
#' 
#' 
#' @returns
#' Returns: VOID. This method modifies the object by reference. \cr
#' Do not use assignments like `x <- ii_set(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#'
#' @example inst/examples/generic_set.R
#' 

#' @rdname sb_set
#' @export
ii_set <- function(x, i = NULL, use = 1, ..., rp, tf) {
  
  .methodcheck.ii(x, i, use, sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  UseMethod("ii_set", x)
}


#' @rdname sb_set
#' @export
ss_set <- function(x, s = NULL, use = rdim(x), ..., rp, tf) {
  
  .methodcheck.ss(x, s, use, sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  UseMethod("ss_set", x)
}


#' @rdname sb_set
#' @export
sbt_set <- function(x, row = NULL, col = NULL, use = 1:2, ..., rp, tf) {
  
  .methodcheck.sbt(x, row, col, use, sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  UseMethod("sbt_set", x)
}




#' @rdname sb_set
#' @export
ii_set.default <- function(
    x, i = NULL, use = 1, ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  if(length(x) == 0L) {
    return(invisible(NULL))
  }
  
  if(is.list(x) && !missing(tf)) {
    tf <- .funply(tf)
  }
  
  # MAIN:
  if(.C_is_missing_idx(i)) {
    .all_set_atomic(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  i <- ci_ii(
    x, i, use, chkdup, .abortcall = sys.call()
  )
  .flat_set_atomic(x, i, use, rp = rp, tf = tf, chkdup, abortcall = sys.call())
  return(invisible(NULL))
}



#' @rdname sb_set
#' @export
ss_set.default <- function(
    x, s = NULL, use = rdim(x), ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_dots(list(...), sys.call())
  return(.sb_set_array(x, s, use, chkdup, rp, tf, sys.call()))
}

#' @rdname sb_set
#' @export
sbt_set.default <- function(
    x, row = NULL, col = NULL, use = 1:2, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_dots(list(...), sys.call())
  use <- .internal_make_use_tabular(use, sys.call())
  return(.sb_set_array(x, n(row, col), use, chkdup, rp, tf, sys.call()))
}


#' @rdname sb_set
#' @export
sbt_set.data.table <- function(
    x, row = NULL, col = NULL, use = 1:2, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  if(!data.table::is.data.table(x)) {
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  if(length(x) == 0L) {
    return(invisible(NULL))
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
  # don't use if(is.null(row or col)) row or col <- 1:... -> will mess up the rest of this function
  
  
  # empty indices:
  if(.any_empty_indices(n(row, col))) {
    return(invisible(NULL))
  }
  
  # prep col:
  if(.C_is_missing_idx(col)) {
    col <- seq_len(ncol(x))
  }
  
  # prep replacement just in case:
  if(!missing(rp)) {
    rp <- .dt_prep_rp(rp)
  }
  
  # tramsformation:
  if(!missing(tf)) {
    tf <- .funply(tf)
    rp <- .dt_transform(x, row, col, tf)
  }
  
  
  # SET:
  if(.C_is_missing_idx(row)) {
    data.table::set(x, j = col, value = rp)
    return(invisible(NULL))
  }
  else {
    row <- as.integer(row)
    data.table::set(x, i = row, j = col, value = rp)
    return(invisible(NULL))
  }
  
  return(invisible(NULL))
  
}

#' @keywords internal
#' @noRd
.sb_set_array <- function(x, s, use, chkdup, rp, tf, abortcall) {
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, s, use, sys.call())
  
  if(is.list(x) && !missing(tf)) {
    tf <- .funply(tf)
  }
  
  # all missing arguments:
  if(.all_missing_indices(s)) {
    .all_set_atomic(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  # zero-length subscripts:
  if(length(use) == 0L) {
    .all_set_atomic(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  # 1d:
  if(ndim(x) == 1L) {
    i <- ci_ss(x, s, use)
    if(is.list(i)) i <- i[[1L]]
    .flat_set_atomic(x, i, sign(use), rp, tf, chkdup, sys.call())
    return(invisible(NULL))
  }
  
  .arr_set_atomic(x, s, use, chkdup, rp, tf, abortcall = sys.call())
  return(invisible(NULL))
}



