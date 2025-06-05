#' Methods to Modify Subsets of a Mutable Object By Reference
#'
#' @description
#' Methods to replace or transform a subset of a
#' \link[=squarebrackets_supported_structures]{supported mutable object}
#' using
#' \link[=mutatomic_PassByReference]{pass-by-reference semantics}. \cr
#' 
#'
#' @param x a \bold{variable} belonging to one of the
#' \link[=squarebrackets_supported_structures]{supported mutable classes}. \cr
#' @param i,s,d,obs,vars,inv See \link{squarebrackets_indx_args}. \cr
#' An empty index selection leaves the original object unchanged. \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp,tf,.lapply see \link{squarebrackets_modify}.
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
#' Do not use assignments like `x <- sb_set(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#'
#' @example inst/examples/generic_set.R
#' 

#' @rdname sb_set
#' @export
i_set <- function(x, ...) {
  
  .methodcheck.i(x, sys.call())
  
  UseMethod("i_set", x)
}

#' @rdname sb_set
#' @export
i2_set <- function(x, ...) {
  
  .methodcheck.i2(x, sys.call())
  
  UseMethod("i2_set", x)
}

#' @rdname sb_set
#' @export
ss_set <- function(x, ...) {
  
  .methodcheck.ss(x, sys.call())
  UseMethod("ss_set", x)
}


#' @rdname sb_set
#' @export
ss2_set <- function(x, ...) {
  
  .methodcheck.ss2(x, sys.call())
  
  
  UseMethod("ss2_set", x)
}

#' @rdname sb_set
#' @export
i_set.default <- function(
    x, i = NULL, inv = FALSE, ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  mutatomic::stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  
  # function:
  if(is.null(i)) {
    .all_set_atomic(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  .flat_set_atomic(x, i, inv, rp = rp, tf = tf, chkdup, abortcall = sys.call())
  return(invisible(NULL))
}



#' @rdname sb_set
#' @export
ss_set.default <- function(
    x, s = NULL, d = 1:ndim(x), inv = FALSE, ...,  rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  mutatomic::stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_dots(list(...), sys.call())
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_array(x, s, d, sys.call())

    
  # all missing arguments:
  if(.all_NULL_indices(list(s))) {
    .all_set_atomic(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  # zero-length subscripts:
  if(length(d) == 0) {
    .all_set_atomic(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  # 1d:
  if(ndim(x) == 1L) {
    i <- .flat_s2i(x, s, d, sys.call())
    .flat_set_atomic(x, i, inv, rp, tf, chkdup, sys.call())
    return(invisible(NULL))
  }
  
  # matrix:
  if(is.matrix(x)) {
    .mat_set(x, s, d, inv, chkdup, rp, tf, sys.call())
    return(invisible(NULL))
  }
  
  
  # s, d arguments:
  .arr_set(x, s, d, chkdup, inv, rp, tf, abortcall = sys.call())
  return(invisible(NULL))
}


#' @rdname sb_set
#' @export
i2_set.default <- function(x, ...) {
  stop("`x` is not a (supported) mutable object")
}

#' @rdname sb_set
#' @export
ss2_set.data.table <- function(
    x, s = NULL, d = 1:2, obs = NULL, vars = NULL, inv = FALSE,
    ..., rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # checks:
  .internal_check_dots(list(...), sys.call())
  if(!data.table::is.data.table(x)) {
    stop("`x` is not a (supported) mutable object")
  }
  .internal_check_rptf(rp, tf, sys.call())
  .check_args_df(x, s, d, obs, vars, abortcall = sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  # make arguments:
  rowcol <- .dt_make_args(x, s, d, obs, vars, inv, chkdup, sys.call())
  row <- rowcol[[1L]]
  col <- rowcol[[2L]]
  # don't use if(is.null(row or col)) row or col <- 1:... -> will mess up the rest of this function
  
  
  # empty indices:
  if(.any_empty_indices(n(row, col))) {
    return(invisible(NULL))
  }
  
  # prep col:
  if(is.null(col)) {
    col <- as.integer(1:ncol(x))
  }
  
  # prep replacement just in case:
  if(!missing(rp)) {
    rp <- .dt_prep_rp(rp)
  }
  
  # tramsformation:
  if(!missing(tf)) {
    rp <- .dt_transform(x, row, col, tf, .lapply)
  }
  
  
  # SET:
  if(is.null(row)) {
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
.sb_set_atomic <- function(x, elements, rp, tf, abortcall) {
  
  .internal_check_rptf(rp, tf, abortcall)
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall)
  
  .rcpp_set_vind(x, elements, rp, abortcall)
  return(invisible(NULL))
  
}



