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
#' @param coe Either `FALSE` (default), `TRUE`, or a function. \cr
#' The argument `coe` is ignored
#' if both the `row` and `filter` arguments are set to `NULL`. \cr
#' See Details section for more info. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' 
#' 
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset. \cr
#' Specifying `rp` will replace the subset. \cr
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' \cr
#' \bold{Argument \code{coe}} \cr
#' For data.frame-like objects,
#' `sb_mod()` can only auto-coerce whole columns, not subsets of columns. \cr
#' So it does not automatically coerce column types
#' when `row` or `filter` is also specified. \cr
#' The `coe` arguments provides 2 ways to circumvent this:
#' 
#'  1) The user can supply a coercion function to argument `coe`. \cr
#'  The function is applied on the entirety of every column specified in `col` or `vars`;
#'  columns outside this subset are not affected. \cr
#'  This coercion function is, of course,
#'  applied before replacement (`rp`) or transformation (`tf()`).
#'  2) The user can set `coe = TRUE`. \cr
#'  In this case,
#'  the whole columns specified in `col` or `vars` are extracted and copied to a list. \cr
#'  Subsets of each list element,
#'  corresponding to the selected rows,
#'  are modified with `rp` or `tf()`,
#'  using R's regular auto-coercion rules. \cr
#'  The modified list is then returned to the data.frame-like object,
#'  replacing the original columns.
#'
#' Note that coercion required additional memory. \cr
#' The larger the data.frame-like object, the larger the memory. \cr
#' The default, `coe = FALSE`, uses the least amount of memory. \cr \cr
#' 
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
  
  if(is.recursive(x)) {
    stop("Use the `sb2_` methods for recursive objects")
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
    return(.sb_mod_all(x, rp, tf, NULL, sys.call()))
  }
  
  return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
}


#' @rdname sb_mod
#' @export
sb_mod.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  if(.all_NULL_indices(list(row, col, i))) {
    return(.sb_mod_all(x, rp, tf, NULL, sys.call()))
  }
  
  if(!is.null(i)) {
    return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
  }
  
  if(!is.null(row)) {
    row <- ci_margin(x, row, 1L, inv, chkdup, .abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- ci_margin(x, col, 2L, inv, chkdup, .abortcall = sys.call())
  }
  
  if(.any_empty_indices(n(row, col))) {
    return(x)
  }
  
  if(is.null(row)) row <- 1:nrow(x)
  if(is.null(col)) col <- 1:ncol(x)
  if(!missing(tf)) {
    rp <- tf(x[row, col, drop = FALSE])
  }
  
  .check_rp_atomic(rp, (length(row) * length(col)), abortcall = sys.call())
  x[row, col] <- rp
  
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  if(.all_NULL_indices(list(sub, dims, i))) {
    return(.sb_mod_all(x, rp, tf, NULL, sys.call()))
  }
  
  if(!is.null(i)) {
    return(.flat_mod_atomic(x, i, inv, rp, tf, chkdup, sys.call()))
  }
  
  if(length(sub) == 0 && length(dims) == 0) {
    return(.sb_mod_all(x, rp, tf, NULL, sys.call()))
  }
  
  lst <- ci_sub(x, sub, dims, inv, chkdup, .abortcall = sys.call())
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
  if(!missing(rp)) {
    if(is.recursive(rp)) stop("`rp` must be non-recursive")
    return(.arr_repl(x, lst, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    return(.arr_tf(x, lst, tf, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb2_mod <- function(x, ...) {
  
  if(!is.recursive(x)) {
    stop("Use the `sb_` methods for non-recursive objects")
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
    return(.sb_mod_all(x, rp, tf, .lapply, sys.call()))
  }
  
  return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
}


#' @rdname sb_mod
#' @export
sb2_mod.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  if(.all_NULL_indices(list(row, col, i))) {
    return(.sb_mod_all(x, rp, tf, .lapply, sys.call()))
  }
  
  if(!is.null(i)) {
    return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  if(!is.null(row)) {
    row <- ci_margin(x, row, 1L, inv, chkdup, .abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- ci_margin(x, col, 2L, inv, chkdup, .abortcall = sys.call())
  }
  
  if(.any_empty_indices(n(row, col))) {
    return(x)
  }
  
  if(is.null(row)) row <- 1:nrow(x)
  if(is.null(col)) col <- 1:ncol(x)
  if(!missing(tf)) {
    rp <- lapply(x[row, col, drop = FALSE], tf)
  }
  
  .check_rp_list(rp, (length(row) * length(col)), abortcall = sys.call())
  x[row, col] <- rp
  
  return(x)
}


#' @rdname sb_mod
#' @export
sb2_mod.array <- function(
    x, sub = NULL, dims = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  if(.all_NULL_indices(list(sub, dims, i))) {
    return(.sb_mod_all(x, rp, tf, .lapply, sys.call()))
  }
  
  if(!is.null(i)) {
    return(.flat_mod_list(x, i, inv, rp, tf, chkdup, .lapply, sys.call()))
  }
  
  if(length(sub) == 0 && length(dims) == 0) {
    return(.sb_mod_all(x, rp, tf, .lapply, sys.call()))
  }
  
  lst <- ci_sub(x, sub, dims, inv, chkdup, .abortcall = sys.call())
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
  if(!missing(rp)) {
    if(!is.list(rp)) stop("`rp` must be a list")
    return(.arr_repl_list(x, lst, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    return(.arr_tf_list(x, lst, tf, .lapply, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb2_mod.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, inv = FALSE, coe = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  .internal_check_dots(list(...), sys.call())
  
  .internal_check_rptf(rp, tf, sys.call())
  
  # checks, errors, and transformations:
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
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
  if(is.null(col)) col <- collapse::seq_col(x)
  col <- as.integer(col)
  
  # coercion:
  rows_unspecified <- is.null(row) && is.null(filter)
  if(is.function(coe) && !rows_unspecified) {
    x <- collapse::ftransformv(x, vars = col, FUN = coe, apply = TRUE)
  } else {
    x <- collapse::ftransformv(x, vars = col, FUN = data.table::copy, apply = TRUE)
  }
  
  # non-empty return:
  if(is.null(row)) {
    return(.sb_mod_data.frame_whole(x, col, rp, tf, .lapply, abortcall = sys.call()))
  }
  if(!is.null(row) && !isTRUE(coe)) {
    return(.sb_mod_data.frame_partial(x, row, col, rp, tf, .lapply, abortcall = sys.call()))
  }
  if(!is.null(row) && isTRUE(coe)) {
    return(.sb_mod_data.frame_partialcoe(x, row, col, rp, tf, .lapply, abortcall = sys.call()))
  }
  
}


#' @keywords internal
#' @noRd
.sb_mod_data.frame_whole <- function(x, col, rp, tf, .lapply, abortcall) {
  
  if(!missing(tf)) {
    rp <- .lapply(collapse::ss(x, j = col, check = FALSE), tf)
  }
  
  .check_rp_df(rp, abortcall = abortcall)
  data.table::set(x, j = col, value = rp)
  
  return(x)
}


#' @keywords internal
#' @noRd
.sb_mod_data.frame_partial <- function(x, row, col, rp, tf, .lapply, abortcall) {
  
  row <- as.integer(row)
  
  if(!missing(tf)) {
    rp <- .lapply(collapse::ss(x, i = row, j = col, check = FALSE), tf)
  }
  
  .check_rp_df(rp, abortcall = abortcall)
  data.table::set(x, i = row, j = col, value = rp)
  
  return(x)
}


#' @keywords internal
#' @noRd
.sb_mod_data.frame_partialcoe <- function(x, row, col, rp, tf, .lapply, abortcall) {
  
  row <- as.integer(row)
  
  extraction <- collapse::qDF(collapse::ss(x, j = col, check = FALSE))
  if(ncol(extraction) == ncol(x) && ncol(x) > 1) {
    warning(simpleWarning("coercing all columns", call = abortcall))
  }
  
  if(!missing(tf)) {
    rp <- .lapply(collapse::ss(extraction, i = row, check = FALSE), tf)
  }
  .check_rp_df(rp, abortcall = abortcall)
  
  extraction[row, ] <- rp
  
  data.table::set(x, j = col, value = extraction)
  
  return(x)
}


#' @keywords internal
#' @noRd
.sb_mod_all <- function(x, rp, tf, .lapply, abortcall) {
  
  if(is.list(x)) {
    if(!missing(tf) && !is.null(tf)) {
      rp <- .lapply(x, tf)
    }
    
    .check_rp_list(rp, length(x), abortcall = sys.call())
    x[] <- rp
    return(x)
  }
  
  if(!missing(tf) && !is.null(tf)) {
    rp <- tf(x)
  }
  .check_rp_atomic(rp, length(x), abortcall = sys.call())
  x[] <- rp
  return(x)
}
