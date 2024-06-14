#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' This is an S3 Method to return a copy of an object with modified subsets. \cr
#' Use `sb_mod(x, ...)` if `x` is a non-recursive object (i.e. atomic or factor). \cr
#' Use `sb2_mod(x, ...)` if `x` is a recursive object (i.e. list or data.frame-like). \cr
#' \cr
#' For modifying subsets using R's default copy-on-modification semantics, see \link{idx}. \cr \cr
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,lvl,row,col,idx,dims,filter,vars,inv See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... further arguments passed to or from other methods.
#' @param tf the transformation function.
#' @param rp an object of somewhat the same type as the selected subset of \code{x},
#' and the same same length as the selected subset of \code{x} or a length of 1. \cr
#' To remove recursive subsets of recursive objects, see either \link{sb2_rec} or \link{sb2_rm}.
#' @param coe Either `FALSE` (default), `TRUE`, or a function. \cr
#' The argument `coe` is ignored
#' if both the `row` and `filter` arguments are set to `NULL`. \cr
#' See Details section for more info. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param .lapply the generic methods use \link[base]{lapply}
#' for list- and data.frame-like objects
#' to compute `tf()` on every list element or dataset column. \cr
#' The user may supply a custom `lapply()`-like function
#' in this argument to use instead. \cr
#' For example, the perform parallel transformation,
#' the user may supply `future.apply::`\link[future.apply]{future_lapply}. \cr
#' The supplied function must use the exact same argument convention as
#' \link[base]{lapply},
#' otherwise errors or unexpected behaviour may occur.
#' 
#' 
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset. \cr
#' Specifying `rp` will replace the subset. \cr
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' Note that the `tf` argument is not available for factors: this is intentional. \cr
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
    x, i, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(
    i, x, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = sys.call()
  )
  
  n.i <- length(elements)
  if(n.i == 0) return(x)
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall = sys.call())
  x[elements] <- rp
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = sys.call()
    )
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- tf(x[elements])
    }
    .check_rp_atomic(rp, n.i, abortcall = sys.call())
    x[elements] <- rp
    return(x)
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = inv, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = inv, abortcall = sys.call())
  }
  
  if(.any_empty_indices(n(row, col))) {
    return(x)
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[row, col, drop = FALSE])
  }
  
  .check_rp_atomic(rp, (length(row) * length(col)), abortcall = sys.call())
  x[row, col] <- rp
  
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.array <- function(
    x, idx = NULL, dims = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = sys.call()
    )
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- tf(x[elements])
    }
    .check_rp_atomic(rp, n.i, abortcall = sys.call())
    x[elements] <- rp
    return(x)
  }
  
  if(!missing(rp)) {
    if(is.recursive(rp)) stop("`rp` must be non-recursive")
    return(.arr_repl(x, idx, dims, inv, rp, chkdup = chkdup, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    return(.arr_tf(x, idx, dims, inv, tf, chkdup = chkdup, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb_mod.factor <- function(
    x, i = NULL, lvl = NULL, inv = FALSE, ..., rp, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  
  .check_args_factor(i, lvl, drop = FALSE, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = sys.call()
    )
    n.i <- length(elements)
    if(n.i == 0) return(x)
    .check_rp_atomic(rp, n.i, abortcall = sys.call())
    x[elements] <- rp
    return(x)
  }
  if(!is.null(lvl)) {
    if(length(lvl) == 0) return(x)
    .prep_relevel(lvl, rp, x, sys.call())
    set.lvls <- levels(x)
    set.lvls[set.lvls == lvl] <- rp
    levels(x) <- set.lvls
    return(x)
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
    x, i, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(
    i, x, is_list = TRUE, chkdup = chkdup, inv = inv, abortcall = sys.call()
  )
  
  n.i <- length(elements)
  
  if(n.i == 0) {
    return(x)
  }
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- .lapply(x[elements], tf)
  }
  
  .check_rp_list(rp, n.i, abortcall = sys.call())
  x[elements] <- rp
  
  return(x)
}



#' @rdname sb_mod
#' @export
sb2_mod.array <- function(
    x, idx = NULL, dims = NULL, i = NULL, inv = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  if(!is.null(i)) {
    return(sb2_mod.default(x, i, inv = inv, ..., rp = rp, tf = tf, chkdup = chkdup))
  }
  
  if(!missing(rp)) {
    if(!is.list(rp)) stop("`rp` must be a list")
    return(.arr_repl_list(x, idx, dims, inv, rp, chkdup = chkdup, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    return(.arr_tf_list(x, idx, dims, inv, tf, chkdup = chkdup, .lapply, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb2_mod.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, inv = FALSE, coe = FALSE, ...,
    rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
) {
  
  # checks, errors, and transformations:
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, chkdup = chkdup, inv = inv, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = chkdup, inv = inv, abortcall = sys.call()
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
    x <- data.table::copy(x)
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
    if(!is.function(tf)) {
      stop(simpleError("`tf` must be a function", call = abortcall))
    }
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
    if(!is.function(tf)) {
      stop(simpleError("`tf` must be a function", call = abortcall))
    }
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
    if(!is.function(tf)) {
      stop(simpleError("`tf` must be a function", call = abortcall))
    }
    rp <- .lapply(collapse::ss(extraction, i = row, check = FALSE), tf)
  }
  .check_rp_df(rp, abortcall = abortcall)
  
  extraction[row, ] <- rp
  
  data.table::set(x, j = col, value = extraction)
  
  return(x)
}

