#' Construct Indices
#'
#' @description
#' These functions construct indices. \cr
#'
#'  - `ci_ii()` constructs an integer vector flat/interior indices.
#'  - `ci_margin()` constructs an integer vector of indices for one particular dimension margin.
#'  - `ci_ss()` constructs a list of integer subscripts.
#'  - `ci_df()` constructs a list of row- and column indices for data.frames specifically.
#'
#'
#' @param x the object for which the indices are meant.
#' @param i,s,row,col,slice,margin,use See \link{squarebrackets_index_args}. \cr
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param uniquely_named Boolean,
#' indicating if the user knows a-priori that the relevant names of `x` are unique. \cr
#' If set to `TRUE`, speed may increase. \cr
#' But specifying `TRUE` when the relevant names are not unique will result in incorrect output.
#' @param .abortcall environment where the error message is passed to.
#' 
#' 
#'
#' @returns
#' An integer vector of constructed indices.
#' 
#' @concept ci
#' @example inst/examples/tci.R
#' 


#' @name developer_ci
NULL


#' @rdname developer_ci
#' @export
ci_ii <- function(
    x, i = NULL, use = 1L, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  is_list <- is.list(x)
  
  if(!is.numeric(use) || length(use) != 1 || is.na(use)) {
    stop(simpleError("`use` must be a numeric scalar", call = .abortcall))
  }
  if(abs(use) > 1) {
    message(simpleMessage("only the sign of `use` will be used", call = .abortcall))
  }
  if(!is.null(dim(i))) {
    stop(simpleError("`i` must be a simple vector", call = .abortcall))
  }
  
  n.i <- length(i)
  
  if(n.i == 0L) {
    return(tci_zerolen(length(x), use))
  }
  
  if(is.function(i) && is_list) {
    i <- vapply(x, i, FUN.VALUE = logical(1L), USE.NAMES = FALSE) |> unlist()
    
    if(!is.logical(i) || length(i) != length(x)) {
      stop(simpleError("if elements are given through a function, the function must return a logical vector", call = .abortcall))
    }
    if(use > 0L) return(which(i))
    if(use < 0L) return(collapse::whichv(i, FALSE))
  }
  
  if(is.formula(i)) {
    i <- tci_formula(i, 0L, length(x), names(x), .abortcall)
  }
  
  if(is.atomic(i)) {
    return(tci_atomic(i, length(x), names(x), use, chkdup, uniquely_named, .abortcall))
  }
  
  .indx_stop(.abortcall)
}



#' @rdname developer_ci
#' @export
ci_margin <- function(
    x, slice = NULL, margin, use = 1L, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {

  if(is.null(dim(x))) {
    stop(simpleError("`x` has no dimensions", call = .abortcall))
  }
  
  n.slice <- length(slice)
  
  
  if(n.slice == 0L) {
    return(tci_zerolen(dim(x)[margin], use))
  }
  
  if(is.formula(slice)) {
    slice <- tci_formula(slice, margin, dim(x)[margin], dimnames(x)[[margin]], .abortcall)
  }
  
  if(is.atomic(slice)) {
    dlen <- dim(x)[margin]
    dnames <- dimnames(x)[[margin]]
    return(tci_atomic(slice, dlen, dnames, use, chkdup, uniquely_named, .abortcall))
  }

  .indx_stop(.abortcall)
}



#' @rdname developer_ci
#' @export
ci_ss <- function(
    x, s = NULL, use = Inf, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  # translate `use` from special cases:
  if(.C_is_missing_idx(use)) {
    stop(simpleError("`use` cannot be specified as `NULL` or `0L`", call = .abortcall))
  }
  else if(length(use) == 1L && is.double(use) && is.infinite(use)) {
    use <- 1:ndim(x) * sign(use)
  }
  
  d <- abs(use)
  use <- sign(use)
  
  if(.all_missing_indices(s)) {
    lst <- lapply(dim(x), \(n)seq_len(n)) # ALTREP sequences
    return(lst)
  }
  
  .ci_ss_check(x, s, d, ndim(x), .abortcall)
  
  # remove missing indices:
  if(is.list(s)) {
    s <- unclass(s)
    rem.ind <- which(vapply(s, .C_is_missing_idx, logical(1L)))
    if(length(rem.ind)) {
      s <- s[-rem.ind]
      d <- d[-rem.ind]
    }
  }
  
  
  if(length(d) == 1L || !is.list(s)) {
    return(.ci_ss.atomic(x, s, d, use, chkdup, uniquely_named, .abortcall))
  }
  else if(length(s) == 1L) {
    return(.ci_ss.atomic(x, s[[1L]], d, use, chkdup, uniquely_named, .abortcall))
  }
  else {
    return(.ci_ss0(x, s, d, use, chkdup, uniquely_named, .abortcall))
  }
}

#' @rdname developer_ci
#' @export
#' @importFrom stats as.formula
ci_df <- function(x, row, col, use = 1:2, chkdup = FALSE, .abortcall) {
  
  use <- .internal_make_use_tabular(use, sys.call())
  
  
  # rows:
  if(is.formula(row) && startsWith(format(row), "~~")) {
    row <- as.formula(row[[2L]])
    row <- .with_internal(x, row, .abortcall)
    if(!is.logical(row)) {
      stop(simpleError("improper formula given for `row`", call = .abortcall))
    }
    if(use[1] > 0) row <- which(row)
    if(use[1] < 0) row <- collapse::whichv(row, FALSE)
  }
  else if(!.C_is_missing_idx(row)) {
    row <- ci_margin(
      x, row, 1L, use[1], chkdup = FALSE, uniquely_named = TRUE, sys.call()
    )
  }
  
  
  # columns:
  if(is.function(col)) {
    col <- collapse::get_vars(x, col, return = "logical")
    if(use[2] > 0) col <- which(col)
    if(use[2] < 0) col <- collapse::whichv(col, FALSE)
  }
  else if(!.C_is_missing_idx(col)) {
    col <- ci_margin(
      x, col, 2L, use[2], chkdup = FALSE, uniquely_named = TRUE, sys.call()
    )
  }
  
  
  # out:
  out <- list(row, col)
  return(out)
}
