#' Construct Indices
#'
#' @description
#' These functions construct indices. \cr
#'
#'  * `ci_ii()` constructs an integer vector flat/interior indices.
#'  * `ci_margin()` constructs an integer vector of indices for one particular dimension margin.
#'  * `ci_ss()` constructs a list of integer subscripts.
#'  * `ci_df()` is the same as `ci_margin()`,
#'  except it is specifically designed for data.frame-like objects. \cr
#'  It is a separate function,
#'  because things like `dimnames(x)[1]` and `rownames(x)`
#'  do not always return the same output for certain data.frame-like objects.
#'  * `ci_obs()` and `ci_vars()` construct row and column indices,
#'  respectively,
#'  for data.frame-like objects. \cr
#'
#'
#' @param x the object for which the indices are meant.
#' @param i,s,use,slice,margin See \link{squarebrackets_indx_args}. \cr
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
#' 
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
  
  if(!is.vector(i) && !is.mutatomic(i) && !is.function(i) && !is.formula(i)) {
    stop(simpleError("`i` must be a simple vector, a function, or a formula", call = .abortcall))
  }
  
  if(is.function(i)) {
    if(is_list){
      i <- vapply(x, i, FUN.VALUE = logical(1L), USE.NAMES = FALSE) |> unlist()
    } else {i <- i(x)}
    
    if(!is.logical(i) || length(i) != length(x)) {
      stop(simpleError("if elements are given through a function, the function must return a logical vector", call = .abortcall))
    }
    if(use > 0L) return(which(i))
    if(use < 0L) return(collapse::whichv(i, FALSE))
  }
  
  n.i <- length(i)
  
  if(n.i == 0L) {
    return(tci_zerolen(length(x), use))
  }
  
  if(is.formula(i)) {
    return(tci_formula(i, 0L, sys.call()))
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
    slice <- tci_formula(x, margin, slice, .abortcall)
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
    x, s = NULL, use = 1:ndim(x), chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
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

