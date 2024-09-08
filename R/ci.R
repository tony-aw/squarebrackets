#' Construct Indices
#'
#' @description
#' These functions construct flat or dimensional indices. \cr
#'
#'  * `ci_flat()` constructs an integer vector flat indices. \cr
#'  * `ci_margin()` constructs an integer vector of indices for one particular dimension margin. \cr
#'  * `ci_sub()` constructs a list of integer subscripts. \cr
#'  * `ci_df()` is the same as `ci_margin()`,
#'  except it is specifically designed for data.frame-like objects. \cr
#'  It is a separate function,
#'  because things like `dimnames(x)[1]` and `rownames(x)`
#'  do not always return the same output for certain data.frame-like objects. \cr
#'
#'
#' @param x the object for which the indices are meant.
#' @param i,slice,margin,sub,dims,inv See \link{squarebrackets_indx_args}. \cr
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
#' An integer vector of casted indices.
#' 
#' 
#' @example inst/examples/ci.R
#' 



#' @rdname ci
#' @export
ci_flat <- function(
    x, i, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  is_list <- is.list(x)
  
  if(is.function(i)) {
    if(is_list){
      i <- vapply(x, i, FUN.VALUE = logical(1L), USE.NAMES = FALSE) |> unlist()
    } else {i <- i(x)}
    
    if(!is.logical(i) || length(i) != length(x)) {
      stop(simpleError("if elements are given through a function, the function must return a logical vector", call = .abortcall))
    }
    if(!inv) return(which(i))
    if(inv) return(which(!i))
  }
  
  n.i <- length(i)
  
  if(n.i == 0L) {
    n <- length(x)
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(n))
  }
  
  if(is.complex(i)) {
    n <- length(x)
    return(tci_complex(i, n, inv, chkdup))
  }
  
  if(is.numeric(i)) {
    n <- length(x)
    return(tci_int(i, n, inv, chkdup))
  }
  
  if(is.character(i)) {
    nms <- names(x)
    return(tci_chr(i, nms, inv, chkdup, uniquely_named, .abortcall))
    
  }
  
  if(is.logical(i)) {
    n <- length(x)
    return(tci_bool(i, n, inv))
  }
  
  .indx_stop(.abortcall)
}



#' @rdname ci
#' @export
ci_margin <- function(
    x, slice, margin, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {

  if(is.null(dim(x))) {
    stop(simpleError("`x` has no dimensions", call = .abortcall))
  }
  
  n.slice <- length(slice)
  
  
  if(n.slice == 0L) {
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(dim(x)[margin]))
  }
  
  if(is.complex(slice)) {
    dlength <- dim(x)[margin]
    return(tci_complex(slice, dlength, inv, chkdup))
  }
  
  if(is.numeric(slice)) {
    dlength <- dim(x)[margin]
    return(tci_int(slice, dlength, inv, chkdup))
  }
  
  if(is.character(slice)) {
    dnames <- dimnames(x)[[margin]]
    return(tci_chr(slice, dnames, inv, chkdup, uniquely_named))
    
  }

  if(is.logical(slice)) {
    dlength <- dim(x)[margin]
    return(tci_bool(slice, dlength, inv))
    
  }

  .indx_stop(.abortcall)
}


#' @rdname ci
#' @export
ci_sub <- function(
    x, sub, dims, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  # Note: since arrays have many dimensions,
  # but the maximum total number of elements remains the same
  # the maximum of each dimension reduces.
  # Thus, creating sequences here is not so expensive.
  
  if(length(dims) == 1L && !is.list(sub)) {
    sub <- list(sub)
  }
  
  .arr_check(x, sub, dims, length(dim(x)), .abortcall)
  lst <- .rcpp_seq_mlen(as.integer(dim(x)))
  if(length(dims) > 0L) {
    for(i in seq_along(dims)) {
      lst[[dims[i]]] <- ci_margin(
        x, sub[[i]], dims[i], inv, chkdup, uniquely_named = FALSE, .abortcall
      )
    }
  }
  return(lst)
}


#' @rdname ci
#' @export
ci_df <- function(
    x, slice, margin, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  if(is.null(dim(x))) {
    stop(simpleError("`x` has no dimensions", call = .abortcall))
  }
  
  n.slice <- length(slice)
  
  
  if(n.slice == 0L) {
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(dim(x)[margin]))
  }
  
  if(is.complex(slice)) {
    if(margin == 1L) dlength <- nrow(x)
    if(margin == 2L) dlength <- ncol(x)
    return(tci_complex(slice, dlength, inv, chkdup))
  }
  
  if(is.numeric(slice)) {
    if(margin == 1L) dlength <- nrow(x)
    if(margin == 2L) dlength <- ncol(x)
    return(tci_int(slice, dlength, inv, chkdup))
  }
  
  if(is.character(slice)) {
    if(margin == 1L) dnames <- rownames(x)
    if(margin == 2L) dnames <- names(x)
    return(tci_chr(slice, dnames, inv, chkdup, uniquely_named))
    
  }
  
  if(is.logical(slice)) {
    if(margin == 1L) dlength <- nrow(x)
    if(margin == 2L) dlength <- ncol(x)
    return(tci_bool(slice, dlength, inv))
    
  }
  
  .indx_stop(.abortcall)
}




#' @keywords internal
#' @noRd
.arr_check <- function(x, sub, dims, ndims, .abortcall = sys.call()) {
  if(!is.list(sub) || !is.numeric(dims)) {
    stop(simpleError("`sub` must be a list, and `dims` must be a integer vector", call = .abortcall))
  }
  if(length(sub) != length(dims)) {
    stop(simpleError("`length(sub) != length(dims)`", call = .abortcall))
  }
  if(.any_badindx(as.integer(dims), ndims)) {
    stop(simpleError("`dims` out of range", call = .abortcall))
  }
}
