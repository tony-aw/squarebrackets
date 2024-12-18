#' Construct Indices
#'
#' @description
#' These functions construct flat or dimensional indices. \cr
#'
#'  * `ci_flat()` constructs an integer vector flat indices.
#'  * `ci_margin()` constructs an integer vector of indices for one particular dimension margin.
#'  * `ci_sub()` constructs a list of integer subscripts.
#'  * `ci_df()` is the same as `ci_margin()`,
#'  except it is specifically designed for data.frame-like objects. \cr
#'  It is a separate function,
#'  because things like `dimnames(x)[1]` and `rownames(x)`
#'  do not always return the same output for certain data.frame-like objects. \cr
#'
#'
#' @param x the object for which the indices are meant.
#' @param i,slice,margin,s,d,inv See \link{squarebrackets_indx_args}. \cr
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
ci_flat <- function(
    x, i, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  is_list <- is.list(x)
  
  if(is.matrix(i)) {
    stop(simpleError("coordinate matrix not supported for `i`", call = .abortcall))
  }
  
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
    return(tci_cplx(i, n, inv, chkdup))
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



#' @rdname developer_ci
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
    return(tci_cplx(slice, dlength, inv, chkdup))
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


#' @rdname developer_ci
#' @export
ci_sub <- function(
    x, s, d, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  # Note: since arrays have many dimensions,
  # but the maximum total number of elements remains the same
  # the maximum of each dimension reduces.
  # Thus, creating sequences here is not so expensive.
  
  .ci_sub_check(x, s, d, ndims(x), .abortcall)
  
  if(length(s) == 1L) {
    return(.ci_sub1(x, s, d, inv, chkdup, uniquely_named, .abortcall))
  }
  else {
    return(.ci_sub0(x, s, d, inv, chkdup, uniquely_named, .abortcall))
  }
}



#' @keywords internal
#' @noRd
.ci_sub1 <- function(
    x, s, d, inv, chkdup, uniquely_named, .abortcall
) {
  
  s <- s[[1]]
  
  if(ndims(x) == 1L) {
    lst <- list(
      ci_flat(x, s, inv, chkdup, uniquely_named = FALSE, .abortcall)
    )
  }
  else {
    lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
    for(i in d) {
      lst[[i]] <- as.integer(ci_margin(
        x, s, i, inv, chkdup, uniquely_named = FALSE, .abortcall
      ))
    }
  }
  
  return(lst)
}


#' @keywords internal
#' @noRd
.ci_sub0 <- function(
    x, s, d, inv, chkdup, uniquely_named, .abortcall
) {
  
  s <- .ci_sub_make_sub(s, d)
  
  lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
  for(i in seq_along(d)) {
    lst[[d[i]]] <- as.integer(ci_margin(
      x, s[[i]], d[i], inv, chkdup, uniquely_named = FALSE, .abortcall
    ))
  }

  return(lst)
}




#' @keywords internal
#' @noRd
.ci_sub_make_sub <- function(s, d) {
  if(length(s) == 1L && length(d) != 1L) {
    s <- rep(s, length(d))
  }
  return(s)
}



#' @keywords internal
#' @noRd
.ci_sub_check <- function(x, s, d, ndims, .abortcall) {
  
  # check `d`:
  if(length(d) == 0L) {
    stop(simpleError("length(d) == 0L has not been captured", call = .abortcall))
  }
  if(!is.numeric(d)) {
    stop(simpleError("`d` must be a integer vector", call = .abortcall))
  }
  if(.any_badindx(as.integer(d), ndims)) {
    stop(simpleError("`d` out of range", call = .abortcall))
  }
  
  # check `s`:
  if(!is.list(s)) {
    stop(simpleError("`s` must be a list"))
  }
  badlen <- length(s) != 1L && length(s) != length(d)
  if(badlen) {
    stop(simpleError("`length(s)` must equal `length(d)`", call = .abortcall))
  }
  
}

#' @rdname developer_ci
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
    return(tci_cplx(slice, dlength, inv, chkdup))
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


