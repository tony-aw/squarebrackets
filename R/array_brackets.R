

#' @keywords internal
#' @noRd
.arr_length <- function(x, lst, dims) {
  x.dim <- dim(x)
  spec.dimsize <- collapse::vlengths(lst[dims])
  unspec.dimsize <- x.dim[-dims]
  return(prod(spec.dimsize, unspec.dimsize))
}


#' @keywords internal
#' @noRd
.arr_check <- function(x, idx, dims, ndims, abortcall) {
  if(!is.list(idx) || !is.numeric(dims)) {
    stop(simpleError("`idx` must be a list, and `dims` must be a integer vector", call = abortcall))
  }
  if(length(idx) != length(dims)) {
    stop(simpleError("`length(idx) != length(dims)`", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.arr_lst_brackets <- function(x, ndims, idx, dims, chkdup, inv, abortcall) {
  
  # Note: since arrays have many dimensions,
  # but the maximum total number of elements remains the same
  # the maximum of each dimension reduces.
  # Thus, creating sequences here is not a big issue.
  
  lst <- .rcpp_seq_mlen(as.integer(dim(x))) 
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim(
      idx[[i]], x, dim.L = dims[i], chkdup = chkdup, inv = inv, abortcall
    )
  }
  return(lst)
}


#' @keywords internal
#' @noRd
.arr_lst_brackets.sb_x <- function(x, ndims, idx, dims, abortcall) {
  lst <- .rcpp_seq_mlen(as.integer(dim(x)))
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim.sb_x(
      idx[[i]], x, dim.L = dims[i], abortcall
    )
  }
  return(lst)
}


#' @keywords internal
#' @noRd
.arr_x <- function(x, idx, dims, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets.sb_x(x, ndims, idx, dims, abortcall = abortcall)
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_rm <- function(x, idx, dims, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, ndims, idx, dims, chkdup = chkdup, inv = TRUE, abortcall = abortcall)
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_tf <- function(x, idx, dims, tf, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, ndims, idx, dims, chkdup = chkdup, inv = FALSE, abortcall = abortcall)
  
  temp.fun <- function(...) {
    rp <- tf(x[..., drop = FALSE])
    .check_rp_atomic(rp, prod(collapse::vlengths(lst)), abortcall)
    x[...] <- rp
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}

#' @keywords internal
#' @noRd
.arr_tf_list <- function(x, idx, dims, tf, chkdup, .lapply, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, ndims, idx, dims, chkdup = chkdup, inv = FALSE, abortcall = abortcall)
  
  temp.fun <- function(...) {
    rp <- .lapply(x[...], tf)
    .check_rp_list(rp, prod(collapse::vlengths(lst)), abortcall)
    x[...] <- rp
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}


#' @keywords internal
#' @noRd
.arr_repl <- function(x, idx, dims, rp, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, ndims, idx, dims, chkdup = chkdup, inv = FALSE, abortcall = abortcall)
  
  temp.fun <- function(...) {
    .check_rp_atomic(rp, prod(collapse::vlengths(lst)), abortcall) # used to be.arr_length(x, lst, dims)
    x[...] <- rp
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}

#' @keywords internal
#' @noRd
.arr_repl_list <- function(x, idx, dims, rp, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, ndims, idx, dims, chkdup = chkdup, inv = FALSE, abortcall = abortcall)
  
  temp.fun <- function(...) {
    .check_rp_list(rp, prod(collapse::vlengths(lst)), abortcall) # used to be.arr_length(x, lst, dims)
    x[...] <- rp
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}


#' @keywords internal
#' @noRd
.arr_lst_grid <- function(x, ndims, idx, dims, chkdup, inv, abortcall) {
  lst <- .rcpp_seq_mlen(as.integer(dim(x)))
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim(
      idx[[i]], x, dim.L = dims[i], chkdup = chkdup, inv = inv, abortcall
    )
  }
  return(lst)
}


