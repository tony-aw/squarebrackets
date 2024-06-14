

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
.arr_lst_brackets <- function(x, idx, dims, chkdup, inv, abortcall) {
  
  # Note: since arrays have many dimensions,
  # but the maximum total number of elements remains the same
  # the maximum of each dimension reduces.
  # Thus, creating sequences here is not so expensive.
  
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
.arr_lst_brackets.sb_x <- function(x, idx, dims, abortcall) {
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
  
  lst <- .arr_lst_brackets.sb_x(x, idx, dims, abortcall = abortcall)
  
  
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_rm <- function(x, idx, dims, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, idx, dims, chkdup = chkdup, inv = TRUE, abortcall = abortcall)
  
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_tf <- function(x, idx, dims, inv, tf, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, idx, dims, chkdup = chkdup, inv = inv, abortcall = abortcall)
  
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
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
.arr_tf_list <- function(x, idx, dims, inv, tf, chkdup, .lapply, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, idx, dims, chkdup = chkdup, inv = inv, abortcall = abortcall)
  
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
  temp.fun <- function(...) {
    rp <- .lapply(x[..., drop = FALSE], tf)
    .check_rp_list(rp, prod(collapse::vlengths(lst)), abortcall)
    x[...] <- rp
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}


#' @keywords internal
#' @noRd
.arr_repl <- function(x, idx, dims, inv, rp, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, idx, dims, chkdup = chkdup, inv = inv, abortcall = abortcall)
  
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
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
.arr_repl_list <- function(x, idx, dims, inv, rp, chkdup, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- .arr_lst_brackets(x, idx, dims, chkdup = chkdup, inv = inv, abortcall = abortcall)
  
  if(.any_empty_indices(lst)) {
    return(x)
  }
  
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
.arr_set <- function(x, idx, dims, chkdup, inv, rp, tf, abortcall) {
  
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .arr_check(x, idx, dims, ndims, abortcall = abortcall)
  
  if(ndims == 1L) {
    elements <- .indx_make_element(
      idx[[1]], x, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = abortcall
    )
    .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  lst <- .arr_lst_brackets(
    x, idx, dims, chkdup = chkdup, inv = inv, abortcall = abortcall
  )
  if(.any_empty_indices(lst)) {
    return(invisible(NULL))
  }
  
  if(ndims <= 6L) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
      rp <- tf(do.call(\(...)x[...], lst))
    }
    .rcpp_set_array_2d_6d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  elements <- sub2ind(lst, x.dim, checks = FALSE)
  .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
  return(invisible(NULL))
  
}

#' @keywords internal
#' @noRd
.rcpp_set_array_2d_6d <- function(x, rp, lst, x.dim, abortcall) {
  dimcumprod <- cumprod(x.dim)
  if(is.logical(x)) {
    .rcpp_set_array_2d_6d_Logical(x, lst, dimcumprod, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_array_2d_6d_Integer(x, lst, dimcumprod, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_array_2d_6d_Numeric(x, lst, dimcumprod, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_array_2d_6d_Character(x, lst, dimcumprod, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_array_2d_6d_Complex(x, lst, dimcumprod, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_array_2d_6d_Raw(x, lst, dimcumprod, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError("unknown array type given", call = abortcall))
  }
}
