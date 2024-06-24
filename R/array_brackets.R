

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
.arr_check <- function(x, sub, dims, ndims, abortcall) {
  if(!is.list(sub) || !is.numeric(dims)) {
    stop(simpleError("`sub` must be a list, and `dims` must be a integer vector", call = abortcall))
  }
  if(length(sub) != length(dims)) {
    stop(simpleError("`length(sub) != length(dims)`", call = abortcall))
  }
  if(.C_any_badindx(as.integer(dims), ndims)) {
    stop(simpleError("`dims` out of range", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.arr_lst_brackets <- function(x, sub, dims, chkdup, inv, abortcall) {
  
  # Note: since arrays have many dimensions,
  # but the maximum total number of elements remains the same
  # the maximum of each dimension reduces.
  # Thus, creating sequences here is not so expensive.
  .arr_check(x, sub, dims, length(dim(x)), abortcall)
  lst <- .rcpp_seq_mlen(as.integer(dim(x)))
  if(length(dims) > 0L) {
    for(i in seq_along(dims)) {
      lst[[dims[i]]] <- .indx_make_dim(
        sub[[i]], x, dim.L = dims[i], chkdup = chkdup, inv = inv, abortcall
      )
    }
  }
  return(lst)
}


#' @keywords internal
#' @noRd
.arr_lst_brackets.sb_x <- function(x, sub, dims, abortcall) {
  
  
  .arr_check(x, sub, dims, length(dim(x)), abortcall)
  lst <- .rcpp_seq_mlen(as.integer(dim(x)))
  if(length(dims) > 0L) {
    for(i in seq_along(dims)) {
      lst[[dims[i]]] <- .indx_make_dim.sb_x(
        sub[[i]], x, dim.L = dims[i], abortcall
      )
    }
  }
  return(lst)
}


#' @keywords internal
#' @noRd
.arr_x <- function(x, lst, abortcall) {
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_tf <- function(x, lst, tf, abortcall) {
  
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
.arr_tf_list <- function(x, lst, tf, .lapply, abortcall) {
  
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
.arr_repl <- function(x, lst, rp, abortcall) {
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
.arr_repl_list <- function(x, lst, rp, abortcall) {
  
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
.arr_set <- function(x, sub, dims, chkdup, inv, rp, tf, abortcall) {
  
  # Prep:
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .arr_check(x, sub, dims, ndims, abortcall = abortcall)
  
  
  # CASE 1: all subscripts are missing arguments, thus everything needs to change
  # (regardless if inv = TRUE or not)
  if(length(sub) == 0 && length(dims) == 0) {
    .rcpp_set_all(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  # CASE 2: `x` is a vector / 1d array, so subscript translation is waste of computation
  if(ndims == 1L) {
    elements <- .indx_make_element(
      sub[[1]], x, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = abortcall
    )
    .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  lst <- .arr_lst_brackets(
    x, sub, dims, chkdup = chkdup, inv = inv, abortcall = abortcall
  )
  
  # CASE 3: all list elements are integer(0), so nothing to change
  if(.any_empty_indices(lst)) {
    return(invisible(NULL))
  }
  
  # CASE 4: `x` has between 2 and 6 dimensions, and neither all nor empty selection
  if(ndims <= 6L) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
      rp <- tf(do.call(\(...)x[...], lst))
    }
    .rcpp_set_array_2d_6d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 5: `x` has more than 6 dimension
  # so default to translating subscripts to flat indices,
  # and treat as vector with the flattened indices.
  elements <- .sub2ind_general(lst, x.dim)
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
