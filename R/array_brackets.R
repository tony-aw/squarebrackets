

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
  if(length(dims) == 1L && !is.list(sub)) {
    sub <- list(sub)
  }
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .arr_check(x, sub, dims, ndims, .abortcall = abortcall)
  
  
  # CASE 1: `x` is a vector / 1d array, so subscript translation is waste of computation
  if(ndims == 1L) {
    elements <- ci_flat(
      x, sub[[1]], inv, chkdup, .abortcall = sys.call()
    )
    .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  lst <- ci_sub(
    x, sub, dims, inv, chkdup, .abortcall = abortcall
  )
  
  # CASE 2: all list elements are integer(0), so nothing to change
  if(.any_empty_indices(lst)) {
    return(invisible(NULL))
  }
  
  # CASE 3: `x` has between 2 and 6 dimensions, and neither all nor empty selection
  if(ndims <= 6L) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
      rp <- tf(do.call(\(...)x[...], lst))
    }
    .rcpp_set_array_2d_6d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 4: `x` has more than 6 dimension
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
