

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
  x <- do.call(temp.fun, lst)
  return(x)
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
  x <- do.call(temp.fun, lst)
  return(x)
}


#' @keywords internal
#' @noRd
.arr_set <- function(x, sub, dims, chkdup, inv, rp, tf, abortcall) {
  
  # Prep:
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .ci_array_check(x, sub, dims, ndims, .abortcall = abortcall)
  
  lst <- ci_sub(
    x, sub, dims, inv, chkdup, .abortcall = abortcall
  ) # Note: ci_sub will already ensure the subs are integers.
  
  
  # CASE 1: `x` has between 3 and 8 dimensions (0d, 1, and 2d already captured earlier)
  if(ndims <= 8L) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
      rp <- tf(do.call(\(...)x[...], lst))
    }
    .rcpp_set_array_2d_8d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 2: `x` has between 9 and 16 dimensions
  if(ndims <= 16L) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
      rp <- tf(do.call(\(...)x[...], lst))
    }
    .rcpp_set_array_16d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 2: `x` has more than 16 dimension
  # so default to translating subscripts to flat indices,
  # and treat as vector with the flattened indices.
  elements <- sub2ind(lst, x.dim, checks = FALSE)
  .sb_set_atomic(x, elements, rp = rp, tf = tf, abortcall = sys.call())
  return(invisible(NULL))
  
}

#' @keywords internal
#' @noRd
.rcpp_set_array_2d_8d <- function(x, rp, lst, x.dim, abortcall) {
  dimcumprod <- as.double(cumprod(x.dim))
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_set_array_2d_8d_atomic(x, lst, dimcumprod, rp)
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.rcpp_set_array_16d <- function(x, rp, lst, x.dim, abortcall) {
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  n <- length(x.dim)
  
  if(n < 16L) {
    lst[(n+1):16L] <- list(1L)
  }
  dimcumprod <- cumprod(c(x.dim, rep(0L, 16L - n))) |> as.double()
  args <- c(list(x), lst, list(dimcumprod, rp))
  do.call(.rcpp_set_array_16d_atomic, args)
  return(invisible(NULL))
  
}
