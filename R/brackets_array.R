

#' @keywords internal
#' @noRd
.arr_length <- function(x, lst, d) {
  x.dim <- dim(x)
  spec.dimsize <- collapse::vlengths(lst[d])
  unspec.dimsize <- x.dim[-d]
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
  
  temp.fun <- function(...) {
    rp <-  .lapply(x[..., drop = FALSE], tf)
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
  
  temp.fun <- function(...) {
    .check_rp_atomic(rp, prod(collapse::vlengths(lst)), abortcall) # used to be.arr_length(x, lst, d)
    x[...] <- rp
    return(x)
  }
  x <- do.call(temp.fun, lst)
  return(x)
}

#' @keywords internal
#' @noRd
.arr_repl_list <- function(x, lst, rp, abortcall) {
  
  temp.fun <- function(...) {
    .check_rp_list(rp, prod(collapse::vlengths(lst)), abortcall) # used to be.arr_length(x, lst, d)
    x[...] <- rp
    return(x)
  }
  x <- do.call(temp.fun, lst)
  return(x)
}


#' @keywords internal
#' @noRd
.arr_set <- function(x, s, d, chkdup, inv, rp, tf, abortcall) {
  
  # Prep:
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .ci_sub_check(x, s, d, ndims, .abortcall = abortcall)
  
  lst <- ci_sub(
    x, s, d, inv, chkdup, .abortcall = abortcall
  ) # Note: ci_sub will already ensure the subs are integers.
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
    rp <- tf(do.call(\(...)x[...], lst))
  }
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  
  # CASE 1: `x` has between 3 and 8 dimensions (emtpy args, 1d, and 2d already captured earlier)
  if(ndims <= 8L) {
    .rcpp_set_array_2d_8d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 2: `x` has between 9 and 16 dimensions
  if(ndims <= 16L) {
    .rcpp_set_array_16d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 3:  `x` has more 16 dimensions
  # use generalized array code (inspired by R's own internal code)
  .rcpp_set_array_general_atomic(x, lst, x.dim, rp)
  return(invisible(NULL))
  
}

#' @keywords internal
#' @noRd
.rcpp_set_array_2d_8d <- function(x, rp, lst, x.dim, abortcall) {
  dimcumprod <- as.double(cumprod(x.dim))
  
  .rcpp_set_array_2d_8d_atomic(x, lst, dimcumprod, rp)
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.rcpp_set_array_16d <- function(x, rp, lst, x.dim, abortcall) {
  n <- length(x.dim)
  
  if(n < 16L) {
    lst[(n+1):16L] <- list(1L)
  }
  dimcumprod <- cumprod(c(x.dim, rep(0L, 16L - n))) |> as.double()
  args <- c(list(x), lst, list(dimcumprod, rp))
  do.call(.rcpp_set_array_16d_atomic, args)
  return(invisible(NULL))
  
}

