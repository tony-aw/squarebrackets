

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
  
  mycall <- as.call(c(
    list(quote(`[`)),
    list(quote(x)),
    lst,
    drop = FALSE
  ))
  x <- eval(mycall)
  return(x)
}


#' @keywords internal
#' @noRd
.arr_tf <- function(x, lst, tf, abortcall) {
  
  temp.fun <- function(...) {
    rp <- tf(x[..., drop = FALSE])
    .check_rp(x, rp, prod(collapse::vlengths(lst)), abortcall)
    x[...] <- rp
    return(x)
  }
  
  mycall <- as.call(c(
    list(quote(temp.fun)),
    lst
  ))
  x <- eval(mycall)
  return(x)
}


#' @keywords internal
#' @noRd
.arr_repl <- function(x, lst, rp, abortcall) {
  
  .check_rp(x, rp, prod(collapse::vlengths(lst)), abortcall)
  
  mycall <- as.call(c(
    list(quote(`[<-`)),
    list(quote(x)),
    lst,
    value = list(rp)
  ))
  x <- eval(mycall)
  return(x)
}

#' @keywords internal
#' @noRd
.arr_set_atomic <- function(x, s, use, chkdup, rp, tf, abortcall) {
  
  # Prep:
  x.dim <- dim(x)
  ndim <- length(x.dim)
  
  lst <- ci_ss(
    x, s, use, chkdup, .abortcall = abortcall
  ) # Note: ci_ss will already ensure the subs are integers.
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop(simpleError("`tf` must be a function", call = abortcall))
    rp <- tf(.arr_x(x, lst, abortcall))
  }
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  
  # CASE 1: `x` has between 2 and 16 dimensions (empty args and 1d arrays already captured earlier)
  if(ndim <= 8L) {
    .rcpp_set_array_d(x, rp, lst, x.dim, abortcall = abortcall)
    return(invisible(NULL))
  }
  
  # CASE 2:  `x` has more 16 dimensions
  # use generalized array code (inspired by R's own internal code)
  .rcpp_set_array_general_atomic(x, lst, x.dim, rp)
  return(invisible(NULL))
  
}

#' @keywords internal
#' @noRd
.rcpp_set_array_d <- function(x, rp, lst, x.dim, abortcall) {
  dimcumprod <- as.double(cumprod(x.dim))
  
  .rcpp_set_array_d_atomic(x, lst, dimcumprod, rp)
  return(invisible(NULL))
  
}


