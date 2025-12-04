
#' @keywords internal
#' @noRd
.flat_s2i <- function(x, s, d, abortcall) {
  .ci_ss_check(x, s, d, ndim(x), abortcall)
  if(is.list(s)) {
    s <- s[[1L]]
  }
  return(s)
}

#' @keywords internal
#' @noRd
.flat_x <- function(x, i, inv, red, chkdup, abortcall) {
  elements <- ci_ii(x, i, inv, chkdup, FALSE, .abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && red) {
    return(x[[elements]])
  }
  return(x[elements])
}

#' @keywords internal
#' @noRd
.flat_a1d_x <- function(x, i, inv, red, chkdup, abortcall) {
  elements <- ci_ii(x, i, inv, chkdup, FALSE, .abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && red) {
    return(x[[elements]])
  }
  return(x[elements, drop = FALSE])
}

#' @keywords internal
#' @noRd
.flat_mod <- function(x, i, inv, rp, tf, chkdup, abortcall) {
  elements <- ci_ii(
    x, i, inv, chkdup, .abortcall = abortcall
  )
  n.i <- length(elements)
  if(n.i == 0) return(x)
  
  
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp(x, rp, n.i, abortcall = abortcall)
  x[elements] <- rp
  return(x)
}



#' @keywords internal
#' @noRd
.flat_set_atomic <- function(x, i, inv, rp, tf, chkdup, abortcall) {
  
  elements <- ci_ii(
    x, i, inv, chkdup, .abortcall = abortcall
  )
  
  .internal_check_rptf(rp, tf, abortcall)
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp(x, rp, n.i, abortcall)
  
  .rcpp_set_vind(x, elements, rp, abortcall)
  return(invisible(NULL))
  
}



#' @keywords internal
#' @noRd
.rcpp_set_vind <- function(x, ind, rp, abortcall) {
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  if(is.integer(ind)) {
    .rcpp_set_vind_32_atomic(x, ind, rp)
    return(invisible(NULL))
  }
  if(is.double(ind)) {
    .rcpp_set_vind_64_atomic(x, ind, rp)
    return(invisible(NULL))
  }
  else {
    return(invisible(NULL))
  }
}

