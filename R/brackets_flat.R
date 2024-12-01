
#' @keywords internal
#' @noRd
.flat_sub2i <- function(x, sub, dims, abortcall) {
  .ci_array_check(x, sub, dims, ndims(x), abortcall)
  if(is.list(sub)) {
    sub <- sub[[1L]]
  }
  return(sub)
}

#' @keywords internal
#' @noRd
.flat_x <- function(x, i, inv, red, chkdup, abortcall) {
  elements <- ci_flat(x, i, inv, chkdup, FALSE, .abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && red) {
    return(x[[elements]])
  }
  return(x[elements])
}

#' @keywords internal
#' @noRd
.flat_a1d_x <- function(x, i, inv, red, chkdup, abortcall) {
  elements <- ci_flat(x, i, inv, chkdup, FALSE, .abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && red) {
    return(x[[elements]])
  }
  return(x[elements, drop = FALSE])
}

#' @keywords internal
#' @noRd
.flat_mod_atomic <- function(x, i, inv, rp, tf, chkdup, abortcall) {
  elements <- ci_flat(
    x, i, inv, chkdup, .abortcall = abortcall
  )
  n.i <- length(elements)
  if(n.i == 0) return(x)
  
  
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall = abortcall)
  x[elements] <- rp
  return(x)
}


#' @keywords internal
#' @noRd
.flat_mod_list <- function(x, i, inv, rp, tf, chkdup, .lapply, abortcall) {
  elements <- ci_flat(
    x, i, inv, chkdup, .abortcall = abortcall
  )
  
  n.i <- length(elements)
  
  if(n.i == 0) {
    return(x)
  }
  
  if(!missing(tf)) {
    rp <- .lapply(x[elements], tf)
  }
  
  .check_rp_list(rp, n.i, abortcall = abortcall)
  x[elements] <- rp
  
  return(x)
}


#' @keywords internal
#' @noRd
.flat_set_atomic <- function(x, i, inv, rp, tf, chkdup, abortcall) {
  
  elements <- ci_flat(
    x, i, inv, chkdup, .abortcall = abortcall
  )
  
  .internal_check_rptf(rp, tf, abortcall)
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall)
  
  .rcpp_set_vind(x, elements, rp, abortcall)
  return(invisible(NULL))
  
}
