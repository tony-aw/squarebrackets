

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
