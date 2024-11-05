


#' @keywords internal
#' @noRd
.rcpp_set_all <- function(x, rp, tf, abortcall) {
  
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop(simpleError("`tf` must be a function", call = abortcall))
    }
    rp <- tf(x)
  }
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_set_all_atomic(x, rp)
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.rcpp_set_vind <- function(x, ind, rp, abortcall) {
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  if(length(x) <= (2^31 -1)) {
    .rcpp_set_vind_32_atomic(x, as.integer(ind - 1L), rp)
    return(invisible(NULL))
  }
  else {
    .rcpp_set_vind_64_atomic(x, as.double(ind - 1), rp)
    return(invisible(NULL))
  }
}
