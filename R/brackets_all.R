
#' @keywords internal
#' @noRd
.all_missing_indices <- function(lst) {
  check <- vapply(lst, .C_is_missing_idx, FUN.VALUE = logical(1L))
  if(all(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @keywords internal
#' @noRd
.all_missing_s_d <- function(s, d) {
  if(.C_is_missing_idx(s) || length(s) == 0L) {
    return(TRUE)
  }
  if(.C_is_missing_idx(d) || length(d) == 0L) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
#' @noRd
.all_mod <- function(x, rp, tf, abortcall) {
  if(!missing(tf) && !is.null(tf)) {
    rp <- tf(x)
  }
  .check_rp(x, rp, length(x), abortcall = sys.call())
  x[] <- rp
  return(x)
}



#' @keywords internal
#' @noRd
.all_set_atomic <- function(x, rp, tf, abortcall) {
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



