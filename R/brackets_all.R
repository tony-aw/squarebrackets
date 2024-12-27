
#' @keywords internal
#' @noRd
.all_NULL_indices <- function(lst) {
  check <- vapply(lst, \(x) is.null(x), FUN.VALUE = logical(1L))
  if(all(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @keywords internal
#' @noRd
.all_missing_s_d <- function(s, d) {
  if(is.null(s) || length(s) == 0L) {
    return(TRUE)
  }
  if(is.null(d) || length(d) == 0L) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
#' @noRd
.all_mod_atomic <- function(x, rp, tf, abortcall) {
  if(!missing(tf) && !is.null(tf)) {
    rp <- tf(x)
  }
  .check_rp_atomic(rp, length(x), abortcall = sys.call())
  x[] <- rp
  return(x)
}


#' @keywords internal
#' @noRd
.all_mod_list <- function(x, rp, tf, .lapply, abortcall) {
  
  if(!missing(tf)) {
    rp <- .lapply(x, tf)
  }
  
  .check_rp_list(rp, length(x), abortcall = sys.call())
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

