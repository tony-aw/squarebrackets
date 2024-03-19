

#' @keywords internal
#' @noRd
.rcpp_setrv_safe <- function(x, v, rp, invert, abortcall) {
  if(is.logical(x)) {
    .rcpp_setrv_safe_Logical(x, as.logical(v), as.logical(rp), invert)
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_setrv_safe_Integer(x, as.integer(v), as.integer(rp), invert)
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_setrv_safe_Numeric(x, as.double(v), as.double(rp), invert)
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_setrv_safe_Character(x, as.character(v), as.character(rp), invert)
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_setrv_safe_Complex(x, as.complex(v), as.complex(rp), invert)
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_setrv_safe_Raw(x, as.raw(v), as.raw(rp), invert)
    return(invisible(NULL))
  }
  else {
    stop(simpleError("unsupported atomic type", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.rcpp_setrv_fast <- function(x, v, rp, invert, abortcall) {
  if(is.logical(x)) {
    .rcpp_setrv_fast_Logical(x, as.logical(v), as.logical(rp), invert)
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_setrv_fast_Integer(x, as.integer(v), as.integer(rp), invert)
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_setrv_fast_Numeric(x, as.double(v), as.double(rp), invert)
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_setrv_fast_Character(x, as.character(v), as.character(rp), invert)
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_setrv_fast_Complex(x, as.complex(v), as.complex(rp), invert)
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_setrv_fast_Raw(x, as.raw(v), as.raw(rp), invert)
    return(invisible(NULL))
  }
  else {
    stop(simpleError("unsupported atomic type", call = abortcall))
  }
}
