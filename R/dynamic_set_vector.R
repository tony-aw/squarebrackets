


#' @keywords internal
#' @noRd
.rcpp_set_all <- function(x, rp, tf, abortcall) {
  
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop(simpleError("`tf` must be a function", call = abortcall))
    }
    rp <- tf(x)
  }
  
  if(is.logical(x)) {
    .rcpp_set_all_Logical(x, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_all_Integer(x, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_all_Numeric(x, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_all_Character(x, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_all_Complex(x, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_all_Raw(x, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
  
}


#' @keywords internal
#' @noRd
.rcpp_set_vind <- function(x, ind, rp, abortcall) {
  
  if(typeof(x) != typeof(rp)) {
    message(sprintf("coercing `rp` to %s", typeof(x)))
    if(is.logical(x)) rp <- as.logical(rp)
    else if(is.integer(x)) rp <- as.integer(rp)
    else if(is.double(x)) rp <- as.double(rp)
    else if(is.complex(x)) rp <- as.complex(rp)
    else if(is.character(x)) rp <- as.character(rp)
    else if(is.raw(x)) rp <- as.raw(rp)
    else {
      stop(simpleError(
        "unsupported atomic type", call = abortcall
      ))
    }
  }
  
  if(length(x) <= (2^31 -1)) {
    .rcpp_set_vind_32_atomic(x, as.integer(ind - 1L), rp)
    return(invisible(NULL))
  }
  else {
    .rcpp_set_vind_64_atomic(x, as.double(ind - 1), rp)
    return(invisible(NULL))
  }
}
