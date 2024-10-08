


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
  
  if(length(x) <= (2^31 -1)) {
    .rcpp_set_vind_32(x, as.integer(ind - 1L), rp, abortcall)
    return(invisible(NULL))
  }
  else {
    .rcpp_set_vind_64(x, as.double(ind - 1), rp, abortcall)
    return(invisible(NULL))
  }
}


#' @keywords internal
#' @noRd
.rcpp_set_vind_32 <- function(x, ind, rp, abortcall) {
  
  if(is.logical(x)) {
    .rcpp_set_vind_32_Logical(x, ind, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_vind_32_Integer(x, ind, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_vind_32_Numeric(x, ind, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_vind_32_Character(x, ind, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_vind_32_Complex(x, ind, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_vind_32_Raw(x, ind, as.raw(rp))
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
.rcpp_set_vind_64 <- function(x, ind, rp, abortcall) {
  
  if(is.logical(x)) {
    .rcpp_set_vind_64_Logical(x, ind, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_vind_64_Integer(x, ind, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_vind_64_Numeric(x, ind, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_vind_64_Character(x, ind, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_vind_64_Complex(x, ind, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_vind_64_Raw(x, ind, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
  
}
