


#' @keywords internal
#' @noRd
.rcpp_slcseq_x <- function(x, start, end, by, len, abortcall) {
  
  if(is.logical(x)) {
    return(.rcpp_slcseq_x_Logical(x, start, end, by, len))
  }
  else if(is.integer(x)) {
    return(.rcpp_slcseq_x_Integer(x, start, end, by, len))
  }
  else if(is.double(x)) {
    return(.rcpp_slcseq_x_Numeric(x, start, end, by, len))
  }
  else if(is.character(x)) {
    return(.rcpp_slcseq_x_Character(x, start, end, by, len))
  }
  else if(is.complex(x)) {
    return(.rcpp_slcseq_x_Complex(x, start, end, by, len))
  }
  else if(is.raw(x)) {
    return(.rcpp_slcseq_x_Raw(x, start, end, by, len))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
}

#' @keywords internal
#' @noRd
.rcpp_slcseq_xrev <- function(x, start, end, by, len, abortcall) {
  
  if(is.logical(x)) {
    return(.rcpp_slcseq_xrev_Logical(x, start, end, by, len))
  }
  else if(is.integer(x)) {
    return(.rcpp_slcseq_xrev_Integer(x, start, end, by, len))
  }
  else if(is.double(x)) {
    return(.rcpp_slcseq_xrev_Numeric(x, start, end, by, len))
  }
  else if(is.character(x)) {
    return(.rcpp_slcseq_xrev_Character(x, start, end, by, len))
  }
  else if(is.complex(x)) {
    return(.rcpp_slcseq_xrev_Complex(x, start, end, by, len))
  }
  else if(is.raw(x)) {
    return(.rcpp_slcseq_xrev_Raw(x, start, end, by, len))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.rcpp_slcseq_rm <- function(x, start, end, by, len, abortcall) {
  
  if(is.logical(x)) {
    return(.rcpp_slcseq_rm_Logical(x, start, end, by, len))
  }
  else if(is.integer(x)) {
    return(.rcpp_slcseq_rm_Integer(x, start, end, by, len))
  }
  else if(is.double(x)) {
    return(.rcpp_slcseq_rm_Numeric(x, start, end, by, len))
  }
  else if(is.character(x)) {
    return(.rcpp_slcseq_rm_Character(x, start, end, by, len))
  }
  else if(is.complex(x)) {
    return(.rcpp_slcseq_rm_Complex(x, start, end, by, len))
  }
  else if(is.raw(x)) {
    return(.rcpp_slcseq_rm_Raw(x, start, end, by, len))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.rcpp_slcseq_set <- function(x, rp, start, end, by, len, abortcall) {
  
  if(is.logical(x)) {
    return(.rcpp_slcseq_set_Logical(x, as.logical(rp), start, end, by, len))
  }
  else if(is.integer(x)) {
    return(.rcpp_slcseq_set_Integer(x, as.integer(rp), start, end, by, len))
  }
  else if(is.double(x)) {
    return(.rcpp_slcseq_set_Numeric(x, as.double(rp), start, end, by, len))
  }
  else if(is.character(x)) {
    return(.rcpp_slcseq_set_Character(x, as.character(rp), start, end, by, len))
  }
  else if(is.complex(x)) {
    return(.rcpp_slcseq_set_Complex(x, as.complex(rp), start, end, by, len))
  }
  else if(is.raw(x)) {
    return(.rcpp_slcseq_set_Raw(x, as.raw(rp), start, end, by, len))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.rcpp_slcseq_setrev <- function(x, rp, start, end, by, len, abortcall) {
  
  if(is.logical(x)) {
    return(.rcpp_slcseq_setrev_Logical(x, as.logical(rp), start, end, by, len))
  }
  else if(is.integer(x)) {
    return(.rcpp_slcseq_setrev_Integer(x, as.integer(rp), start, end, by, len))
  }
  else if(is.double(x)) {
    return(.rcpp_slcseq_setrev_Numeric(x, as.double(rp), start, end, by, len))
  }
  else if(is.character(x)) {
    return(.rcpp_slcseq_setrev_Character(x, as.character(rp), start, end, by, len))
  }
  else if(is.complex(x)) {
    return(.rcpp_slcseq_setrev_Complex(x, as.complex(rp), start, end, by, len))
  }
  else if(is.raw(x)) {
    return(.rcpp_slcseq_setrev_Raw(x, as.raw(rp), start, end, by, len))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.rcpp_slcseq_setinv <- function(x, rp, start, end, by, len, abortcall) {
  
  if(is.logical(x)) {
    return(.rcpp_slcseq_setinv_Logical(x, as.logical(rp), start, end, by, len))
  }
  else if(is.integer(x)) {
    return(.rcpp_slcseq_setinv_Integer(x, as.integer(rp), start, end, by, len))
  }
  else if(is.double(x)) {
    return(.rcpp_slcseq_setinv_Numeric(x, as.double(rp), start, end, by, len))
  }
  else if(is.character(x)) {
    return(.rcpp_slcseq_setinv_Character(x, as.character(rp), start, end, by, len))
  }
  else if(is.complex(x)) {
    return(.rcpp_slcseq_setinv_Complex(x, as.complex(rp), start, end, by, len))
  }
  else if(is.raw(x)) {
    return(.rcpp_slcseq_setinv_Raw(x, as.raw(rp), start, end, by, len))
  }
  else {
    stop(simpleError(
      "unsupported atomic type", call = abortcall
    ))
  }
}
