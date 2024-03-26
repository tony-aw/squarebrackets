

#' @keywords internal
#' @noRd
.rcpp_setapply_row <- function(x, f, abortcall) {
  
  if(is.logical(x)) {
    .rcpp_setapply_row_Logical(x, f)
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_setapply_row_Integer(x, f)
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_setapply_row_Numeric(x, f)
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_setapply_row_Character(x, f)
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_setapply_row_Complex(x, f)
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_setapply_row_Raw(x, f)
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}

#' @keywords internal
#' @noRd
.rcpp_setapply_col <- function(x, f, abortcall) {
  
  if(is.logical(x)) {
    .rcpp_setapply_col_Logical(x, f)
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_setapply_col_Integer(x, f)
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_setapply_col_Numeric(x, f)
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_setapply_col_Character(x, f)
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_setapply_col_Complex(x, f)
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_setapply_col_Raw(x, f)
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }
  
}
