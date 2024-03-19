

#' @keywords internal
#' @noRd
.set_mat <- function(x, row, col, rp, tf, abortcall) {
  
  # CASE 1: rows specified, columns missing
  if(is.null(col)) { 
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp = tf(x[row, ])
    }
    if(length(rp) == 1) {
      .rcpp_set_row1(x, row, rp, abortcall)
      return(invisible(NULL))
    }
    .check_rp_atomic(rp, length(row) * ncol(x), abortcall)
    .rcpp_set_row(x, row, rp, abortcall)
    return(invisible(NULL))
  }
  
  # CASE 2: columns specified, rows missing
  if(is.null(row)) { 
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp = tf(x[, col])
    }
    if(length(rp) == 1) {
      .rcpp_set_col1(x, col, rp, abortcall)
      return(invisible(NULL))
    }
    .check_rp_atomic(rp, length(col) * nrow(x), abortcall)
    .rcpp_set_col(x, col, rp, abortcall)
    return(invisible(NULL))
  }
  
  # CASE 3: rows AND columns specified:
  if(!missing(tf)) {
    if(!is.function(tf)) stop(simpleError(
      "`tf` must be a function", call = abortcall
    ))
    rp = tf(x[row, col])
  }
  if(length(rp) == 1) {
    .rcpp_set_rowcol1(x, row, col, rp, abortcall)
    return(invisible(NULL))
  }
  .check_rp_atomic(rp, length(row) * length(col), abortcall)
  .rcpp_set_rowcol(x, row, col, rp, abortcall)
  return(invisible(NULL))
  
}

 


 

#' @keywords internal
#' @noRd
.rcpp_set_row <- function(x, row, rp, abortcall) {
  row <- as.integer(row - 1L)
  if(is.logical(x)) {
    .rcpp_set_row_Logical(x, row, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_row_Integer(x, row, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_row_Numeric(x, row, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_row_Character(x, row, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_row_Complex(x, row, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_row_Raw(x, row, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}

 

#' @keywords internal
#' @noRd
.rcpp_set_col <- function(x, col, rp, abortcall) {
  col <- as.integer(col - 1L)
  if(is.logical(x)) {
    .rcpp_set_col_Logical(x, col, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_col_Integer(x, col, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_col_Numeric(x, col, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_col_Character(x, col, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_col_Complex(x, col, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_col_Raw(x, col, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}

 

#' @keywords internal
#' @noRd
.rcpp_set_rowcol <- function(x, row, col, rp, abortcall) {
  row <- as.integer(row - 1L) 
 col <- as.integer(col - 1L)
  if(is.logical(x)) {
    .rcpp_set_rowcol_Logical(x, row, col, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_rowcol_Integer(x, row, col, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_rowcol_Numeric(x, row, col, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_rowcol_Character(x, row, col, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_rowcol_Complex(x, row, col, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_rowcol_Raw(x, row, col, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}

 


 

#' @keywords internal
#' @noRd
.rcpp_set_row1 <- function(x, row, rp, abortcall) {
  row <- as.integer(row - 1L)
  if(is.logical(x)) {
    .rcpp_set_row_Logical1(x, row, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_row_Integer1(x, row, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_row_Numeric1(x, row, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_row_Character1(x, row, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_row_Complex1(x, row, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_row_Raw1(x, row, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}


 

#' @keywords internal
#' @noRd
.rcpp_set_col1 <- function(x, col, rp, abortcall) {
  col <- as.integer(col - 1L)
  if(is.logical(x)) {
    .rcpp_set_col_Logical1(x, col, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_col_Integer1(x, col, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_col_Numeric1(x, col, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_col_Character1(x, col, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_col_Complex1(x, col, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_col_Raw1(x, col, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}


 

#' @keywords internal
#' @noRd
.rcpp_set_rowcol1 <- function(x, row, col, rp, abortcall) {
  row <- as.integer(row - 1L) 
 col <- as.integer(col - 1L)
  if(is.logical(x)) {
    .rcpp_set_rowcol_Logical1(x, row, col, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_rowcol_Integer1(x, row, col, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_rowcol_Numeric1(x, row, col, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_rowcol_Character1(x, row, col, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_rowcol_Complex1(x, row, col, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_rowcol_Raw1(x, row, col, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }

}



