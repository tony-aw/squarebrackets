

#' @keywords internal
#' @noRd
.set_mat <- function(x, row, col, rp, tf, abortcall) {
  
  # SPECIAL CASE: neither rows nor columns specified
  if(is.null(row) && is.null(col)) {
    .rcpp_set_all(x, rp, tf, abortcall = sys.call())
    return(invisible(NULL))
  }
  
  
  if(!is.null(row) && is.null(col)) { # CASE 1: rows specified, columns missing
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp <- tf(x[row, ])
    }
    .check_rp_atomic(rp, length(row) * ncol(x), abortcall)
    col <- 0
  }
  else if(is.null(row) && !is.null(col)) { # CASE 2: columns specified, rows missing
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp <- tf(x[, col])
    }
    .check_rp_atomic(rp, length(col) * nrow(x), abortcall)
    row <- 0
  }
  else if(!is.null(row) && !is.null(col)) { # CASE 3: rows AND columns specified
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp <- tf(x[row, col])
    }
    .check_rp_atomic(rp, length(row) * length(col), abortcall)
  }
  
  # return:
  .rcpp_set_matrix(x, row, col, rp, abortcall)
  return(invisible(NULL))
  
  
}

 

#' @keywords internal
#' @noRd
.rcpp_set_matrix <- function(x, row, col, rp, abortcall) {
  row <- as.integer(row - 1L) 
  col <- as.integer(col - 1L)
  if(is.logical(x)) {
    .rcpp_set_matrix_Logical(x, row, col, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_matrix_Integer(x, row, col, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_matrix_Numeric(x, row, col, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_matrix_Character(x, row, col, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_matrix_Complex(x, row, col, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_matrix_Raw(x, row, col, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      "unsupported matrix type", call = abortcall
    ))
  }
  
}




