
#' @keywords internal
#' @noRd
.mat_prepsub2 <- function(x, sub, dims) {
  if(is.atomic(sub)) {
    return(sub)
  }
  if(is.list(sub)) {
    if(length(sub) == 1L) {
      return(sub[[1L]])
    }
    else {
      return(sub[dims])
    }
  }
}

#' @keywords internal
#' @noRd
.mat_rowcol <- function(x, sub, dims, inv, chkdup, abortcall) {
  if(length(dims) == 1L) {
    if(is.list(sub)) {
      sub <- sub[[1L]]
    }
    if(dims == 1L) {
      row <- ci_margin(x, sub, 1L, inv, chkdup, FALSE, sys.call())
      col <- NULL
    }
    if(dims == 2L) {
      col <- ci_margin(x, sub, 2L, inv, chkdup, FALSE, sys.call())
      row <- NULL
    }
  }
  else {
    sub <- .mat_prepsub2(x, sub, dims)
    if(is.atomic(sub)) {
      row <- ci_margin(x, sub, 1L, inv, chkdup, FALSE, sys.call())
      col <- ci_margin(x, sub, 2L, inv, chkdup, FALSE, sys.call())
    }
    if(is.list(sub)) {
      row <- ci_margin(x, sub[[1L]], 1L, inv, chkdup, FALSE, sys.call())
      col <- ci_margin(x, sub[[2L]], 2L, inv, chkdup, FALSE, sys.call())
    }
  }
  
  out <- list(row = row, col = col)
  return(out)
}


#' @keywords internal
#' @noRd
.mat_x <- function(x, sub, dims, inv, red, chkdup, abortcall) {
  
  .ci_array_check(x, sub, dims, ndims(x), abortcall)
  
  rowcol <- .mat_rowcol(x, sub, dims, inv, chkdup, abortcall)
  row <- rowcol[[1L]]
  col <- rowcol[[2L]]
  
  # col:
  if(is.null(row)) {
    if(red && length(col) == 1L && nrow(x) == 1L) {
      return(x[[1L, col]])
    }
    else if(is.null(names(x))) {
      return(x[, col, drop = FALSE])
    }
    else {
      return(.internal_fix_names(x, \(x)x[, col, drop = FALSE]))
    }
  }
  
  
  # row:
  if(is.null(col)) {
    if(red && length(row) == 1L && ncol(x) == 1L) {
      return(x[[row, 1L]])
    }
    else if(is.null(names(x))) {
      return(x[row, , drop = FALSE])
    }
    else {
      return(.internal_fix_names(x, \(x)x[row, , drop = FALSE]))
    }
  }
  
  # row & col:
  if(red && length(row) == 1L && length(col) == 1L) {
    return(x[[row, col]])
  }
  else if(is.null(names(x))) {
    return(x[row, col, drop = FALSE])
  }
  else {
    return(.internal_fix_names(x, \(x)x[row, col, drop = FALSE]))
  }
  
}

#' @keywords internal
#' @noRd
.mat_mod_atomic <- function(x, sub, dims, inv, rp, tf, chkdup, abortcall) {
  
  .ci_array_check(x, sub, dims, ndims(x), abortcall)
  rowcol <- .mat_rowcol(x, sub, dims, inv, chkdup, abortcall)
  row <- rowcol$row
  col <- rowcol$col
  
  
  if(is.null(row)) row <- 1:nrow(x) # ALTREP
  if(is.null(col)) col <- 1:ncol(x) # ALTREP
  if(!missing(tf)) {
    rp <- tf(x[row, col, drop = FALSE])
  }
  
  .check_rp_atomic(rp, (length(row) * length(col)), abortcall = sys.call())
  x[row, col] <- rp
  
  return(x)
  
  
}

#' @keywords internal
#' @noRd
.mat_mod_list <- function(x, sub, dims, inv, rp, tf, chkdup, .lapply, abortcall) {
  
  .ci_array_check(x, sub, dims, ndims(x), abortcall)
  rowcol <- .mat_rowcol(x, sub, dims, inv, chkdup, abortcall)
  row <- rowcol$row
  col <- rowcol$col
  
  
  if(is.null(row)) row <- 1:nrow(x) # ALTREP
  if(is.null(col)) col <- 1:ncol(x) # ALTREP
  if(!missing(tf)) {
    rp <- .lapply(x[row, col, drop = FALSE], tf)
  }
  
  .check_rp_list(rp, (length(row) * length(col)), abortcall = sys.call())
  x[row, col] <- rp
  
  return(x)
  
  
}


#' @keywords internal
#' @noRd
.mat_set <- function(x, sub, dims, inv, chkdup, rp, tf, abortcall) {
  
  .ci_array_check(x, sub, dims, ndims(x), abortcall)
  rowcol <- .mat_rowcol(x, sub, dims, inv, chkdup, abortcall)
  row <- rowcol$row
  col <- rowcol$col
  
  
  
  if(!is.null(row) && is.null(col)) { # CASE 1: rows specified, columns missing
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp <- tf(x[row, ])
    }
    col <- 0
  }
  else if(is.null(row) && !is.null(col)) { # CASE 2: columns specified, rows missing
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp <- tf(x[, col])
    }
    row <- 0
  }
  else if(!is.null(row) && !is.null(col)) { # CASE 3: rows AND columns specified
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        "`tf` must be a function", call = abortcall
      ))
      rp <- tf(x[row, col])
    }
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
  
  if(typeof(x) != typeof(rp)) {
    message(sprintf("coercing replacement to %s", typeof(x)))
  }
  
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