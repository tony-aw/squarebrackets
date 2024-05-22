

#' @keywords internal
#' @noRd
.sb3d_ifelse <- function(x, row, col, lyr, rat) {
  
  null_row <- is.null(row)
  null_col <- is.null(col)
  null_lyr <- is.null(lyr)
  
  if(null_row && null_col && null_lyr) {
    return(x)
  }
  if(null_row && null_col) {
    if(rat) {
      x <- .fix_attr(x[, , lyr, drop = FALSE], attributes(x))
    } else{ x <- x[, , lyr, drop = FALSE] }
    
    return(x)
  }
  if(null_row && null_lyr) {
    if(rat) {
      x <- .fix_attr(x[, col, , drop = FALSE], attributes(x))
    } else{ x <- x[, col, , drop = FALSE] }
    
    return(x)
  }
  if(null_col && null_lyr) {
    if(rat) {
      x <- .fix_attr(x[row, , , drop = FALSE], attributes(x))
    } else{ x <- x[row, , , drop = FALSE] }
    
    return(x)
  }
  if(null_row) {
    if(rat) {
      x <- .fix_attr(x[, col, lyr, drop = FALSE], attributes(x))
    } else{ x <- x[, col, lyr, drop = FALSE] }
    
    return(x)
  }
  if(null_col) {
    if(rat) {
      x <- .fix_attr(x[row, , lyr, drop = FALSE], attributes(x))
    } else{ x <- x[row, , lyr, drop = FALSE] }
    
    return(x)
  }
  if(null_lyr) {
    if(rat) {
      x <- .fix_attr(x[row, col, , drop = FALSE], attributes(x))
    } else{ x <- x[row, col, , drop = FALSE] }
    
    return(x)
  }
  
  if(rat) {
    x <- .fix_attr(x[row, col, lyr, drop = FALSE], attributes(x))
  } else{ x <- x[row, col, lyr, drop = FALSE] }
  
  return(x)
}


#' @keywords internal
#' @noRd
.sb3d_x <- function(x, row, col, lyr, rat, chkdup, abortcall) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim.sb_x(row, x,  1, abortcall = abortcall)
  }
  if(!is.null(col)) {
    col <- .indx_make_dim.sb_x(col, x,  2, abortcall = abortcall)
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim.sb_x(lyr, x,  3, abortcall = abortcall)
  }
  
  return(.sb3d_ifelse(x, row, col, lyr, rat))
}


#' @keywords internal
#' @noRd
.sb3d_rm <- function(x, row, col, lyr, rat, chkdup, abortcall) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = TRUE, abortcall = abortcall)
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = TRUE, abortcall = abortcall)
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, chkdup = chkdup, inv = TRUE, abortcall = abortcall)
  }
  
  return(.sb3d_ifelse(x, row, col, lyr, rat))
}


#' @keywords internal
#' @noRd
.sb3d_mod <- function(x, row, col, lyr, inv, rp, tf, chkdup, abortcall) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = inv, abortcall = abortcall)
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = inv, abortcall = abortcall)
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, chkdup = chkdup, inv = inv, abortcall = abortcall)
  }
  
  if(.any_empty_indices(row, col, lyr)) {
    return(x)
  }
  
  x.dim <- dim(x)
  if(is.null(row)) row <- seq_len(x.dim[1L])
  if(is.null(col)) col <- seq_len(x.dim[2L])
  if(is.null(lyr)) lyr <- seq_len(x.dim[3L])
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[row, col, lyr, drop = FALSE])
  }
  
  .check_rp_atomic(rp, (length(row) * length(col) * length(lyr)), abortcall)
  x[row, col, lyr] <- rp
  
  return(x)
  
}

#' @keywords internal
#' @noRd
.sb3d_get_elements <- function(x, row, col, lyr, inv, chkdup, abortcall) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = inv, abortcall = abortcall)
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = inv, abortcall = abortcall)
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, chkdup = chkdup, inv = inv, abortcall = abortcall)
  }
  
  if(.any_empty_indices(row, col, lyr)) {
    return(invisible(NULL))
  }
  
  if(is.null(row) && is.null(col) && is.null(lyr)) {
    return(seq_along(x))
  }
  
  x.dim <- dim(x)
  if(is.null(row)) row <- seq_len(x.dim[1L])
  if(is.null(col)) col <- seq_len(x.dim[2L])
  if(is.null(lyr)) lyr <- seq_len(x.dim[3L])
  
  dimcumprod <- as.integer(cumprod(x.dim))
  elements <- .rcpp_sub2ind_3d(
    as.integer(row), as.integer(col), as.integer(lyr), dimcumprod
  )
  
  return(elements)
}



