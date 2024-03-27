

#' @keywords internal
#' @noRd
.check_in <- function(pos, n, abortcall) {
  error.txt2 <- simpleError("`pos` must be a strictly positive integer scalar", call = abortcall)
  
  if(!is.numeric(pos) || length(pos) != 1) {
    stop(error.txt2)
  }
  if(pos < 1) { # removed any(), as pos is scalar
    stop(error.txt2)
  }
  if(pos > n) { # removed any(), as pos is scalar
    stop(simpleError("subscript out of bounds", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.is_prepend <- function(pos, n) {
  if(pos == 1 || n == 1) { 
    return(TRUE)
  }
  return(FALSE)
}


#' @keywords internal
#' @noRd
.is_postpend <- function(pos, n) {
  if(pos == n || n == 1) {
    return(TRUE)
  }
  return(FALSE)
}



#' @keywords internal
#' @noRd
.sb_in_dim_before <- function(x, margin, pos, new, .attr, abortcall) {
  
  n.x <- dim(x)[[margin]]
  
  if(.is_prepend(pos, n.x)) {
    out <- abind::abind(new, x, along = margin)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- abind::abind(
    abind::asub(x, idx = seq_len(pos-1), dims = margin),
    new,
    abind::asub(x, idx = seq.int(pos, n.x), dims = margin),
    along = margin
  )
  out <- .fix_attr(out, .attr)
  return(out)
  
}

#' @keywords internal
#' @noRd
.sb_in_dim_after <- function(x, margin, pos, new, .attr, abortcall) {
  
  n.x <- dim(x)[[margin]]
  
  if(.is_postpend(pos, n.x)) {
    out <- abind::abind(x, new, along = margin)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- abind::abind(
    abind::asub(x, idx = seq_len(pos), dims = margin),
    new,
    abind::asub(x, idx = seq.int(pos+1, n.x), dims = margin),
    along = margin
  )
  out <- .fix_attr(out, .attr)
  return(out)
  
}