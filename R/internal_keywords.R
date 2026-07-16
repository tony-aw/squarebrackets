
#' @keywords internal
#' @noRd
.keyword_bi <- function(ellipsis, size) {
  
  # concatenate:
  if(length(ellipsis) == 1L) {
    out <- ellipsis[[1L]]
  }
  else {
    out <- do.call(c, ellipsis)
  }
  
  # check type:
  if(!is.numeric(out)) {
    stop("only numeric indices can be bilateral")
  }
  
  # conversion:
  if(.C_is_altrep(out) && length(out) > 2L) {
    out <- .C_convert_bi(out[1], size):.C_convert_bi(out[length(out)], size)
  }
  else {
    out <- .C_convert_bi(out, size)
  }
  
  return(out)
}


#' @keywords internal
#' @noRd
.keyword_ptrn <- function(ptrn, start, end, size) {
  if(size == 0L) {
    return(integer(0L))
  }
  if(!is.logical(ptrn) || anyNA(ptrn)) {
    stop("`ptrn` must be a logical vector without NAs")
  }
  out <- (start:end)[ptrn]
  return(out)
}

