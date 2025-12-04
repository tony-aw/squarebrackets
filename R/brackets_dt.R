



#' @keywords internal
#' @noRd
.dt_prep_rp <- function(rp) {
  if(is.list(rp)) {
    attributes(rp) <- NULL
    return(rp)
  }
  else {
    return(rp)
  }
}


#' @keywords internal
#' @noRd
.dt_transform <- function(x, row, col, tf) {
  if(.C_is_missing_idx(row)) {
    rp <- tf(collapse::ss(x, j = col, check = FALSE))
  }
  else {
    rp <- tf(collapse::ss(x, i = as.integer(row), j = col, check = FALSE))
  }
  
  rp <- .dt_prep_rp(rp)
  
  return(rp)
}


#' @keywords internal
#' @noRd
.dt_mod <- function(x, row, col, rp, abortcall) {
  
  row <- as.integer(row)
  extraction <- collapse::qDF(collapse::ss(x, j = col, check = FALSE))
  extraction[row, ] <- rp
  extraction <- unname(unclass(extraction), force = TRUE)
  data.table::set(x, j = col, value = extraction)
  
  return(x)
}

