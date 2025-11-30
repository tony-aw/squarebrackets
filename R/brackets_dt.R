


#' @keywords internal
#' @noRd
.dt_check_needcoe <- function(x, col, rp) {
  check <- .rcpp_dt_needcoe(x, col, rp)
  return(check)
}



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
.dt_mod_whole <- function(x, col, rp, abortcall) {
  
  data.table::set(x, j = col, value = rp)
  
  return(x)
}


#' @keywords internal
#' @noRd
.dt_mod_partialset <- function(x, row, col, rp, abortcall) {
  
  row <- as.integer(row)
  data.table::set(x, i = row, j = col, value = rp)
  
  return(x)
}


#' @keywords internal
#' @noRd
.dt_mod_partialcoe <- function(x, row, col, rp, abortcall) {
  
  row <- as.integer(row)
  extraction <- collapse::qDF(collapse::ss(x, j = col, check = FALSE))
  extraction[row, ] <- rp
  extraction <- unname(unclass(extraction), force = TRUE)
  data.table::set(x, j = col, value = extraction)
  
  return(x)
}

