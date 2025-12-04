

#' @keywords internal
#' @noRd
.dt_make_args <- function(x, s, d, obs, vars, inv, chkdup, abortcall) {
  rowcol <- list(NULL, NULL)
  if(!.C_is_missing_idx(obs)) {
    rowcol[[1L]] <- ci_obs(
      x, obs, inv, chkdup, TRUE, sys.call()
    )
  }
  if(!.C_is_missing_idx(vars)) {
    rowcol[[2L]] <- ci_vars(
      x, vars, inv, chkdup, TRUE, sys.call()
    )
  }
  if(length(d) > 0L && !.C_is_missing_idx(s)) {
    .ci_ss_check(x, s, d, 2L, sys.call())
    rowcol <- ci_ss(x, s, d, inv, chkdup, TRUE, sys.call())
  }
  return(rowcol)
}


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
<<<<<<< Updated upstream
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
=======
.dt_mod <- function(x, row, col, rp, abortcall) {
>>>>>>> Stashed changes
  
  row <- as.integer(row)
  extraction <- collapse::qDF(collapse::ss(x, j = col, check = FALSE))
  extraction[row, ] <- rp
  extraction <- unname(unclass(extraction), force = TRUE)
  data.table::set(x, j = col, value = extraction)
  
  return(x)
}

