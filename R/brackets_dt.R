

#' @keywords internal
#' @noRd
.dt_rowcol <- function(x, s, d, inv, chkdup, abortcall) {
  if(length(d) == 1L) {
    if(is.list(s)) {
      s <- s[[1L]]
    }
    if(d == 1L) {
      row <- ci_margin(x, s, 1L, inv, chkdup, TRUE, sys.call())
      col <- NULL
    }
    if(d == 2L) {
      col <- ci_margin(x, s, 2L, inv, chkdup, TRUE, sys.call())
      row <- NULL
    }
  }
  else {
    s <- .mat_prepsub2(x, s, d)
    row <- ci_margin(x, s[[1L]], 1L, inv, chkdup, TRUE, sys.call())
    col <- ci_margin(x, s[[2L]], 2L, inv, chkdup, TRUE, sys.call())
  }
  
  out <- list(row, col)
  return(out)
}


#' @keywords internal
#' @noRd
.dt_make_args <- function(x, s, d, obs, vars, inv, chkdup, abortcall) {
  rowcol <- list(NULL, NULL)
  if(!is.null(obs)) {
    rowcol[[1L]] <- ci_obs(
      x, obs, inv, chkdup, TRUE, sys.call()
    )
  }
  if(!is.null(vars)) {
    rowcol[[2L]] <- ci_vars(
      x, vars, inv, chkdup, TRUE, sys.call()
    )
  }
  if(length(d) > 0L && !is.null(s)) {
    .ci_sub_check(x, s, d, 2L, sys.call())
    rowcol <- .dt_rowcol(x, s, d, inv, chkdup, abortcall = sys.call())
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
    rp <- unname(unclass(rp), force = TRUE)
    return(rp)
  }
  else {
    return(rp)
  }
}


#' @keywords internal
#' @noRd
.dt_transform <- function(x, row, col, tf, .lapply) {
  if(is.null(row)) {
    rp <- .lapply(collapse::ss(x, j = col, check = FALSE), tf)
  }
  else {
    rp <- .lapply(collapse::ss(x, i = as.integer(row), j = col, check = FALSE), tf)
  }
  
  rp <- .dt_prep_rp(rp)
  
  return(rp)
}


#' @keywords internal
#' @noRd
.dt_mod_whole <- function(x, col, rp, .lapply, abortcall) {
  
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

