

#' @keywords internal
#' @noRd
.flat_x <- function(x, i, use, chkdup, abortcall) {
  elements <- ci_ii(x, i, use, chkdup, FALSE, .abortcall = abortcall)
  return(x[elements])
}


#' @keywords internal
#' @noRd
.flat_mod <- function(x_expr, x_shadow, env, i, use, chkdup, rp, tf, abortcall) {
  
  
  if(.C_is_missing_idx(i)) {
    .all_mod(x_expr, x_shadow, env, rp, tf)
    return(invisible(NULL))
  }
  indices <- ci_ii(x_shadow, i, use, chkdup, FALSE, sys.call())
  n.i <- length(indices)
  
  
  if(length(x_shadow) == 0L || n.i == 0L) {
    return(invisible(NULL))
  }
  
  if(!missing(rp)) {
    value <- rp
  }
  else {
    tf_expr <- substitute(
      tf(X[indices]),
      list(tf = tf, X = x_expr, indices = indices)
    )
    value <- eval(tf_expr, env)
  }
  
  .check_rp(x_shadow, value, n.i, sys.call())
  
  expr <- substitute(
    X[indices] <- value,
    list(X = x_expr, indices = indices, value = value)
  )
  eval(expr, envir = env)
  
  return(invisible(NULL))
  
}



#' @keywords internal
#' @noRd
.flat_set_atomic <- function(x, elements, use, rp, tf, chkdup, abortcall) {
  
  .argscheck_rptf(rp, tf, abortcall)
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    rp <- tf(x[elements])
  }
  
  .check_rp(x, rp, n.i, abortcall)
  
  .rcpp_set_vind(x, elements, rp, abortcall)
  return(invisible(NULL))
  
}



#' @keywords internal
#' @noRd
.rcpp_set_vind <- function(x, ind, rp, abortcall) {
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  if(is.integer(ind)) {
    .rcpp_set_vind_32_atomic(x, ind, rp)
    return(invisible(NULL))
  }
  if(is.double(ind)) {
    .rcpp_set_vind_64_atomic(x, ind, rp)
    return(invisible(NULL))
  }
  else {
    return(invisible(NULL))
  }
}

