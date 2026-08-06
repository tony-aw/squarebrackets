
#' @keywords internal
#' @noRd
.all_missing_indices <- function(lst) {
  if(!is.list(lst)) lst <- list(lst)
  check <- vapply(lst, .C_is_missing_idx, FUN.VALUE = logical(1L))
  if(all(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @keywords internal
#' @noRd
.all_missing_s_d <- function(s, d) {
  if(.C_is_missing_idx(s) || length(s) == 0L) {
    return(TRUE)
  }
  if(.C_is_missing_idx(d) || length(d) == 0L) {
    return(TRUE)
  }
  return(FALSE)
}

#' @keywords internal
#' @noRd
.all_mod_list <- function(x, rp, tf, abortcall) {
  if(!missing(tf) && !is.null(tf)) {
    rp <- tf(x)
  }
  .check_rp(x, rp, length(x), abortcall = sys.call())
  x[] <- rp
  return(x)
}


.all_mod <- function(x_expr, x_shadow, env, rp, tf) {
  
  if(length(x_shadow) == 0L) {
    return(invisible(NULL))
  }
  
  if(!missing(rp)) {
    value <- rp
  }
  else {
    tf_expr <- substitute(
      tf(X[]),
      list(tf = tf, X = x_expr)
    )
    value <- eval(tf_expr, env)
  }
  
  expr <- substitute(
    X[] <- value,
    list(X = x_expr, value = value)
  )
  eval(expr, envir = env)
  
  return(invisible(NULL))
  
}


#' @keywords internal
#' @noRd
.all_set_atomic <- function(x, rp, tf, abortcall) {
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop(simpleError("`tf` must be a function", call = abortcall))
    }
    rp <- tf(x)
  }
  
  rp <- .internal_coerce_rp(x, rp, abortcall)
  
  .rcpp_set_all_atomic(x, rp)
  return(invisible(NULL))
}



