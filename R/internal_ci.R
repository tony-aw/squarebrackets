
#' @keywords internal
#' @noRd
.ci_sub.atomic <- function(
    x, s, d, inv, chkdup, uniquely_named, .abortcall
) {
  
  if(is.list(s)) {
    s <- s[[1]]
  }
  
  if(ndim(x) == 1L) {
    lst <- list(
      ci_flat(x, s, inv, chkdup, uniquely_named = FALSE, .abortcall)
    )
  }
  else {
    lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
    lst[[d]] <- as.integer(ci_margin(
      x, s, d, inv, chkdup, uniquely_named = FALSE, .abortcall
    ))
  }
  
  return(lst)
}


#' @keywords internal
#' @noRd
.ci_sub1 <- function(
    x, s, d, inv, chkdup, uniquely_named, .abortcall
) {
  
  s <- s[[1]]
  
  if(ndim(x) == 1L) {
    lst <- list(
      ci_flat(x, s, inv, chkdup, uniquely_named = FALSE, .abortcall)
    )
  }
  else {
    lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
    for(i in d) {
      lst[[i]] <- as.integer(ci_margin(
        x, s, i, inv, chkdup, uniquely_named = FALSE, .abortcall
      ))
    }
  }
  
  return(lst)
}


#' @keywords internal
#' @noRd
.ci_sub0 <- function(
    x, s, d, inv, chkdup, uniquely_named, .abortcall
) {
  
  s <- .ci_sub_make_sub(s, d)
  
  lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
  for(i in seq_along(d)) {
    lst[[d[i]]] <- as.integer(ci_margin(
      x, s[[i]], d[i], inv, chkdup, uniquely_named = FALSE, .abortcall
    ))
  }
  
  return(lst)
}




#' @keywords internal
#' @noRd
.ci_sub_make_sub <- function(s, d) {
  if(is.list(s) && length(s) == 1L && length(d) != 1L) {
    s <- rep(s, length(d))
  }
  return(s)
}



#' @keywords internal
#' @noRd
.ci_sub_check <- function(x, s, d, ndim, .abortcall) {
  
  # check `d`:
  if(length(d) == 0L) {
    stop(simpleError("length(d) == 0L has not been captured", call = .abortcall))
  }
  if(!is.numeric(d)) {
    stop(simpleError("`d` must be a integer vector", call = .abortcall))
  }
  if(.any_badindx(as.integer(d), ndim)) {
    stop(simpleError("`d` out of range", call = .abortcall))
  }
  
  # check `s`:
  if(!is.list(s) && length(d) != 1L) {
    stop(simpleError("if `length(d) > 1`, `s` must be a list"))
  }
  if(is.list(s)) {
    badlen <- length(s) != 1L && length(s) != length(d)
    if(badlen) {
      stop(simpleError("if `s` is a list, `length(s)` must equal `length(d)`", call = .abortcall))
    }
  }
  
  
}

