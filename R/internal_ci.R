
#' @keywords internal
#' @noRd
.ci_ss.atomic <- function(
    x, s, d, use, chkdup, uniquely_named, .abortcall
) {
  
  if(is.list(s)) {
    s <- s[[1]]
  }
  
  if(ndim(x) == 1L) {
    lst <- list(as.integer(ci_margin(
      x, s, 1L, use, chkdup, uniquely_named = FALSE, .abortcall
    )))
  }
  else {
    lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
    for(i in seq_along(d)) {
      iD <- d[i]
      lst[[iD]] <- as.integer(ci_margin(
        x, s, iD, use[i], chkdup, uniquely_named = FALSE, .abortcall
      ))
    }
    
  }
  
  return(lst)
}


#' @keywords internal
#' @noRd
.ci_ss1 <- function(
    x, s, d, use, chkdup, uniquely_named, .abortcall
) {
  
  s <- s[[1]]
  
  if(ndim(x) == 1L) {
    lst <- list(
      ci_margin(x, s, 1L, use, chkdup, uniquely_named = FALSE, .abortcall)
    )
  }
  else {
    lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
    for(i in seq_along(d)) {
      iD <- d[i]
      lst[[iD]] <- as.integer(ci_margin(
        x, s, iD, use[i], chkdup, uniquely_named = FALSE, .abortcall
      ))
    }
  }
  
  return(lst)
}


#' @keywords internal
#' @noRd
.ci_ss0 <- function(
    x, s, d, use, chkdup, uniquely_named, .abortcall
) {
  
  s <- .ci_ss_make_sub(s, d)
  
  lst <- lapply(dim(x), \(y) 1:y ) # create list of ALTREP compact integers
  for(i in seq_along(d)) {
    iD <- d[i]
    lst[[iD]] <- as.integer(ci_margin(
      x, s[[i]], iD, use[i], chkdup, uniquely_named = FALSE, .abortcall
    ))
  }
  
  return(lst)
}




#' @keywords internal
#' @noRd
.ci_ss_make_sub <- function(s, d) {
  if(is.list(s) && length(s) == 1L && length(d) != 1L) {
    s <- rep(s, length(d))
  }
  return(s)
}



#' @keywords internal
#' @noRd
.ci_ss_check <- function(x, s, d, ndim, .abortcall) {
  
  # check `use`:
  if(length(d) == 0L) {
    stop(simpleError("length(use) == 0L has not been captured", call = .abortcall))
  }
  if(!is.numeric(d)) {
    stop(simpleError("`use` must be a integer vector", call = .abortcall))
  }
  if(.any_badindx(as.integer(d), ndim)) {
    stop(simpleError("`use` out of range", call = .abortcall))
  }
  if(anyDuplicated(d)) {
    stop(simpleError("`use` cannot have duplicate values", call = .abortcall))
  }
  
  # check `s`:
  if(is.list(s)) {
    badlen <- length(s) != 1L && length(s) != length(d)
    if(badlen) {
      stop(simpleError("if `s` is a list, `length(s)` must equal `length(use)`", call = .abortcall))
    }
  }
  
  
}

