#

#' @keywords internal
#' @noRd
.methodcheck.i <- function(x, abortcall) {
  if(is.list(x)) {
    stop(simpleError("Use the `i2_` methods for recursive objects", call = abortcall))
  }
  if(!is.atomic(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.methodcheck.i2 <- function(x, abortcall) {
  
  if(is.atomic(x)) {
    stop("Use the `i_` methods for atomic objects")
  }
  if(is.data.frame(x)) {
    stop("Use the `ss2_` methods for data.frame-like objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
}

#' @keywords internal
#' @noRd
.methodcheck.ss <- function(x, abortcall) {
  
  if(is.list(x)) {
    stop("Use the `ss2_` methods for recursive objects")
  }
  if(!is.atomic(x)) {
    stop("unsupported object")
  }
  
}

#' @keywords internal
#' @noRd
.methodcheck.ss2 <- function(x, abortcall) {
  
  if(is.atomic(x)) {
    stop("Use the `ss_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
}

