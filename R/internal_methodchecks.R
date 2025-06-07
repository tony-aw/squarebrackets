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
    stop(simpleError(
      "Use the `i_` methods for atomic objects",
      call = abortcall
    ))
  }
  if(is.data.frame(x)) {
    stop(simpleError(
      "Use the `ss2_` methods for data.frame-like objects",
      call = abortcall
    ))
  }
  if(!is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.methodcheck.ss <- function(x, abortcall) {
  
  if(is.null(dim(x))) {
    stop(simpleError(
      "cannot use the `ss_`/`ss2_` methods on non-dimensional objects",
      call = abortcall
    ))
  }
  if(is.list(x)) {
    stop(simpleError(
      "Use the `ss2_` methods for recursive objects",
      call = abortcall
    ))
  }
  if(!is.atomic(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.methodcheck.ss2 <- function(x, abortcall) {
  
  if(is.null(dim(x))) {
    stop(simpleError(
      "cannot use the `ss_`/`ss2_` methods on non-dimensional objects",
      call = abortcall
    ))
  }
  if(is.atomic(x)) {
    stop(simpleError(
      "Use the `ss_` methods for atomic objects",
      call = abortcall
    ))
  }
  if(!is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  
}

