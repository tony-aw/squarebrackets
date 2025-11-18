#

#' @keywords internal
#' @noRd
.methodcheck.ii <- function(x, abortcall) {
  if(is.data.frame(x)) {
    stop(simpleError("Use the `sbt_` methods for data.frames", call = abortcall))
  }
  if(!is.atomic(x) && !is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.methodcheck.ss <- function(x, abortcall) {
  
  if(is.null(dim(x))) {
    stop(simpleError(
      "Use the `ii_` methods for non-dimensional objects",
      call = abortcall
    ))
  }
  if(is.data.frame(x)) {
    stop(simpleError(
      "Use the `sbt_` methods for data.frames",
      call = abortcall
    ))
  }
  if(!is.atomic(x) && !is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.methodcheck.sbt <- function(x, abortcall) {
  
  if(is.null(dim(x))) {
    stop(simpleError(
      "Use the `ii_` methods for non-dimensional objects",
      call = abortcall
    ))
  }
  if(ndim(x) != 2L) {
    stop(simpleError(
      "Use the `ss_` methods for dimensional objects with ndim(x) != 2L",
      call = abortcall
    ))
  }
  if(!is.atomic(x) && !is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  
}


