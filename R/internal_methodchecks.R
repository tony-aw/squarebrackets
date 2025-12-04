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
<<<<<<< Updated upstream
=======
  if(.C_is_missing_idx(use)) {
    stop(simpleError("`use` cannot be specified as `NULL` or `0L`", call = abortcall))
  }
  if(.C_all_dim_zero(dim(x))) {
    stop(simpleError(
      "cannot operate on array with all zero dimensions",
      call = abortcall
    ))
  }
>>>>>>> Stashed changes
  
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
  
<<<<<<< Updated upstream
=======
  if(!is.numeric(use) || anyNA(use)) {
    stop(simpleError("`use` must be a numeric vector without missing values", call = abortcall))
  }
  if(.C_all_dim_zero(dim(x))) {
    stop(simpleError(
      "cannot operate on object with all zero dimensions",
      call = abortcall
    ))
  }
>>>>>>> Stashed changes
}


