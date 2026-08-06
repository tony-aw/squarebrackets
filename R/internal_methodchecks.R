#

#' @keywords internal
#' @noRd
.methodcheck.ii <- function(x, abortcall) {
  if(is.data.frame(x)) {
    stop(simpleError("Use the `tt_` methods for data.frames", call = abortcall))
  }
  if(!is.atomic(x) && !is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  if(length(x) == 0) {
    stop(simpleError(
      "cannot operate on vector with zero length",
      call = abortcall
    ))
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
      "Use the `tt_` methods for data.frames",
      call = abortcall
    ))
  }
  if(!is.atomic(x) && !is.list(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  if(.C_all_dim_zero(dim(x))) {
    stop(simpleError(
      "cannot operate on array with all zero dimensions",
      call = abortcall
    ))
  }
  
  
}


#' @keywords internal
#' @noRd
.methodcheck.tt <- function(x, abortcall) {
  
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
  if(is.array(x) && !.is.array_like(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  if(is.data.frame(x) && !.is.data.frame_like(x)) {
    stop(simpleError("unsupported object", call = abortcall))
  }
  
  if(.C_all_dim_zero(dim(x))) {
    stop(simpleError(
      "cannot operate on object with all zero dimensions",
      call = abortcall
    ))
  }
  
}



#' @keywords internal
#' @noRd
.methodcheck.dt <- function(x, abortcall) {
  
  if(!data.table::is.data.table(x)) {
    stop(simpleError("`x` must be a data.table", call = abortcall))
  }
  if(!.is.data.frame_like(x)) {
    stop(simpleError("`x` must be a data.table", call = abortcall))
  }
  
  if(anyDuplicated(names(x))) {
    txt <- "`x` does not have unique variable names for all columns; \n fix this before subsetting"
    stop(simpleError(txt, call = abortcall))
  }
  
  
  if(.C_all_dim_zero(dim(x))) {
    stop(simpleError(
      "cannot operate on object with all zero dimensions",
      call = abortcall
    ))
  }
  
}

#' @keywords internal
#' @noRd
.is.data.frame_like <- function(x) {
  return(
    is.list(x) && length(x) == ncol(x) && inherits(x, "data.frame")
  )
}
