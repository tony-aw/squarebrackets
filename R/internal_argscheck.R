

#' @keywords internal
#' @noRd
.argscheck_rptf <- function(rp, tf, abortcall) {
  if(!missing(rp) && !missing(tf)) {
    stop(simpleError("cannot specify both `rp` and `tf`", call = abortcall))
  }
  if(missing(rp) && missing(tf)) {
    stop(simpleError("must specify either `rp` or `tf`", call = abortcall))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
  }
}

