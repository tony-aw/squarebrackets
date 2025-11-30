#' Compute Special Indices for data.frames
#'
#' @description
#' `idx_obs()` and `idx_vars()` produce an integer vector of row or column indices, respectively,
#' given formulas based on data.frame variables. \cr
#' \cr
#' 
#' @param x a data.frame
#' @param form a formula; see `Examples` section below.
#' @param use set to `1` to use the specified indices,
#' set to `-1` to use all indices **except** the specified indices. \cr \cr
#' 
#' @returns
#' A vector of indices.
#'
#'
#'
#' @example inst/examples/idx_dt.R

#' @rdname idx_dt
#' @export
idx_obs <- function(x, form, use = 1) {
  
  if(!is.data.frame(x)) {
    stop("`x` must be a data.frame")
  }
  if(!is.formula(form)) {
    stop("`form` must be a formula")
  }
  return(.idx_make_filter(x, form, use, sys.call()))
}


#' @rdname idx_dt
#' @export
idx_vars <- function(x, form, use = 1) {
  
  if(!is.data.frame(x)) {
    stop("`x` must be a data.frame")
  }
  if(!is.formula(form)) {
    stop("`form` must be a formula")
  }
  return(.idx_make_vars_range(x, form, use, sys.call()))
}



#' @keywords internal
#' @noRd
.idx_make_filter <- function(x, filter, use, abortcall) {
  
  if(length(filter) != 2L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  
  mm <- .with_internal(x, filter, abortcall)
  environment(filter) <- NULL
  
  if(!is.logical(mm)) {
    stop(simpleError("invalid formula given", call = abortcall))
  }
  if(use > 0)return(which(mm))
  if(use < 0)return(collapse::whichv(mm, FALSE))
  
}

#' @keywords internal
#' @noRd
.idx_make_vars_range <- function(x, form, use, abortcall) {
  
  if(length(form) != 3L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  vars <- all.vars(form)
  if(length(vars) != 2L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  nms <- names(x)
  pos1 <- .rcpp_dt_find_name(nms, vars[1L], 1L)
  pos2 <- .rcpp_dt_find_name(nms, vars[2L], 1L)
  
  rng <- pos1:pos2
  if(use < 1) {
    rng <- seq_len(length(x))[-rng]
  }
  return(rng)
}




