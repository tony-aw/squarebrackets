#' Find and Replace Present Values in mutable_atomic Objects By Reference
#'
#' @description
#' The `ma_setv(x, v rp)` function performs the equivalent of \cr
#' `x[which(x == v)] <- rp` \cr
#' but using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' \cr
#' This is faster than using `sb_set(x, i = which(x == v), rp = rp)`. \cr
#' \cr
#' Inspired by \code{collapse::}\link[collapse]{setv},
#' but written in 'C++' through 'Rcpp', with additional safety checks. \cr \cr
#' 
#' @param x a \link{mutable_atomic} \bold{variable}.
#' @param v non-missing (so no `NA` or `NaN`) atomic scalar to find.
#' @param rp atomic scalar giving the replacement value.
#' @param invert Boolean. \cr
#' If `FALSE` (default), the equivalent of `x[which(x == v()] <- rp` is performed; \cr
#' If `TRUE`, the equivalent of `x[which(x != v)] <- rp` is performed instead.
#' @param NA.safety Boolean. \cr
#' just like in \link{which},
#' `NA` and `NaN` results in `x==v` should be ignored,
#' thus `NA.safety` is `TRUE` by default. \cr
#' However, if it is known that `x` contains no `NA`s or `NaN`s,
#' setting `NA.safety` to `FALSE` will increase performance a bit. \cr
#' NOTE: Setting `NA.safety = FALSE` when `x` does contain `NA`s or `NaN`s,
#' may result in unexpected behaviour. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' 
#'
#'
#' 
#' 
#' 
#' @example inst/examples/ma_setv.R
#' 
#' @returns
#' Returns: VOID. This function modifies the object by reference. \cr
#' Do not use assignment like `x <- ma_setv(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#' 
#' 

#' @rdname ma_setv
#' @export
ma_setv <- function(x, v, rp, invert = FALSE, NA.safety = TRUE) {
  
  # Error handling: 
  if(!is.mutable_atomic(x)) {
    stop("`x` must be a mutable_atomic object")
  }
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  if(length(v) != 1 || length(rp) != 1 || !is.atomic(v) || !is.atomic(rp)) {
    stop("`v` and  replacement must be atomic scalars")
  }
  if(is.na(v)) {
    stop("`NA`/ `NaN` not allowed for `v`")
  }
  if(is.na(invert)) {
    stop("`invert` must be `TRUE` or `FALSE`")
  }
  
  # Function:
  if(isTRUE(NA.safety)) {
    .rcpp_setrv_safe(x, v, rp, invert,  sys.call())
    return(invisible(NULL))
  }
  if(isFALSE(NA.safety)) {
    .rcpp_setrv_fast(x, v, rp, invert,  sys.call())
    return(invisible(NULL))
  }
  stop("`NA.safety` must be `TRUE` or `FALSE`")
}
