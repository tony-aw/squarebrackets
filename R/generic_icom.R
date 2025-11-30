#' Compute Indices for Copy-On-Modify Substitution
#'
#' @description
#' The `_icom()` methods compute indices for copy-on-modify substitution.
#' 
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' x <- array(...)
#' my_ss2ii <- ss_icom(x, s, use)
#' x[my_ss2ii] <- value
#' 
#' y <- data.frame(...)
#' rows <- sbt_icom(y, 1:10, 1, inv = TRUE)
#' cols <- sbt_icom(y, c("a", "b"), 2)
#' y[rows, cols] <- value
#' ```
#' 
#' thus allowing the user to benefit from the convenient index translations from 'squarebrackets',
#' whilst still using R's default copy-on-modification semantics
#' (instead of the semantics provided by 'squarebrackets'). \cr
#' \cr \cr
#' 
#' 
#' @param x vector, matrix, array, or data.frame; both atomic and recursive objects are supported.
#' @param i,s,slice,use See \link{squarebrackets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#'
#'
#' @returns
#' A strictly positive numeric vector of indices. \cr \cr
#'
#'
#'
#' @example inst/examples/generic_icom.R





#' @rdname sb_icom
#' @export
ii_icom <- function(x, i = NULL, use = 1, ...) {
  .methodcheck.ii(x, i, use, sys.call())
  UseMethod("ii_icom", x)
}

#' @rdname sb_icom
#' @export
ss_icom <- function(x, s = NULL, use = 1:ndim(x), ...) {
  .methodcheck.ss(x, s, use, sys.call())
  UseMethod("ss_icom", x)
}


#' @rdname sb_icom
#' @export
sbt_icom <- function(x, slice, use, ...) {
  UseMethod("sbt_icom", x)
}

#' @rdname sb_icom
#' @export
ii_icom.default <- function(
    x, i = NULL, use = 1,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  elements <- ci_ii(
    x, i, use, chkdup, .abortcall = sys.call()
  )
  return(elements)
}


#' @rdname sb_icom
#' @export
ss_icom.default <- function(
    x, s = NULL, use = 1:ndim(x),
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  
  # s, d:
  if(.C_is_missing_idx(s)) {
    stop("`s` not specified")
  }
  .check_args_array(x, s, use, sys.call())
  lst <- ci_ss(
    x, s, use, chkdup, .abortcall = sys.call()
  )
  elements <- ss2ii(lst, dim(x), checks = FALSE)
  return(elements)
  
}

#' @rdname sb_icom
#' @export
sbt_icom.default <- function(
    x, slice = NULL, use = NULL,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  if(.C_is_missing_idx(slice) || .C_is_missing_idx(use)) {
    stop("`slice` and `use` not specified")
  }
  return(ci_margin(
    x, slice, abs(use), sign(use), chkdup, .abortcall = sys.call()
  ))
  
}

