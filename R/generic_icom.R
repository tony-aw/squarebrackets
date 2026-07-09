#' Compute Indices for Copy-On-Modify Substitution
#'
#' @description
#' The `_icom()` methods compute indices,
#' suitable for usage in R's native copy-on-modify substitution. \cr
#' \cr
#' The `arepl(x, sub, value)` function directly evaluates the expression \cr
#' `x[ sub[[1]], ... , sub[[ndim(x)]] ] <- value` \cr
#' in the calling environment. \cr
#' \cr
#' Demonstration:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' x <- array(...)
#' myss <- ss_icom(x, s, use)
#' arepl(x, myss, value)
#' 
#' y <- data.frame(...)
#' rows <- tt_icom(y, 1:10, 1, -1L)
#' cols <- tt_icom(y, c("a", "b"), 2L)
#' y[rows, cols] <- value
#' ```
#' 
#' These allow the user to benefit from the convenient index translations from 'squarebrackets',
#' whilst still using R's default copy-on-modification semantics
#' (instead of the semantics provided by 'squarebrackets'). \cr
#' \cr \cr
#' 
#' 
#' @param x vector, matrix, array, or data.frame; both atomic and recursive objects are supported.
#' @param i,s,slice,use See \link{squarebrackets_index_args}. \cr
#' Duplicates are not allowed.
#' @param sub a list of integer subscripts. \cr
#' The first element of the list corresponds to the first dimension (rows),
#' the second element to the second dimensions (columns),
#' etc. \cr
#' The length of `sub` must be equal to the length of `ndim(x)`. \cr
#' One cannot give an empty subscript;
#' instead fill in something like `seq_len(dim(x)[margin])`. \cr
#' @param value the replacement value
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... see \link{squarebrackets_ellipsis}.
#'
#'
#' @returns
#' For `ii_icom()`: \cr
#' A strictly positive numeric vector of indices. \cr
#' To be used in the flat form of the `[<-` operator. \cr
#' \cr
#' For `tt_icom()`: \cr
#' A strictly positivie numeric vector of either row or column indices. \cr
#' To be used in the first (for rows) or second (for columns) slot of the tabular form of the `[<-` operator. \cr
#' \cr
#' For `ss_icom()`: \cr
#' A list of strictlt positive integer vectors, containing array subscripts. \cr
#' To be used in the `arepl()` function. \cr
#' Can also be combined with \link{ss2ii} to use in the flat form of the `[<-` operator. \cr
#' \cr
#' For `arepl()`: \cr
#' Returns nothing, but modfies `x` in place using R's default semantics. \cr
#' \cr
#'
#'
#' @example inst/examples/generic_icom.R


#' @rdname sb_icom
#' @export
arepl <- function(x, sub, value) {
  if(!is.list(sub)) {
    stop("`sub` must be a list")
  }
  arr_expr <- substitute(x)
  lhs <- as.call(c(list(quote(`[`)), arr_expr, sub))
  full_expr <- call("<-", lhs, value)
  eval(full_expr, envir = parent.frame())
}

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
tt_icom <- function(x, slice, use, ...) {
  UseMethod("tt_icom", x)
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
  return(lst)
  
}

#' @rdname sb_icom
#' @export
tt_icom.default <- function(
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

