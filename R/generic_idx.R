#' Convert/Translate Indices (for Copy-On-Modify Substitution)
#'
#' @description
#' The `idx()` method converts indices. \cr
#' The type of output depends on the type of input index arguments given:
#' 
#'  - `idx(x, i = i, ...)`
#'  converts linear indices to a strictly positive integer vector of linear indices.
#'  - `idx(x, s = s, d = d, ...)`
#'  converts dimensional indices to a strictly positive integer vector of linear indices.
#'  - `idx(x, slice = slice, margin = margin, ...)`
#'  converts indices of one dimension to a strictly positive integer vector of
#'  indices for that specific dimension.
#' 
#' Vectors (both atomic and recursive) only have index argument `i`. \cr
#' Data.frame-like objects only have the `slice, margin` argument pair. \cr
#' Arrays (both atomic and recursive) have the `s, d` argument pair,
#' as well as the `i` argument and the `slice, margin` argument pair. \cr
#' \cr
#' The result of the `idx()` method
#' can be used inside the regular square-brackets operators. \cr
#' For example like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' x <- array(...)
#' my_sub2ind <- idx(x, s, d)
#' x[my_sub2ind] <- value
#' 
#' y <- data.frame(...)
#' rows <- idx(y, 1:10, 1, inv = TRUE)
#' cols <- idx(y, c("a", "b"), 2)
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
#' @param i,s,d,margin,slice,inv See \link{squarebrackets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#'
#'
#' @returns
#' For `idx(x, i = i, ...)` and `idx(x, s = s, d = d, ...)`: \cr
#' A strictly positive integer vector of flat indices. \cr
#' \cr
#' For `idx(x, margin = margin, slice = slice, ...)`: \cr
#' A strictly positive integer vector of indices
#' for the dimension specified in `margin`. \cr \cr
#'
#'
#'
#' @example inst/examples/generic_idx.R





#' @rdname idx1
#' @export
idx <- function(x, ...) {
  UseMethod("idx", x)
}


#' @rdname idx1
#' @export
idx.default <- function(
    x, i, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  elements <- ci_flat(
    x, i, inv, chkdup, .abortcall = sys.call()
  )
  return(elements)
}


#' @rdname idx1
#' @export
idx.array <- function(
    x, s = NULL, d = 1:ndim(x), slice = NULL, margin = NULL, i = NULL, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  # error checks:
  .internal_check_dots(list(...), sys.call())
  check_args <- c(
    !is.null(s) && !is.null(d),
    !is.null(slice) && !is.null(margin),
    !is.null(i)
  )
  if(sum(check_args) > 1L) {
    stop("incorrect combination of arguments given")
  }
  check_args <- is.null(slice) == is.null(margin)
  if(!check_args) {
    stop("incorrect combination of arguments given")
  }
  
  # flat indices:
  if(!is.null(i)) {
    elements <- elements <- ci_flat(
      x, i, inv, chkdup, .abortcall = sys.call()
    )
    return(elements)
  }
  
  # slice, margin:
  if(!is.null(slice) && !is.null(margin)) {
    return(ci_margin(x, slice, margin, inv, chkdup, .abortcall = sys.call()))
  }
  
  # s, d:
  if(is.null(s) || is.null(d)) {
    stop("`s` and/or `d` not specified")
  }
  .check_args_array(x, s, d, sys.call())
  x.dim <- dim(x)
  lst <- ci_sub(
    x, s, d, inv, chkdup, .abortcall = sys.call()
  )
  elements <- sub2ind(lst, x.dim, checks = FALSE)
  return(elements)
  
}

#' @rdname idx1
#' @export
idx.data.frame <- function(
    x, slice, margin, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(ci_df(
    x, slice, margin, inv, chkdup, .abortcall = sys.call()
  ))
  
}

