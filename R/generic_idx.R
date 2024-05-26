#' Convert/Translate Indices (for Copy-On-Modify Substitution)
#'
#' @description
#' The `idx()` method converts indices. \cr
#' The type of output depends on the type of input index arguments given:
#' 
#'  - `idx(x, i = i, ...)`
#'  converts linear indices to a strictly positive integer vector of linear indices.
#'  - `idx(x, idx = idx, dims = dims, ...)`
#'  converts dimensional indices to a strictly positive integer vector of linear indices.
#'  - `idx(x, slice = slice, margin = margin, ...)`
#'  converts indices of one dimension to a strictly positive integer vector of
#'  indices for that specific dimension.
#' 
#' Vectors (both atomic and recursive) only have index argument `i`. \cr
#' Data.frame-like objects only have the `slice, margin` index argument pair. \cr
#' Arrays (both atomic and recursive) have the `idx, dims` index argument pair,
#' as well as the arguments `i` and `slice, margin`. \cr
#' \cr
#' The result of the `idx()` method
#' can be used inside the regular square-brackets operators. \cr
#' For example like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' x <- array(...)
#' my_indices <- idx(x, idx, dims)
#' x[my_indices] <- value
#' 
#' y <- data.frame(...)
#' rows <- idx(y, 1:10, 1, inv = TRUE)
#' cols <- idx(y, c("a", "b"), 2)
#' y[rows, cols] <- value
#' ```
#' 
#' thus allowing the user to benefit from the convenient index translations from 'squarebrackets',
#' whilst still using R's default copy-on-modification semantics
#' (instead of the deep copy semantics and
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics}
#' provided by 'squarebrackets'). \cr
#' \cr
#' The `idx()` method
#' is particularly handy for replacing or coercively transforming shallow subsets
#' of recursive objects,
#' without having to return a copy of the entire object. \cr
#' Thus combining `[<-` with `idx` is more efficient than \link{sb2_mod}
#' for recursive objects. \cr \cr
#' 
#' 
#' @param x vector, matrix, array, or data.frame; both atomic and recursive objects are supported.
#' @param i,idx,dims,inv See \link{squarebrackets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param slice see arguments `row` and `col` in \link{squarebrackets_indx_args}.
#' @param margin a single integer, specifying the dimension for `slice`.
#' @param chkdup see \link{squarebrackets_duplicates}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' A vector of strictly positive integer indices.
#'
#'
#'
#' @example inst/examples/idx1.R





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
  elements <- .indx_make_element(
    i, x, is_list = is.list(x), chkdup = chkdup, inv = inv, abortcall = sys.call()
  )
  return(elements)
}


#' @rdname idx1
#' @export
idx.array <- function(
    x, idx = NULL, dims = NULL, slice = NULL, margin = NULL, i = NULL, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  check_args <- c(
    !is.null(idx) && !is.null(dims),
    !is.null(slice) && !is.null(margin),
    !is.null(i)
  )
  if(sum(check_args)>1) {
    stop("incorrect combination of arguments given")
  }
  check_args <- is.null(idx) == is.null(dims) && is.null(slice) == is.null(margin)
  if(!check_args) {
    stop("incorrect combination of arguments given")
  }
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = is.list(x), chkdup = chkdup, inv = inv, abortcall = sys.call()
    )
    return(elements)
  }
  
  if(!is.null(slice) && !is.null(margin)) {
    .idx_slicemargin(x, slice, margin, inv, chkdup)
  }
  
  x.dim <- dim(x)
  ndims <- length(x.dim)
  .arr_check(x, idx, dims, ndims, abortcall = sys.call())
  lst <- .arr_lst_grid(
    x, ndims, idx, dims, chkdup = chkdup, inv = inv, abortcall = sys.call()
  )
  elements <- sub2ind(lst, x.dim, checks = FALSE)
  return(elements)
  
}

#' @rdname idx1
#' @export
idx.data.frame <- function(
    x, slice, margin, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)) {
  
  return(.idx_slicemargin(
    x, slice, margin, inv, chkdup, abortcall = sys.call()
  ))
  
}

#' @keywords internal
#' @noRd
.idx_slicemargin <- function(x, slice, margin, inv, chkdup, abortcall) {
  if(is.null(dim(x))) {
    stop(simpleError("`x` has no dimensions", call = abortcall))
  }
  obj <- stats::setNames(seq_len(dim(x)[margin]), dimnames(x)[[margin]])
  if(!is.atomic(slice)) {
    stop(simpleError("`slice` must be atomic", call = abortcall))
  }
  elements <- .indx_make_element(
    slice, obj, is_list = FALSE, chkdup = chkdup, inv = inv, abortcall = sys.call()
  )
  return(elements)
}
