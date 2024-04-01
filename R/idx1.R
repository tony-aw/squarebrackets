#' Compute Flat Integer Indices (for Copy-On-Modify Substitution)
#'
#' @description
#' The `idx1()` method
#' translates the given indices/subscripts to flat/linear integer indices. \cr
#' \cr
#' This function can be used inside the regular square brackets operators
#' (without commas; as stated, these are linear indices). \cr
#' For example like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[idx1(x, ...)] <- value
#' # OR
#' my_indices <- idx1(x, ...)
#' x[my_indices] <- value
#' 
#' ```
#' 
#' thus allowing the user to benefit from the convenient index translations from 'squarebrackets',
#' whilst still using R's default copy-on-modification semantics
#' (instead of the deep copy semantics and
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics}
#' provided by 'squarebrackets'). \cr
#' \cr
#' `idx1()` supports any `x` that is a vector, matrix, or array,
#' regardless if `x` is atomic or recursive. \cr
#' \cr
#' For data.frames, see \code{collapse::}\link[collapse]{fsubset}. \cr \cr
#' 
#' 
#' @param x vector, matrix, or array; both atomic and recursive objects are supported.
#' @param i,row,col,idx,dims,rcl,inv See \link{squarebrackets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param chkdup see \link{squarebrackets_duplicates}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' A vector of flat/linear integer indices.
#'
#'
#'
#' @example inst/examples/idx1.R


#' @rdname idx1
#' @export
idx1 <- function(x, ...) {
  UseMethod("idx1", x)
}


#' @rdname idx1
#' @export
idx1.default <- function(
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
idx1.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = is.list(x), chkdup = chkdup, inv = inv, abortcall = sys.call()
    )
    return(elements)
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = inv, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = inv, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(integer(0))
  }
  if(is.null(row) && is.null(col)) {
    return(seq_along(x))
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  
  elements <- sub2ind(list(row, col), dim(x), checks = FALSE)
  return(elements)
}


#' @rdname idx1
#' @export
idx1.array <- function(
    x, idx = NULL, dims = NULL, rcl = NULL, i = NULL, inv = FALSE,
    ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = is.list(x), chkdup = chkdup, inv = inv, abortcall = sys.call()
    )
    return(elements)
  }
  
  if(!is.null(rcl)) {
    elements <- .sb3d_get_elements(
      x, row = rcl[[1]], col = rcl[[2]], lyr = rcl[[3]], inv, chkdup = chkdup, abortcall = sys.call()
    )
    return(elements)
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
