#' Compute Grouped Indices
#'
#' @description
#' Given:
#' 
#'  * a sub-set function `f`;
#'  * an object `x` with its margin `m`;
#'  * and a grouping factor `grp`;
#' 
#' the `idx_by()` function takes `indices` \bold{per group} `grp`. \cr
#' The result of `idx_by()` can be supplied to the indexing arguments
#' (see \link{squarebrackets_indx_args})
#' to perform \bold{grouped} subset operations. \cr
#' \cr
#' 
#' @param x the object from which to compute the indices.
#' @param m a single non-negative integer giving the margin for which to compute indices. \cr
#' For flat indices or for non-dimensional objects, use `m = 0L`. \cr
#' @param f a subset function to be applied per group on `indices`. \cr
#' If `m == 0L`, `indices` is here defined as `setNames(1:length(x), names(x))`. \cr
#'  If `m > 0L`, `indices` is here defined as `setNames(1:dim(x)[m], dimnames(x)[[m]])`. \cr
#' The function must produce a character or integer vector as output. \cr
#' For example, to subset the last element per group, specify: \cr
#' `f = last`
#' @param grp a factor giving the groups.
#' @param parallel,mc.cores see \link[collapse]{BY}.
#'
#' @returns
#' A vector of indices.
#'
#'
#'
#' @example inst/examples/idx_by.R

#' @rdname idx_by
#' @export
idx_by <- function(x, m, f, grp, parallel = FALSE, mc.cores = 1L) {
  
  if(length(m) != 1L || !is.numeric(m) || m < 0L) {
    stop("`m` must be a single non-negative integer")
  }
  
  if(!is.function(f)) stop("`f` must be a subset function")
  if(!is.factor(grp)) stop("`grp` must be a factor")
  
  if(m == 0L) {
    r <- stats::setNames(seq_along(x), names(x))
  }
  else {
   r <- stats::setNames(seq_len(dim(x)[m]), dimnames(x)[[m]])
  }
  
  out <- collapse::BY.default(
    r, grp, f,
    reorder = FALSE, expand.wide = FALSE, return = "same",
    parallel = parallel, mc.cores = mc.cores
  )
  return(out)
}


