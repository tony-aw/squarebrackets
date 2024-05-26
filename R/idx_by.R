#' Compute Grouped Indices
#'
#' @description
#' Given:
#' 
#'  * a sub-set function `f`;
#'  * the complete range of indices `r` of some object `x`;
#'  * and a grouping factor `grp`;
#' 
#' the `idx_by()` function takes indices `f(r)` \bold{per group} `grp`. \cr
#' The result of `idx_by()` can be supplied to the indexing arguments
#' (see \link{squarebrackets_indx_args})
#' to perform \bold{grouped} subset operations. \cr
#' \cr
#' 
#' @param f a subset function to be applied per group on `r`. \cr
#' The function must accept a character or integer vector as input,
#' and produce a character or integer vector as output. \cr
#' For example, to subset the last element per group, specify: \cr
#' `f = last`
#' @param r an integer or character vector,
#' giving the complete range of indices of an object. \cr
#' For example: `colnames(x)`, `1:nrow(x)`, etc.
#' @param grp a factor giving the groups.
#' Make sure its order corresponds to `i` and `r`,
#' otherwise it makes no sense.
#' @param parallel,mc.cores see \link[collapse]{BY}.
#'
#' @returns
#' A vector of indices of the same type as `r`.
#'
#'
#'
#' @example inst/examples/idx_by.R

#' @rdname idx_by
#' @export
idx_by <- function(f, r, grp, parallel = FALSE, mc.cores = 1L) {
  if(!is.function(f)) stop("`f` must be a subset function")
  if(!is.numeric(r) && !is.character(r)) stop("`r` must be an integer or character vector")
  if(!is.factor(grp)) stop("`grp` must be a factor")
  out <- collapse::BY.default(
    r, grp, f,
    reorder = FALSE, expand.wide = FALSE, return = "same",
    parallel = parallel, mc.cores = mc.cores
  )
  return(out)
}


