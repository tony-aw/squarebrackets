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
#' of: \cr
#' \link{sb_x}, \link{sb_rm}, \link{sb_mod}, \link{sb_set}, or \link{sb_coe}, \cr
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
#' @examples
#' 
#' # vectors ====
#' (a <- 1:20)
#' (grp <- factor(rep(letters[1:5], each = 4)))
#' 
#' # get the last element of `a` for each group in `grp`:
#' i <- idx_by(last, 1:length(a), grp)
#' sb_x(cbind(a, grp), row = i)
#' 
#' 
#' # data.frame ====
#' x <- data.frame(
#'   a = sample(1:20),
#'   b = letters[1:20],
#'   group = factor(rep(letters[1:5], each = 4))
#' )
#' print(x)
#' # get the first row for each group in data.frame `x`:
#' row <- idx_by(first, 1:nrow(x), x$group)
#' sb_x(x, row)
#' # get the first row for each group for which a > 10:
#' x2 <- sb_x(x, filter = ~ a > 10)
#' row <- na.omit(idx_by(first, 1:nrow(x2), x2$group))
#' sb_x(x2, row)
#'  


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


