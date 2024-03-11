#' Access Recursive Subsets
#'
#' @description
#' The `sb_rec()` method allows the user to access recursive subsets of lists. \cr
#' \cr
#' The `sb_rec()` method also allows replacing or transforming a recursive subset of a list,
#' using R's default in-place semantics,
#' by specifying the `rp` argument.
#' 
#' @param lst a list, or list-like object.
#' @param rec a vector of length `p`,
#' such that `lst[[rec]]` is equivalent to `lst[[ rec[1] ]]...[[ rec[p] ]]`,
#' providing all but the final indexing results in a list. \cr
#' When on a certain subset level of a nested list,
#' multiple subsets with the same name exist,
#' only the first one will be selected when performing recursive indexing by name,
#' due to the recursive nature of this type of subsetting.
#' @param rp optional. If specified, performs `lst[[rec]] <- rp`,
#' using R's default in-place semantics. \cr \cr
#' 
#'
#' @returns
#' If `rp` is not specified: Returns the recursive subset. \cr
#' If `rp` is specified: Returns nothing,
#' but replaces the recursive subset with `rp`,
#' using R's default in-place semantics. \cr \cr
#' 
#'
#'
#'
#' @example inst/examples/generic_rec.R
#'

#' @name sb_rec
NULL



#' @rdname sb_rec
#' @export
sb_rec <- function(lst, rec, rp) {
  
  if(!is.list(lst)) {
    stop("`lst` must be a list")
  }
  
  if(missing(rp)) {
    return(lst[[rec]])
  }
  
  if(!missing(rp)) {
    eval.parent(substitute(lst[[rec]] <- rp))
  }
  
}



