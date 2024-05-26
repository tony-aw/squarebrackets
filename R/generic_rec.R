#' Access Recursive Subsets
#'
#' @description
#' The `sb2_rec()` method allows the user to access recursive subsets of lists. \cr
#' \cr
#' The `sb2_rec()` method also allows replacing or transforming a recursive subset of a list,
#' using R's default copy-on-modify in-place semantics,
#' by specifying the `rp` or `tf` argument, respectively. \cr \cr
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
#' using R's default in-place semantics. \cr
#' Since this is a replacement of a recursive subset,
#' `rp` does not necessarily have to be a list itself; \cr
#' `rp` can be any type of object. \cr
#' @param tf an optional function. If specified, performs `lst[[rec]] <- tf(lst[[rec]])`,
#' using R's default copy-on-modify in-place semantics. \cr \cr
#' 
#'
#' @returns
#' If `rp` and `tf` are not specified: Returns the recursive subset. \cr
#' \cr
#' If `rp` or `tf` is specified: Returns VOID,
#' but replaces or transforms the recursive subset,
#' using R's default copy-on-modify in-place semantics. \cr \cr
#' 
#'
#'
#'
#' @example inst/examples/generic_rec.R
#'

#' @name sb2_rec
NULL



#' @rdname sb2_rec
#' @export
sb2_rec <- function(lst, rec, rp, tf) {
  
  if(!is.list(lst)) {
    stop("`lst` must be a list")
  }
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  
  if(missing(rp) && missing(tf)) {
    return(lst[[rec]])
  }
  if(!missing(rp)) {
    eval.parent(substitute(lst[[rec]] <- rp))
  }
  if(!missing(tf)) {
    eval.parent(substitute(lst[[rec]] <- tf(lst[[rec]])))
  }
  
  return(invisible(NULL))
  
}



