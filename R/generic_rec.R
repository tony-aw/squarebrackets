#' Access, Replace, Transform, Remove, or Extend Recursive Subsets
#'
#' @description
#' The `sb2_rec()` and `sb2_recin()` methods
#' are essentially convenient wrappers around `[[` and `[[<-`,
#' respectively. \cr
#' Unlike `[[` and `[[<-`, these are actually S3 methods,
#' so package authors can create additional method dispatches. \cr
#' \cr
#' `sb2_rec()` will access recursive subsets of lists. \cr
#' \cr
#' `sb2_recin()` can do the following things: \cr
#' 
#'  - replace or transform recursive subsets of a list,
#'  using R's default Copy-On-Modify semantics,
#'  by specifying the `rp` or `tf` argument, respectively.
#'  - remove a recursive subset of a list,
#'  using R's default Copy-On-Modify semantics,
#'  by specifying argument `rp = NULL`.
#'  - extending a list with additional recursive elements,
#'  using R's default Copy-On-Modify semantics. \cr
#'  This is done by specifying an out-of-bounds index in argument `rec`,
#'  and entering the new values in argument `rp`. \cr
#'  Note that adding surface level elements of a dimensional list
#'  will remove the dimension attributes of that list. \cr \cr
#' 
#' 
#' 
#' @param x a list, or list-like object.
#' @param rec a strictly positive integer vector or character vector, of length `p`,
#' such that `sb2_rec(x, rec)` is equivalent to `x[[ rec[1] ]]...[[ rec[p] ]]`,
#' providing all but the final indexing results in a list. \cr
#' When on a certain subset level of a nested list,
#' multiple subsets with the same name exist,
#' only the first one will be selected when performing recursive indexing by name,
#' since recursive indexing can only select a single element.
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp optional, and allows for multiple functionalities:
#'  - In the simplest case, performs `x[[rec]] <- rp`,
#' using R's default semantics. \cr
#' Since this is a replacement of a recursive subset,
#' `rp` does not necessarily have to be a list itself; \cr
#' `rp` can be any type of object.
#' - Specifying `rp = NULL` will \bold{remove} (recursive) subset `sb(x, rec)`. \cr
#' To specify actual `NULL` instead of removing a subset, use `rp = list(NULL)`.
#' - When `rec` is an integer, and specifies an out-of-bounds subset,
#' `sb2_recin()` will add value `rp` to the list. \cr
#' Any empty positions in between will be filled with `NA`.
#' - When `rec` is character, and specifies a non-existing name,
#' `sb2_recin()` will add value `rp` to the list as a new element at the end.
#' @param tf an optional function. If specified, performs `x[[rec]] <- tf(x[[rec]])`,
#' using R's default Copy-On-Modify semantics. \cr
#' Does not support extending a list like argument `rp`.
#' 
#' 
#' @details
#' Since recursive objects are references to other objects,
#' extending a list or removing an element of a list does not copy the entire list,
#' in contrast to atomic vectors. \cr \cr
#' 
#'
#' @returns
#' For `sb2_rec()`: \cr
#' Returns the recursive subset. \cr
#' \cr
#' For `sb2_recin(..., rp = rp)`: \cr
#' Returns VOID,
#' but replaces, adds, or removes the specified recursive subset,
#' using R's default Copy-On-Modify semantics. \cr
#' \cr
#' For `sb2_recin(..., tf = tf)`: \cr
#' Returns VOID,
#' but transforms the specified recursive subset,
#' using R's default Copy-On-Modify semantics. \cr \cr
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
sb2_rec <- function(x, ...) {
  
  if(is.atomic(x)) {
    stop("Use the `sb_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb2_rec", x)
}


#' @rdname sb2_rec
#' @export
sb2_rec.default <- function(x, rec, ...) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!is.numeric(rec) && !is.character(rec)) {
    stop("`rec` must be an integer vector or a character vector")
  }
  
  return(x[[rec]])
}


#' @rdname sb2_rec
#' @export
sb2_recin <- function(x, ...) {
  
  if(is.atomic(x)) {
    stop("Use the `sb_` methods for atomic objects")
  }
  if(!is.list(x)) {
    stop("unsupported object")
  }
  
  UseMethod("sb2_recin", x)
}



#' @rdname sb2_rec
#' @export
sb2_recin.default <- function(x, rec, ..., rp, tf) {
  
  # error handling:
  .internal_check_dots(list(...), sys.call())
  if(!is.numeric(rec) && !is.character(rec)) {
    stop("`rec` must be an integer vector or a character vector")
  }
  .internal_check_rptf(rp, tf, sys.call())
  
  # function:
  if(!missing(rp)) {
    eval.parent(substitute(x[[rec]] <- rp))
  }
  if(!missing(tf)) {
    eval.parent(substitute(x[[rec]] <- tf(x[[rec]])))
  }
  
  # end:
  return(invisible(NULL))
  
}
