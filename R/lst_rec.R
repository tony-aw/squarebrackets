#' Access, Replace, Transform, Delete, or Extend Recursive Subsets
#'
#' @description
#' The `lst_rec()` and `lst_recin()` methods
#' are essentially convenient wrappers around `[[` and `[[<-`,
#' respectively. \cr
#' \cr
#' `lst_rec()` will access recursive subsets of lists. \cr
#' \cr
#' `lst_recin()` can do the following things: \cr
#' 
#'  - replace or transform recursive subsets of a list,
#'  using R's default Copy-On-Modify semantics,
#'  by specifying the `rp` or `tf` argument, respectively.
#'  - delete a recursive subset of a list,
#'  using R's default Copy-On-Modify semantics,
#'  by specifying argument `rp = NULL`.
#'  - extending a list with additional recursive elements,
#'  using R's default Copy-On-Modify semantics. \cr
#'  This is done by specifying an out-of-bounds index in argument `rec`,
#'  and entering the new values in argument `rp`. \cr
#'  Note that adding surface level elements of a dimensional list
#'  will delete the dimension attributes of that list. \cr \cr
#' 
#' 
#' 
#' @param x a list, or list-like object.
#' @param rec a strictly positive integer vector or character vector, of length `p`,
#' such that `lst_rec(x, rec)` is equivalent to `x[[ rec[1] ]]...[[ rec[p] ]]`,
#' providing all but the final indexing results in a list. \cr
#' When on a certain subset level of a nested list,
#' multiple subsets with the same name exist,
#' only the first one will be selected when performing recursive indexing by name,
#' since recursive indexing can only select a single element. \cr
#' `NA, NaN, Inf, -Inf` are not valid values for `rec`.
#' @param ... see \link{squarebrackets_method_dispatch}.
#' @param rp optional, and allows for multiple functionalities:
#'  - In the simplest case, performs `x[[rec]] <- rp`,
#' using R's default semantics. \cr
#' Since this is a replacement of a recursive subset,
#' `rp` does not necessarily have to be a list itself; \cr
#' `rp` can be any type of object.
#' - Specifying `rp = NULL` will \bold{delete} (recursive) subset `lst_rec(x, rec)`. \cr
#' To specify actual `NULL` instead of deleting a subset, use `rp = list(NULL)`.
#' - When `rec` is an integer, and specifies an out-of-bounds subset,
#' `lst_recin()` will add value `rp` to the list. \cr
#' Any empty positions in between will be filled with `NA`.
#' - When `rec` is character, and specifies a non-existing name,
#' `lst_recin()` will add value `rp` to the list as a new element at the end.
#' @param tf an optional function. If specified, performs `x[[rec]] <- tf(x[[rec]])`,
#' using R's default Copy-On-Modify semantics. \cr
#' Does not support extending a list like argument `rp`.
#' 
#' 
#' @details
#' Since recursive objects are references to other objects,
#' extending a list or deleting an element of a list does not copy the entire list,
#' in contrast to atomic vectors. \cr \cr
#' 
#'
#' @returns
#' For `lst_rec()`: \cr
#' Returns the recursive subset. \cr
#' \cr
#' For `lst_recin(..., rp = rp)`: \cr
#' Returns VOID,
#' but replaces, adds, or deletes the specified recursive subset,
#' using R's default Copy-On-Modify semantics. \cr
#' \cr
#' For `lst_recin(..., tf = tf)`: \cr
#' Returns VOID,
#' but transforms the specified recursive subset,
#' using R's default Copy-On-Modify semantics. \cr \cr
#' 
#'
#'
#'
#' @example inst/examples/lst_rec.R
#'



#' @rdname lst_rec
#' @export
lst_rec <- function(x, ...) {
  
  if(!is.list(x)) {
    stop("The `lst_` methods only support (nested) lists")
  }
  
  UseMethod("lst_rec", x)
}


#' @rdname lst_rec
#' @export
lst_rec.default <- function(x, rec, ...) {
  
  # error handling:
  .internal_check_dots(list(...), sys.call())
  if(anyNA(rec)) {
    stop("`rec` cannot contain `NA`")
  }
  if(!is.numeric(rec) && !is.character(rec)) {
    stop("`rec` must be an integer vector or a character vector")
  }
  
  return(x[[rec]])
}


#' @rdname lst_rec
#' @export
lst_recin <- function(x, ...) {
  
  if(!is.list(x)) {
    stop("The `lst_` methods only support (nested) lists")
  }
  
  UseMethod("lst_recin", x)
}



#' @rdname lst_rec
#' @export
lst_recin.default <- function(x, rec, ..., rp, tf) {
  
  # error handling:
  .internal_check_dots(list(...), sys.call())
  if(anyNA(rec)) {
    stop("`rec` cannot contain `NA`")
  }
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

