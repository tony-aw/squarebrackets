#' Dimensional Binding of Objects
#'
#' @description
#' The `bind_` and `bind2_` implementations provide dimensional binding functionalities. \cr
#' `bind_` is for atomic objects, and `bind2_` for recursive objects. \cr
#' When possible, the `bind_`/`bind2_` functions return \link[=squarebrackets_mutable_classes]{mutable classes}. \cr
#' \cr
#' The following implementations are available:
#' 
#'  - `bind_array()` binds atomic arrays and matrices. \cr
#'  Returns a \link{mutable_atomic} array. \cr
#'  - `bind2_array()` binds recursive arrays and matrices. \cr
#'  Returns a recursive array (immutable).
#'  - `bind2_dt()` binds data.tables and other data.frame-like objects. \cr
#'  Returns a `data.table`. \cr
#'  Faster than `do.call(cbind, ...)` or `do.call(rbind, ...)` for regular `data.frame` objects. \cr
#' 
#' 
#' 
#' @param arg.list a list of only the appropriate objects. \cr
#' Do not mix recursive and atomic objects in the same list,
#' as that may result in unexpected results. \cr
#' @param along a single integer,
#' indicating the dimension along which to bind the dimensions. \cr
#' I.e. use `along = 1` for row-binding, `along = 2` for column-binding, etc. \cr
#' For arrays, additional flexibility is available:
#'  * Specifying `along = 0` will bind the arrays on a new dimension before the first,
#'  making `along` the new first dimension.
#'  * Specifying `along = n+1`, with `n` being the last available dimension,
#'  will create an additional dimension (`n+1`) and bind the arrays along that new dimension.
#' @param name_along Boolean, for `bind_array()` and `bind2_array()`. \cr
#' Indicates if dimension `along` should be named. \cr
#' @param name_shared integer or `NULL`, for `bind_array()` and `bind2_array()`. \cr
#' Indicates which object in `arg.list` should be used for naming the shared dimension. \cr
#' If `NULL`, no shared names will be given. \cr
#' For example: \cr
#' When binding columns of atomic matrices,
#' `name_shared = 1` results in `bind_array()` using `rownames(arg.list[[1]])` for the row names of the output.
#' @param name_flat Boolean, for `bind_array()` and `bind2_array()`. \cr
#' Indicates if flat indices should be named. \cr
#' Note that setting this to `TRUE` will reduce performance considerably. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr \cr
#' 
#' @details
#' `bind_array()` and `bind2_array()` are modified versions of the fantastic
#' \code{abind::}\link[abind]{abind} function
#' by Tony Plare and Richard Heiberger (see reference below). \cr
#' `bind_array()` has slightly better performance than \code{abind::}\link[abind]{abind},
#' and has more streamlined naming options. \cr
#' `bind2_array()` also has the streamlined naming options,
#' and additionally differs from \code{abind::}\link[abind]{abind}
#' in that it can handle recursive arrays properly
#' (the original \code{abind::}\link[abind]{abind} function would unlist everything to atomic arrays). \cr \cr
#' 
#' 
#' @returns
#' The new object.
#'
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#'
#' @example inst/examples/generic_bind.R
#' 
#'  


#' @name bind
NULL


#' @rdname bind
#' @export
bind_array <- function(
    arg.list, along, name_along = TRUE, name_shared = 1L, name_flat = FALSE
) {
  out <- .internal_abind(arg.list, along, TRUE, name_along)
  
  
  if(!is.null(name_shared)) {
    .bind_set_sharednames(out, name_shared, arg.list, along)
  }
  if(name_flat) {
    names(out) <- .bind_make_flatnames(arg.list, along)
  }
  
  if(couldb.mutable_atomic(out)) {
    # Note: no need to copy names, as .internal_abind already does that
    .internal_set_ma(out)
  }
  return(out)
}


#' @rdname bind
#' @export
bind2_array <- function(
    arg.list, along, name_along = TRUE, name_shared = 1L, name_flat = FALSE
) {
  out <- .internal_abind(arg.list, along, FALSE, name_along)
  
  if(!is.null(name_shared)) {
    .bind_set_sharednames(out, name_shared, arg.list, along)
  }
  if(name_flat) {
    names(out) <- .bind_make_flatnames(arg.list, along)
  }
  return(out)
}


#' @rdname bind
#' @export
bind2_dt <- function(
    arg.list, along
) {
  if(along == 1) {
    return(data.table::rbindlist(arg.list))
  }
  if(along == 2) {
    return(do.call(data.table::data.table, arg.list))
  }
}

