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
#'  This is a modified version of the fantastic \code{abind::}\link[abind]{abind} function by Tony Plare and Richard Heiberger,
#'  such that it can handle recursive arrays
#'  (\code{abind::}\link[abind]{abind} would unlist everything to atomic). \cr
#'  Returns dimensional lists.
#'  - `bind2_dt()` binds data.tables and other data.frame-like objects. \cr
#'  Returns a `data.table`. \cr
#'  Faster than `do.call(cbind, ...)` or `do.call(rbind, ...)` for regular `data.frame` objects. \cr
#' 
#' 
#' 
#' @param arg.list a list of only the appropriate objects.
#' Do not mix different types of objects. \cr
#' @param along a single integer,
#' indicating the dimension along which to bind the dimensions. \cr
#' I.e. use `along = 1` for row-binding, `along = 2` for column-binding, etc.
#' @param name_along Boolean, for `bind_array()` and `bind2_array()`. \cr
#' Indicates if dimension `along` should be named. \cr
#' Other dimensions will never be named.
#' @param name_flat Boolean, for `bind_array()` and `bind2_array()`. \cr
#' Indicates if flat indices should be named. \cr
#' Note that setting this to `TRUE` will reduce performance considerably. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr \cr
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
    arg.list, along, name_along = TRUE, name_flat = FALSE
) {
  out <- .internal_abind(arg.list, along, TRUE, name_along)
  if(name_flat) {
    names(out) <- .bind_make_flatnames(arg.list, along)
  }
  if(couldb.mutable_atomic(out)) {
    # Note: no need to copy names, as .internal_abind already does that
    attr(out, "typeof") <- typeof(out)
    class(out) <- c("mutable_atomic", class(out))
  }
  return(out)
}

#' @rdname bind
#' @export
bind2_array <- function(
    arg.list, along, name_along = TRUE, name_flat = FALSE
) {
  out <- .internal_abind(arg.list, along, FALSE, name_along)
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

