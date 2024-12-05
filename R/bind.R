#' Dimensional Binding of Objects
#'
#' @description
#' The `bind_`implementations provide dimensional binding functionalities. \cr
#' When possible, the `bind_` functions return \link[=squarebrackets_supported_structures]{mutable classes}. \cr
#' \cr
#' The following implementations are available:
#' 
#'  - `bind_mat()` binds dimensionless (atomic/recursive) vectors and (atomic/recursive) matrices row- or column-wise. \cr
#'  If the result is atomic, returns a \link{mutable_atomic} matrix; otherwise returns a recursive matrix. \cr
#'  - `bind_array()` binds (atomic/recursive) arrays and (atomic/recursive) matrices. \cr
#'  If the result is atomic, returns a \link{mutable_atomic} array; otherwise returns a recursive array. \cr
#'  - `bind_dt()` binds data.tables and other data.frame-like objects. \cr
#'  Returns a `data.table`. \cr
#'  Faster than `do.call(cbind, ...)` or `do.call(rbind, ...)` for regular `data.frame` objects. \cr
#' 
#' 
#' Note that the naming convention of the binding implementations here is
#' "bind_" followed by the \bold{resulting class} (abbreviated). \cr
#' I.e. `bind_mat` \bold{returns} a matrix, but can bind both matrices and vectors. \cr
#' And `bind_array` \bold{returns} an array, but can bind both arrays and matrices. \cr
#' And `bind_dt` \bold{returns} a data.table, but can bind not only data.tables,
#' but also most other data.frame-like objects. \cr \cr
#' 
#' 
#' 
#' @param arg.list a list of only the appropriate objects. \cr
#' If `arg.list` is named,
#' its names will be used for the names of dimension `along` of the output,
#' as far as possible.
#' @param along a single integer,
#' indicating the dimension along which to bind the dimensions. \cr
#' I.e. use `along = 1` for row-binding, `along = 2` for column-binding, etc. \cr
#' For arrays, additional flexibility is available:
#'  * Specifying `along = 0` will bind the arrays on a new dimension before the first,
#'  making `along` the new first dimension.
#'  * Specifying `along = n+1`, with `n` being the last available dimension,
#'  will create an additional dimension (`n+1`) and bind the arrays along that new dimension.
#' @param name_deparse Boolean, for `bind_mat()`. \cr
#' Indicates if dimension `along` should be named. \cr
#' Uses the naming method from \link[base]{rbind}/\link[base]{cbind} itself.
#' @param name_along Boolean, for `bind_array()`. \cr
#' Indicates if dimension `along` should be named.
#' @param comnames_from either integer scalar or `NULL`,
#' for `bind_mat()` and  `bind_array()`. \cr
#' Indicates which object in `arg.list` should be used for naming the shared dimension. \cr
#' If `NULL`, no communal names will be given. \cr
#' For example: \cr
#' When binding columns of matrices, the matrices will share the same rownames. \cr
#' Using `comnames_from = 10` will then result in `bind_array()` using
#' `rownames(arg.list[[10]])` for the rownames of the output.
#' @param name_flat Boolean, for `bind_array()`. \cr
#' Indicates if flat indices should be named. \cr
#' Note that setting this to `TRUE` will reduce performance considerably. \cr
#' `r .mybadge_performance_set2("FALSE")`
#' @param ... arguments to be passed to \link[data.table]{rbindlist}. \cr \cr
#' 
#'  
#' 
#' @details
#' `bind_array()` is a modified version of the fantastic
#' \code{abind::}\link[abind]{abind} function
#' by Tony Plare & Richard Heiberger (2016),
#' in the following ways:
#'  
#'  - `bind_array()` primarily differs from \code{abind::}\link[abind]{abind}
#'  in that it can handle recursive arrays properly \cr
#'  (the original \code{abind::}\link[abind]{abind} function would unlist everything to atomic arrays,
#'  ruining the structure). 
#'  - unlike \code{abind::}\link[abind]{abind},
#'  `bind_array()` only binds (atomic/recursive) arrays and matrices. \cr
#'  `bind_array()`does not attempt to convert things to arrays when they are not arrays,
#'  but will give an error instead. \cr
#'  This saves computation time and prevents unexpected results.
#'  - if `bind_array()` results in an atomic array, it will be a \link{mutable_atomic} array.
#'  - `bind_array()` has more streamlined naming options. \cr \cr
#'  
#'  
#' `bind_mat()` is a modified version of \link[base]{rbind}/\link[base]{cbind}. \cr
#' The primary differences is that `bind_mat()` gives an error when fractional recycling is attempted
#' (like binding  `1:3` with `1:10`). \cr \cr
#' 
#' 
#' @returns
#' The bound object.
#'
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#'
#' @example inst/examples/bind.R
#' 
#'  


#' @name bind
NULL



#' @rdname bind
#' @export
bind_mat <- function(
    arg.list, along, name_deparse = TRUE, comnames_from = 1L
) {
  
  # error checks:
  .bind_checkargs(along, name_deparse, comnames_from, FALSE, abortcall = sys.call())
  if(any(vapply(arg.list, is.data.frame, logical(1L)))) {
    stop("use `bind_dt to bind data.frame-like objects")
  }
  
  if(along == 1L) imargin <- 2L
  else if(along == 2L) imargin <- 1L
  else {
    stop("`along` must be 1 or 2")
  }
  
  sizes <- .rcpp_rcbind_get_sizes(arg.list, imargin - 1L)
  sizes <- sizes[sizes != 1L]
  if(length(sizes) > 1) {
    fractions <- sizes / min(sizes)
    if(any(fractions != round(fractions))) {
      stop("fractional recycling not allowed")
    }
  }
  
  
  name_deparse <- as.integer(name_deparse)
  if(along == 1L) {
    out <- do.call(rbind, c(arg.list, n(deparse.level = name_deparse)))
    not_along <- 2L
  }
  if(along == 2L) {
    out <- do.call(cbind, c(arg.list, n(deparse.level = name_deparse)))
    not_along <- 1L
  }
  
  
  if(is.null(comnames_from)) {
    dimnames(out)[[not_along]] <- NULL
  }
  if(!is.null(comnames_from)) {
    comarg <- arg.list[[comnames_from]]
    if(is.array(comarg) && !is.null(dimnames(comarg))) {
      dimnames(out)[[not_along]] <- dimnames(comarg)[[not_along]]
    }
    else {
      if(!is.null(names(comarg))) {
        dimnames(out)[[not_along]] <- names(comarg)
      }
    }
  }
  
  if(is.atomic(out)) {
    .internal_set_ma(out)
  }
  return(out)
}


#' @rdname bind
#' @export
bind_array <- function(
    arg.list, along, name_along = TRUE, comnames_from = 1L, name_flat = FALSE
) {
  
  .bind_checkargs(along, name_along, comnames_from, name_flat, abortcall = sys.call())
  
  out <- .internal_abind(arg.list, along, name_along, sys.call())
  
  
  if(!is.null(comnames_from)) {
    .bind_set_sharednames(out, comnames_from, arg.list, along)
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
bind_dt <- function(
    arg.list, along, ...
) {
  if(along == 1) {
    out <- data.table::rbindlist(arg.list, ...)
  }
  if(along == 2) {
    out <- do.call(data.table::data.table, c(arg.list, check.names = TRUE))
  }
  return(out)
  
}
