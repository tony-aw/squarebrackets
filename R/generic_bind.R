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
#' Indicates if flat indices should be named. \cr \cr
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
  out <- .internal_abind(arg.list, along, TRUE, name_along, name_flat)
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
  return(.internal_abind(arg.list, along, FALSE, name_along, name_flat))
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


#' @keywords internal
#' @noRd
.bind_set_dimnames <- function(
    out, along, arg.list, arg.dimnames, arg.marginlen
) {
  name_along <- vector(mode = "character", length = dim(out)[along])
  arg.names <- names(arg.list)
  start.pos <- 0L
  for(i in seq_along(arg.list)) {
    marginlen <- arg.marginlen[i]
    indx <- seq_len(marginlen) + start.pos
    temp.dimnames <- .bind_getnames(arg.dimnames[[i]], arg.names[i], marginlen)
    collapse::setv(
      name_along, indx, temp.dimnames, vind1 = TRUE, xlist = FALSE
    )
    start.pos <- start.pos + marginlen
  }
  dimnames <- rep(list(NULL), length(dim(out)))
  dimnames[[along]] <- name_along
  data.table::setattr(out, "dimnames", dimnames)
}


#' @keywords internal
#' @noRd
.bind_make_flatnames <- function(
    out, arg.list, arg.flatnames, arg.lens
) {
  name_flat <- vector(mode = "character", length = length(out))
  arg.names <- names(arg.list)
  start.pos <- 0L
  for(i in seq_along(arg.list)) {
    len <- arg.lens[i]
    indx <- seq_len(len) + start.pos
    temp.names <- .bind_getnames(arg.flatnames[[i]], arg.names[i], len)
    collapse::setv(
      name_flat, indx, temp.names, vind1 = TRUE, xlist = FALSE
    )
    start.pos <- start.pos + len
  }
  return(name_flat)
}


#' @keywords internal
#' @noRd
.bind_getnames <- function(main.names, arg.name, size) {
  if(!is.null(main.names)) {
    temp.names <- main.names
  }
  else if(!is.null(arg.name)) {
    temp.names <- stringi::stri_c(arg.name, ".", seq_len(size))
  }
  else {
    temp.names <- stringi::stri_c("X", seq_len(size))
  }
  return(temp.names)
}

