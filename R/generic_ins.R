#' Methods to Insert New Values Before or After an Index Along a Dimension
#'
#' @description
#' The `sb_before()` method
#' inserts new values before some position along a dimension. \cr
#' The `sb_after()` method
#' inserts new values after some position along a dimension. \cr
#' \cr
#' These functions use a modified version of \code{abind::}\link[abind]{abind}
#' (see reference below).
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param new the new value(s). The type of object depends on `x`:
#'  * For atomic objects, `new` can be any atomic object.
#'  However, if one wished the added values in `new` to be named,
#'  ensure `new` is the same type of object as `x`.
#'  For example: use matrix with column names for `new` when appending/inserting columns to matrix `x`.
#'  * For factors, `new` must be a factor.
#'  * For lists, `new` must be a (possible named) list.
#'  * For data.frame-like objects, `new` must be a data.frame.
#' @param pos a strictly positive single integer scalar (so no duplicates),
#' giving the position along the dimension (specified in `margin`),
#' before or after which the new values are added.
#' @param margin a single scalar, giving the dimension along which to add new values.
#' @param .attr a list,
#' giving additional potentially missing attributes to be added to the returned object. \cr
#' By default, concatenation strips attributes,
#' since the attributes of `x` and `new` may not be equal or even compatible. \cr
#' In the `attr` argument, the attributes of the merged object can be specified. \cr
#' Only attributes that are actually missing AFTER insertion will be added,
#' thus preventing overwriting existing attributes like names. \cr
#' One may, for example, specify `.attr = sb_a(x)` or `.attr = sb_a(new)`. \cr
#' If `NULL` (the default), no attributes will be added post-insert. \cr
#' If speed is important, `NULL` is the best option (but then attributes won't be preserved). \cr
#' @param ... further arguments passed to or from other methods.
#'
#' 
#'
#' @returns
#' Returns a copy of the appended object.
#'
#'
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#'
#'
#' @example inst/examples/generic_ins.R
#' 

#' @rdname sb_in
#' @export
sb_before <- function(x, ...) {
  UseMethod("sb_before", x)
}

#' @rdname sb_in
#' @export
sb_after <- function(x, ...) {
  UseMethod("sb_after", x)
}

#' @rdname sb_in
#' @export
sb_before.default <- function(x, new, pos = 1, .attr = NULL, ...) {
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_prepend(pos, n)) {
    out <- c(new, x)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- c(x[seq_len(pos-1)], new, x[seq.int(pos, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}

#' @rdname sb_in
#' @export
sb_after.default <- function(x, new, pos = length(x), .attr = NULL, ...) {
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_postpend(pos, n)) {
    out <- c(x, new)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  out <- c(x[seq_len(pos)], new, x[seq.int(pos+1, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}


#' @rdname sb_in
#' @export
sb_before.array <- function(x, new, margin, pos = 1, .attr = NULL, ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  n <- dim(x)[[margin]]
  .check_in(pos, n, abortcall = sys.call())
  return(.sb_in_dim_before(x, margin, pos, new, .attr, abortcall = sys.call()))
}

#' @rdname sb_in
#' @export
sb_after.array <- function(x, new, margin, pos = dim(x)[margin], .attr = NULL, ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  
  n <- dim(x)[[margin]]
  .check_in(pos, n, abortcall = sys.call())
  return(.sb_in_dim_after(x, margin, pos, new, .attr, abortcall = sys.call()))
}


#' @rdname sb_in
#' @export
sb_before.factor <- function(x, new, pos = 1, .attr = NULL, ...) {
  
  if(!is.factor(new)) {
    stop("`new` must be a (possibly named) factor")
  }
  
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_prepend(pos, n)) {
    out <- c(new, x)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  out <- c(x[seq_len(pos-1)], new, x[seq.int(pos, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}

#' @rdname sb_in
#' @export
sb_after.factor <- function(x, new, pos = length(x), .attr = NULL, ...) {
  
  if(!is.factor(new)) {
    stop("`new` must be a (possibly named) factor")
  }
  
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_postpend(pos, n)) {
    out <- c(x, new)
    out <- .fix_attr(out, .attr)
    return(out)
  }

  out <- c(x[seq_len(pos)], new, x[seq.int(pos+1, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}


#' @rdname sb_in
#' @export
sb_before.list <- function(x, new, pos = 1, .attr = NULL, ...) {
  
  if(!is.list(new)) {
    stop("`new` must be a (possibly named) list")
  }
  
  n <- length(x)
  .check_in(pos,  n, abortcall = sys.call())
  
  if(.is_prepend(pos, n)) {
    out <- c(new, x)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- c(x[seq_len(pos-1)], new, x[seq.int(pos, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}



#' @rdname sb_in
#' @export
sb_after.list <- function(x, new, pos = length(x), .attr = NULL, ...) {
  
  if(!is.list(new)) {
    stop("`new` must be a (possibly named) list")
  }
  
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_postpend(pos, n)) {
    out <- c(x, new)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- c(x[seq_len(pos)], new, x[seq.int(pos+1, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}


#' @rdname sb_in
#' @export
sb_before.data.frame <- function(x, new, margin, pos = 1, .attr = NULL, ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  
  if(!is.data.frame(new)) {
    stop("`new` must be a data.frame-like object")
  }

  if(margin == 1) {
    abind <- rbind
    ss <- function(x, ind) collapse::ss(x, i = ind)
    n <- collapse::fnrow(x)
  }
  if(margin == 2){ 
    abind <- cbind
    ss <- function(x, ind) collapse::ss(x, j = ind)
    n <- collapse::fncol(x)
  }
  
  if(.is_prepend(pos, n)) {
    out <- abind(new, x)
    out <- .fix_attr(out, .attr)
    colnames(out) <- make.names(colnames(out), unique = TRUE)
    return(out)
  }
  out <- abind(
    ss(x, seq_len(pos-1)),
    new,
    ss(x, seq.int(pos, n))
  )
  out <- .fix_attr(out, .attr)
  colnames(out) <- make.names(colnames(out), unique = TRUE)
  return(out)
}

#' @rdname sb_in
#' @export
sb_after.data.frame <- function(x, new, margin, pos = collapse::fdim(x)[margin], .attr = NULL, ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  
  if(!is.data.frame(new)) {
    stop("`new` must be a data.frame-like object")
  }
  
  if(margin == 1) {
    abind <- rbind
    ss <- function(x, ind) collapse::ss(x, i = ind)
    n <- collapse::fnrow(x)
  }
  if(margin == 2){ 
    abind <- cbind
    ss <- function(x, ind) collapse::ss(x, j = ind)
    n <- collapse::fncol(x)
  }
  
  if(.is_postpend(pos, n)) {
    out <- abind(x, new)
    out <- .fix_attr(out, .attr)
    colnames(out) <- make.names(colnames(out), unique = TRUE)
    return(out)
  }
  out <- abind(
    ss(x, seq_len(pos)),
    new,
    ss(x, seq.int(pos+1, n))
  )
  out <- .fix_attr(out, .attr)
  colnames(out) <- make.names(colnames(out), unique = TRUE)
  return(out)
}

#' @keywords internal
#' @noRd
.check_in <- function(pos, n, abortcall) {
  error.txt2 <- simpleError("`pos` must be a strictly positive integer scalar", call = abortcall)
  
  if(!is.numeric(pos) || length(pos) != 1) {
    stop(error.txt2)
  }
  if(pos < 1) { # removed any(), as pos is scalar
    stop(error.txt2)
  }
  if(pos > n) { # removed any(), as pos is scalar
    stop(simpleError("subscript out of bounds", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.is_prepend <- function(pos, n) {
  if(pos == 1 || n == 1) { 
      return(TRUE)
  }
  return(FALSE)
}


#' @keywords internal
#' @noRd
.is_postpend <- function(pos, n) {
  if(pos == n || n == 1) {
      return(TRUE)
  }
  return(FALSE)
}



#' @keywords internal
#' @noRd
.sb_in_dim_before <- function(x, margin, pos, new, .attr, abortcall) {
  
  n.x <- dim(x)[[margin]]
  
  if(.is_prepend(pos, n.x)) {
    out <- .abind(new, x, ._along = margin)
    out <- .fix_attr(out, .attr)
    return(out)
  }

  out <- .abind(
    .asub(x, idx = seq_len(pos-1), dims = margin),
    new,
    .asub(x, idx = seq.int(pos, n.x), dims = margin),
    ._along = margin
  )
  out <- .fix_attr(out, .attr)
  return(out)

}

#' @keywords internal
#' @noRd
.sb_in_dim_after <- function(x, margin, pos, new, .attr, abortcall) {
  
  n.x <- dim(x)[[margin]]
  
  if(.is_postpend(pos, n.x)) {
    out <- .abind(x, new, ._along = margin)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- .abind(
    .asub(x, idx = seq_len(pos), dims = margin),
    new,
    .asub(x, idx = seq.int(pos+1, n.x), dims = margin),
    ._along = margin
  )
  out <- .fix_attr(out, .attr)
  return(out)

}
