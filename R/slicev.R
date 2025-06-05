#' Efficient Value-based Subset Methods on (Long) Vectors
#' 
#' @description
#' The `slicev_` - methods are similar to the `i_`/`ss_` - methods,
#' except they don't require an indexing vector,
#' and are designed for memory efficiency. \cr
#' \cr
#' `counv(y, v, from, to)` counts how often a value, or range of values, `v`,
#' occurs in a vector subset `y[from:to]`. \cr \cr
#' 
#' @param x an atomic vector. \cr
#' For `slicev_set()` it must be a \link[mutatomic]{mutatomic} \bold{variable}.
#' @param ... See \link{squarebrackets_slicev}.
#' @param y,v,na,r See \link{squarebrackets_slicev}.
#' @param from,to see \link{cp_seq}.
#' @param use.names Boolean, indicating if flat names should be preserved. \cr
#' Note that, since the `slicev_` methods operates on flat indices only,
#' dimensions and `dimnames` are always dropped.
#' @param rp,tf see \link{squarebrackets_modify}.
#' @param sticky see \link{squarebrackets_options}.
#' 
#' 
#' 
#'
#' @returns
#' Similar to the `i_`/`ss_` methods. \cr
#' \cr
#' For `countv()`: A single number,
#' giving the number of elements matching the specified condition. \cr \cr
#' 
#' @example inst/examples/slicev.R
#' 
#' 


#' @name slicev
NULL


#' @rdname slicev
#' @export
slicev_x <- function(x, ...) {
  
  UseMethod("slicev_x", x)
}



#' @rdname slicev
#' @export
slicev_x.default <- function(
    x, ...,
    y = x, v = NULL, na = FALSE, r = TRUE, from = NULL, to = NULL,
    use.names = TRUE, sticky = getOption("squarebrackets.sticky", FALSE)
) {
  
  # general checks:
  .internal_check_dots(list(...), sys.call())
  .checkv_general(y, v, na, r, sys.call())
  
  # construct value parameters (if not missing):
  if(!is.na(na)) {
    myval <- .cp_val(y, v, r, sys.call())
    v <- myval$v
    r <- myval$r
  }
  
  # construct range parameters:
  myslice <- cp_seq(y, 0L, from = from, to = to)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  len <- myslice$length.out
  
  # run function:
  out <- .rcpp_slicev_x_atomic(
    x, y, v, na, !r, start - 1L, end - 1L, by, len
  )
  
  # attributes handling:
  if(!is.null(names(x)) && use.names && len != 0L) {
    nms <- .rcpp_slicev_x_atomic(
      names(x), y, v, na, !r, start - 1L, end - 1L, by, len
    )
    data.table::setattr(out, "names", nms)
  }
  if(is.factor(x)) {
    data.table::setattr(out, "contrasts", attr(x, "contrasts", exact = TRUE))
    data.table::setattr(out, "levels", attr(x, "levels", exact = TRUE))
    data.table::setattr(out, "class", oldClass(x))
  }
  if(is.mutatomic(x)) {
    .internal_set_ma(out)
  }
  if(is.logical(sticky) && length(sticky) == 1L) {
    if(sticky) {
      .internal_set_stickyattr(out, x)
    }
  }
  if(is.character(sticky) && inherits(x, sticky, which = FALSE)) {
    .internal_set_stickyattr(out, x)
  }
  
  
  return(out)
}


#' @rdname slicev
#' @export
slicev_set <- function(x, ...) {
  
  UseMethod("slicev_set", x)
}


#' @rdname slicev
#' @export
slicev_set.default <- function(
    x, ...,
    y = x, v = NULL, na = FALSE, r = TRUE, from = NULL, to = NULL,
    rp, tf
) {
  
  # error checks:
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  .internal_check_rptf(rp, tf, sys.call())

  # general checks:
  .internal_check_dots(list(...), sys.call())
  .checkv_general(y, v, na, r, sys.call())
  
  # construct value parameters (if not missing):
  if(!is.na(na)) {
    myval <- .cp_val(y, v, r, sys.call())
    v <- myval$v
    r <- myval$r
  }
  
  # construct range parameters:
  myslice <- cp_seq(y, 0L, from = from, to = to)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  len <- myslice$length.out
  
  # replacement checks:
  if(!missing(rp)) {
    if(length(rp) != 1L || !is.atomic(rp)) {
      stop("`rp` must be an atomic scalar")
    }
    value <- rp
  }
  
  # transformation checks:
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
    check <- .rcpp_slicev_x_atomic(
      x, y, v, na, !r, start - 1L, end - 1L, by, len
    )
    value <- tf(check)
    if(length(value) != length(check) && length(value) != 1L) {
      stop("recycling not allowed")
    }
  }
  
  # general value check:
  value <- .internal_coerce_rp(x, value, sys.call())
  
  # run function:
  .rcpp_slicev_set_atomic(x, y, v, na, !r, start-1, end-1, by, len, value)
  return(invisible(NULL))
}



#' @rdname slicev
#' @export
countv <- function(y, ..., v = NULL, na = FALSE, r = TRUE, from = NULL, to = NULL) {
  
  # general checks:
  .internal_check_dots(list(...), sys.call())
  .checkv_general(y, v, na, r, sys.call())
  
  # construct value parameters (if not missing):
  if(!is.na(na)) {
    myval <- .cp_val(y, v, r, sys.call())
    v <- myval$v
    r <- myval$r
  }
  
  # construct range parameters:
  myslice <- cp_seq(y, 0L, from = from, to = to)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  len <- myslice$length
  
  # run function:
  out <- .rcpp_countv(y, v, na, !r, start - 1L, end - 1L, by, len)
  
  return(out)
}


#' @keywords internal
#' @noRd
.checkv_general <- function(y, v, na, r, abortcall) {
  if(!is.atomic(y)) {
    stop(simpleError("`y` must be atomic", call = abortcall))
  }
  if(length(y) == 0L) {
    stop(simpleError("zero-length `y` not allowed", call = abortcall))
  }
  if(anyNA(r)) {
    stop(simpleError("`r` must not contain `NA`", call = abortcall))
  }
  if(!is.logical(r)) {
    stop(simpleError("`r` must be logical", call = abortcall))
  }
  if(length(r) > 2L) {
    stop(simpleError("`length(r) > 2L`"))
  }
  if(length(na) != 1 || !is.logical(na)) {
    stop(simpleError("`na` must be `TRUE`, `FALSE`, or `NA`", call = abortcall))
  }
  
  if(is.na(na)) {
    if(length(r) > 1L) {
      stop(simpleError("if `na = NA`, `r` must be Boolean", call = abortcall))
    }
  }
  if(is.na(na)) {
    if(!is.null(v)) {
      if(!is.na(v)) {
        message(simpleMessage("`na = NA`, so argument `v` will be ignored", call = abortcall))
      }
    }
  }
  if(is.factor(y)) {
    if(anyNA(levels(y))) {
      stop(simpleError("`NA` factor-levels not supported"))
    }
  }
  
}


#' @keywords internal
#' @noRd
.cp_val <- function(y, v, r, abortcall) {
  
  # general checks:
  if(!is.atomic(v)) {
    stop(simpleError("`v` must be atomic", call = abortcall))
  }
  if(length(v) == 0L) {
    stop(simpleError("zero-length `v` not allowed", call = abortcall))
  }
  if(anyNA(v)) {
    stop(simpleError("`v` must not contain NA/NaN; please use `na = NA` to find NA/NaN values", call = abortcall))
  }
  
  # type-dependent checks:
  if(length(r) > 1L) {
    stop(simpleError("`r` must be Boolean"))
  }
  
  if(is.factor(y)) {
    v <- .checkv_lvl2int(v, y, abortcall)
  }
  else if(is.numeric(y) && !is.complex(y)) {
    if(!is.numeric(v)) {
      stop(simpleError("if `y` is numeric, `v` must also be numeric", call = abortcall))
    }
    if(length(v) > 2L) {
      stop(simpleError("if `y` is numeric `v` must be of length 1 or 2", call = abortcall))
    }
    if(length(v) == 2L) {
      if(v[1] > v[2]) {
        stop(simpleError("problem in `v`: lower bound larger than upper bound", call = abortcall))
      }
    }
    v <- as.double(v)
  }
  else if(is.character(y)) {
    if(typeof(v) != typeof(y)) {
      stop(simpleError("`typeof(v)` not compatible with `typeof(y)`", call = abortcall))
    }
    if(!all(nzchar(v))) {
      stop(simpleError("`v` cannot contain zero-length strings", call = abortcall))
    }
  }
  else {
    if(typeof(v) != typeof(y)) {
      stop(simpleError("`typeof(v)` not compatible with `typeof(y)`", call = abortcall))
    }
    if(length(v) > 1L) {
      stop(simpleError("non-scalar `v` not supported for this data type", call = abortcall))
    }
  }
  
  out <- list(
    v = v, r = r
  )
  return(out)
  
}


#' @keywords internal
#' @noRd
.checkv_lvl2int <- function(v, y, abortcall) {
  
  if(length(v) > 1L) {
    stop(simpleError("non-scalar `v` not supported for this data type", call = abortcall))
  }
  if(is.character(v)) {
    v <- factor(v, levels = levels(y), ordered = is.ordered(y), exclude = NULL)
    v <- collapse::unattrib(v)
    return(as.integer(v))
  }
  
  if(is.factor(v)) {
    if(any(levels(v) != levels(y))) {
      stop(simpleError("`v` must have same levels as `y`", call = abortcall))
    }
    if(is.ordered(v) != is.ordered(y)) {
      stop(simpleError("`v` must have the same ordering as `y`", call = abortcall))
    }
    v <- collapse::unattrib(v)
    return(v)
  }
  if(is.numeric(v)) {
    nlevels <- nlevels(y)
    if(any(!v %in% 1:nlevels)) {
      stop(simpleError("invalid factor level", call = abortcall))
    }
    return(as.integer(v))
  }
  else {
    stop(simpleError("improper type of `v`", call = abortcall))
  }
}



