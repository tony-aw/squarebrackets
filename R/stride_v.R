

#' @rdname stride_v
#' @export
stride_v <- function(y, ...) {
  UseMethod("stride_v", y)
}

#' @rdname stride_v
#' @export
stride_v.default <- function(y, ..., v = NULL, na = FALSE, use = 1) {
  
  ellipsis <- list(...)
  if(length(ellipsis)) {
    stop("unknown arguments given")
  }
  
  mycall <- sys.call()
  
  # check args:
  .stride_v_checkargs(y, v, na, use, mycall)
  
  # MAIN FUNCTION:
  return(.stride_v_main(y, v, na, use, mycall))
}



#' @keywords internal
#' @noRd
.stride_v_main <- function(y, v, na, use, abortcall) {
  
  if(!is.na(na)) {
    if(is.numeric(y) && !is.complex(y)) {
      if(!is.numeric(v)) {
        stop("if `y` is numeric, `v` must also be numeric")
      }
      if(length(v) > 2L) {
        stop("if `y` is numeric `v` must be of length 1 or 2")
      }
      if(length(v) == 2L) {
        if(v[1] > v[2]) {
          stop("problem in `v`: lower bound larger than upper bound")
        }
      }
      v <- as.double(v)
    }
    else if(is.character(y)) {
      if(typeof(v) != typeof(y)) {
        stop("`typeof(v)` not compatible with `typeof(y)`")
      }
      if(!all(nzchar(v))) {
        stop("`v` cannot contain zero-length strings")
      }
    }
    else {
      if(typeof(v) != typeof(y)) {
        stop("`typeof(v)` not compatible with `typeof(y)`")
      }
      if(length(v) > 1L) {
        stop("non-scalar `v` not supported for this data type")
      }
    }
  }
  
  cond <- use > 0L
  chunks <- .rcpp_stridev_chunks(y)
  preplist <- .rcpp_stridev_preplist(y, v, chunks, cond, na)
  prepvector <- .rcpp_stridev_prepvector(y, preplist)
  pool <- .rcpp_stridev_pool(y, v,  preplist, prepvector, cond, na)
  
  out <- pairlist(
    y = y,
    chunks = chunks,
    preplist = preplist,
    prepvector = prepvector,
    pool = pool,
    use = use
  )
  class(out) <- c("stride_v", "stride")
  
  return(out)
}


#' @keywords internal
#' @noRd
.stride_v_checkargs <- function(y, v, na, use, abortcall) {
  
  
  # check use:
  if(!.stride_use_OK(use)) {
    stop(simpleError("improper `use` given", call = abortcall))
  }
  
  # check y:
  stopifnot(couldb.mutatomic(y))
  if(length(y) == 0L) {
    stop(simpleError("zero-length `y` not allowed", call = abortcall))
  }
  
  # check na & v:
  if(!is.logical(na) || length(na) != 1L) {
    stop("`na` must be a logical scalar")
  }
  if(length(na) != 1 || !is.logical(na)) {
    stop(simpleError("`na` must be `TRUE`, `FALSE`, or `NA`", call = abortcall))
  }
  if(is.null(v) && !is.na(na)) {
    stop(simpleError("if `na` is not `NA`, `v` must be specified", call = abortcall))
  }
  if(is.na(na)) {
    if(!is.null(v)) {
      if(!collapse::allNA(v)) {
        warning(simpleMessage("`na = NA`, so argument `v` will be ignored", call = abortcall))
      }
    }
  }
  if(!is.na(na)) {
    if(!is.atomic(v)) {
      stop(simpleError("`v` must be atomic", call = abortcall))
    }
    if(length(v) == 0L) {
      stop(simpleError("zero-length `v` not allowed", call = abortcall))
    }
    if(anyNA(v)) {
      txt <- "`v` must not contain NA/NaN; please use `na = NA` to find NA/NaN values"
      stop(simpleError(txt, call = abortcall))
    }
  }
  
}



