
#' @rdname stride_pv
#' @export
stride_pv <- function(p, v = NULL, na = FALSE) {
  
  # check args:
  .stride_pv_checkargs(p, v, na, sys.call())
  
  # MAIN FUNCTION:
  if(!is.logical(na) || length(na) != 1L) {
    stop("`na` must be a logical scalar")
  }
  
  if(!is.na(na)) {
    if(is.numeric(p) && !is.complex(p)) {
      if(!is.numeric(v)) {
        stop("if `p` is numeric, `v` must also be numeric")
      }
      if(length(v) > 2L) {
        stop("if `p` is numeric `v` must be of length 1 or 2")
      }
      if(length(v) == 2L) {
        if(v[1] > v[2]) {
          stop("problem in `v`: lower bound larger than upper bound")
        }
      }
      v <- as.double(v)
    }
    else if(is.character(p)) {
      if(typeof(v) != typeof(p)) {
        stop("`typeof(v)` not compatible with `typeof(p)`")
      }
      if(!all(nzchar(v))) {
        stop("`v` cannot contain zero-length strings")
      }
    }
    else {
      if(typeof(v) != typeof(p)) {
        stop("`typeof(v)` not compatible with `typeof(p)`")
      }
      if(length(v) > 1L) {
        stop("non-scalar `v` not supported for this data type")
      }
    }
  }
  
  
  out <- pairlist(
    p = p,
    v = v,
    na = na
  )
  class(out) <- c("stride_pv", "stride")
  
  return(out)
  
}

#' @rdname stride_pv
#' @export
countv <- function(p, v = NULL, na = FALSE, use = 1) {
  
  stride <- stride_pv(p, v, na)
  
  # run function:
  out <- .rcpp_countv(stride$p, stride$v, stride$na, use)
  
  return(out)
}


#' @keywords internal
#' @noRd
.stride_pv_checkargs <- function(p, v, na, abortcall) {
  
  if(!couldb.mutatomic(p)) {
    stop(simpleError("couldb.mutatomic(p) is not TRUE"))
  }
  stopifnot(couldb.mutatomic(p))
  
  if(length(p) == 0L) {
    stop(simpleError("zero-length `p` not allowed", call = abortcall))
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
