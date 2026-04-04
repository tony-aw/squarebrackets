

#' @keywords internal
#' @noRd
.long_pv_x <- function(
    x, stride, use, use.names, sticky, abortcall
) {
  
  args <- eval_stride(stride, x, use)
  
  # run function:
  out <- .rcpp_slicev_x_atomic(
    x, args$p, args$v, args$na, use
  )
  
  # attributes handling:
  if(!is.null(names(x)) && use.names && length(args$p) != 0L) {
    nms <- .rcpp_slicev_x_atomic(
      names(x), args$p, args$v, args$na, use
    )
    data.table::setattr(out, "names", nms)
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


#' @keywords internal
#' @noRd
.long_pv_set <- function(
    x, stride, use,
    rp, tf, abortcall
) {
  
  
  args <- eval_stride(stride, x, use)
  
  # replacement checks:
  if(!missing(rp)) {
    if(!is.atomic(rp)) {
      stop("`rp` must be an atomic scalar")
    }
    if(length(rp) != 1 && length(rp) != args$len) {
      stop("vector recycling not allowed")
    }
    value <- rp
  }
  
  # transformation checks:
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
    extr <- .rcpp_slicev_x_atomic(
      x, args$p, args$v, args$na, use
    )
    value <- tf(extr)
    if(length(value) != length(extr) && length(value) != 1L) {
      stop("recycling not allowed")
    }
  }
  
  # general value check:
  value <- .internal_coerce_rp(x, value, abortcall)
  
  # run function:
  .rcpp_slicev_set_atomic(x, args$p, args$v, args$na, use, value)
  return(invisible(NULL))
}


#' @keywords internal
#' @noRd
.long_seq_x <- function(
    x, stride, use, use.names, sticky, abortcall
) {
  
  args <- eval_stride(stride, x, use)
  
  # run function:
  out <- .rcpp_slice_seq_x_atomic(x, args, use)
  
  # attributes handling:
  if(!is.null(names(x)) && use.names) {
    nms <- .rcpp_slice_seq_x_atomic(names(x), args, use)
    data.table::setattr(out, "names", nms)
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



#' @keywords internal
#' @noRd
.long_seq_set <- function(
    x, stride, use,
    rp, tf, abortcall
) {
  
  
  args <- eval_stride(stride, x, use)
  
  # replacement checks:
  if(!missing(rp)) {
    if(!is.atomic(rp)) {
      stop("`rp` must be an atomic scalar")
    }
    if(length(rp) != 1 && length(rp) != args$len) {
      stop("vector recycling not allowed")
    }
    value <- rp
  }
  
  # transformation checks:
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
    extr <- .rcpp_slice_seq_x_atomic(
      x, args, use
    )
    value <- tf(extr)
    if(length(value) != length(extr) && length(value) != 1L) {
      stop("recycling not allowed")
    }
  }
  
  # general value check:
  value <- .internal_coerce_rp(x, value, abortcall)
  
  # run function:
  .rcpp_slice_seq_set_atomic(x, value, args, use)
  return(invisible(NULL))
}


#' @keywords internal
#' @noRd
.long_ptrn_x <- function(
    x, stride, use, use.names, sticky, abortcall
) {
  
  args <- eval_stride(stride, x, use)
  
  # run function:
  out <- .rcpp_slice_ptrn_x_atomic(x, args, use)
  
  # attributes handling:
  if(!is.null(names(x)) && use.names) {
    nms <- .rcpp_slice_ptrn_x_atomic(names(x), args, use)
    data.table::setattr(out, "names", nms)
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



#' @keywords internal
#' @noRd
.long_ptrn_set <- function(
    x, stride, use,
    rp, tf, abortcall
) {
  
  
  args <- eval_stride(stride, x, use)
  
  # replacement checks:
  if(!missing(rp)) {
    if(!is.atomic(rp)) {
      stop("`rp` must be an atomic scalar")
    }
    if(length(rp) != 1 && length(rp) != args$len) {
      stop("vector recycling not allowed")
    }
    value <- rp
  }
  
  # transformation checks:
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
    extr <- .rcpp_slice_ptrn_x_atomic(
      x, args, use
    )
    value <- tf(extr)
    if(length(value) != length(extr) && length(value) != 1L) {
      stop("recycling not allowed")
    }
  }
  
  # general value check:
  value <- .internal_coerce_rp(x, value, abortcall)
  
  # run function:
  .rcpp_slice_ptrn_set_atomic(x, value, args, use)
  return(invisible(NULL))
}





#' @keywords internal
#' @noRd
.stride_ntiles_seq <- function(from, to, by) {
  out <- (to - from) / by
  out <- floor(abs(out)) + 1L
  return(out)
}

#' @keywords internal
#' @noRd
.stride_ntiles_ptrn <- function(from, to, by) {
  out <- (to - from + 1) / by
  out <- floor(abs(out))
  return(out)
}

#' @keywords internal
#' @noRd
.is.natural_scalar <- function(x, min = 1) {
  
  if(!is.numeric(x)) return(FALSE)
  if(length(x) != 1L) return(FALSE)
  
  if(is.na(x) || is.infinite(x)) return(FALSE)
  if(round(x) != x) return(FALSE)
  if(x < min) return(FALSE)
  
  return(TRUE)
}

