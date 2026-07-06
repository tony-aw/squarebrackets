#' Evaluate Stride Object
#' 
#' @description
#' `eval_stride()` evaluates a stride or formula object. \cr
#' This can be handy to check exactly what parameters will be fed to the `long_` methods. \cr
#' \cr
#' `formula2stride()` converts a formula to a stride_seq or stride_ptrn object.
#' 
#' @param stride see \link{squarebrackets_stride}.
#' @param form a formula, as described in \link{squarebrackets_stride}.
#' @param x a (long) atomic vector.
#' 
#'
#' @returns
#' \bold{Using `stride_v()`} \cr
#' The original stride object, but as a list. \cr
#' \cr
#' \cr
#' \bold{Using `stride_seq()` or `stride_ptr()`} \cr
#' A list with at least the following elements: \cr
#' \cr
#' \bold{`$start`}: \cr
#' The actual starting point of the sequence. \cr
#' This is simply `from` translated to regular numeric. \cr
#' \cr
#' \bold{`$end`}: \cr
#' The \bold{actual} ending point of the sequence. \cr
#' This is not the same as `to`. \cr
#' For example, the following code:
#' 
#' ```{r eval = TRUE, echo = TRUE}
#' seq(from = 1L, to = 10L, by = 2L)
#' 
#' ```
#' specifies `to = 10L`. \cr
#' But the sequence doesn't actually end at `10`; it ends at `9`. \cr
#' Therefore, `stride_seq(x, 1, 10, 2) |> eval_stride()` will return `end = 9`, not `end = 10`. \cr
#' This allows the user to easily predict where an sequence given in
#' \link{stride_seq}/\link{stride_ptrn} will actually end. \cr
#' \cr
#' \bold{`$len`}: \cr
#' The actual vector length the sequence would be,
#' given the translated parameters. \cr \cr
#' 
#' 
#' 
#' @example inst/examples/long.R


#' @name stride_eval
NULL

#' @rdname stride_eval
#' @export
eval_stride <- function(stride, x) {
  UseMethod("eval_stride", x)
}

#' @rdname stride_eval
#' @export
eval_stride.default <- function(stride, x) {
  .eval_stride_checkargs(stride, x, sys.call())
  
  if(is.formula(stride)) {
    stride <- formula2stride(stride, x)
  }
  
  if(class(stride)[1] == "stride_v") {
    return(.eval_stride_v(stride, x, sys.call()))
  }
  else if(class(stride)[1] == "stride_seq") {
    return(.eval_stride_seq(stride, x, sys.call()))
  }
  else if(class(stride)[1] == "stride_ptrn") {
    return(.eval_stride_ptrn(stride, x, sys.call()))
  }
  else {
    stop("unknown type of `stride` given")
  }
}


#' @rdname stride_eval
#' @export
is.stride <- function(x) {
  if(!is.pairlist(x)) return(FALSE)
  if(length(class(x)) != 2L) return(FALSE)
  if(!class(x)[2] == "stride") return(FALSE)
  if(!class(x)[1] %in% c("stride_seq", "stride_ptrn", "stride_v")) return(FALSE)
  
  return(TRUE)
}

#' @rdname stride_eval
#' @export
formula2stride <- function(form, x) {
  keywords <- list(
    .N = length(x),
    `:` = function(e1, e2) c(e1, list(e2))
  )
  obj <- .with_array(x, keywords, form, sys.call())
  if(!is.list(obj)) {
    stop("improper formula given")
  }
  if(length(obj) != 4L) {
    stop("improper formula given")
  }
  
  has_use <- .stride_use_OK(obj[[4L]])
  if(!has_use) {
    stop("improper formula given")
  }
  
  is_seq <- all(sapply(obj[1:3], .is.natural_scalar))
  is_ptrn <- all(sapply(obj[1:2], .is.natural_scalar)) && is.logical(obj[[3L]])
  
  if(is_seq) {
    stride <- stride_seq(obj[[1L]], obj[[2L]], obj[[3L]], obj[[4L]])
  }
  else if(is_ptrn) {
    stride <- stride_ptrn(obj[[1L]], obj[[2L]], obj[[3L]], obj[[4L]])
  }
  else {
    stop("improper formula given")
  }
  
  return(stride)
}

#' @keywords internal
#' @noRd
.eval_stride_checkargs <- function(stride, x, abortcall) {
  
  if(!is.stride(stride) && !is.formula(stride)) {
    txt <- "`stride` must be an object inheriting from class \"stride\", or a formula"
    stop(simpleError(txt, call = abortcall))
  }
  stopifnot(couldb.mutatomic(x))
  
}


#' @keywords internal
#' @noRd
.eval_stride_v <- function(stride, x, abortcall) {
  
  if(length(x) != length(stride$y)) {
    stop(simpleError("`p` must be of same length as `x`"))
  }
  
  out <- as.list(stride)
  out$len <- out$prepvector[3L]
  
  return(out)
}



#' @keywords internal
#' @noRd
.eval_stride_seq <- function(stride, x, abortcall) {
  
  x.len <- length(x)
  out <- as.list(stride)
  use <- out$use
  if(use > 0) {
    out$len <- stride$n_tiles
  }
  else if(use < 0) {
    out$len <- x.len - stride$n_tiles
    if(stride$end < stride$start) {
      out$start <- stride$end
      out$end <- stride$start
    }
  }
 
  if(any(as.numeric(out[1:3]) > x.len)) {
    stop(simpleError("`stride` out of bounds", call = abortcall))
  }
  
  return(out)
}


#' @keywords internal
#' @noRd
.eval_stride_ptrn <- function(stride, x, abortcall) {
  
  x.len <- length(x)
  out <- as.list(stride)
  use <- out$use
  out$len <- .ptrn_len(stride$start, stride$end, stride$ptrn, x.len, use)
  
  if(use > 0) {
    out$ptrn <- as.integer(which(stride$ptrn) - 1L)
  }
  else if(use < 0) {
    ptrn <- stride$ptrn
    if(stride$end < stride$start) {
      ptrn <- .rcpp_ptrn_shift(ptrn, stride$start, stride$end)
      out$start <- stride$end
      out$end <- stride$start
    }
    out$ptrn <- as.integer(which(!ptrn) - 1L)
    
  }
  else {
    stop("unknown `use` given")
  }
  
  
  if(any(as.numeric(out[1:3]) > x.len)) {
    stop(simpleError("`stride` out of bounds", call = abortcall))
  }
  
  return(out)
}


#' @keywords internal
#' @noRd
.ptrn_len <- function(start, end, ptrn, x.len, use) {
  
  if(use > 0) {
    return(ptrn_len(start, end, ptrn))
  }
  else if(use < 0) {
    return(x.len - ptrn_len(start, end, ptrn))
  }
  else {
    stop("`use` must be `1` or `-1`")
  }
}
