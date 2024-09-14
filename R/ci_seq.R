#' Construct Sequence Parameters Based on Margins
#' 
#' @description
#' `ci_seq()` returns a list of parameters to construct a sequence based on the margins of an object. \cr
#' It is internally used by the \link{idx_r} function. \cr \cr
#' 
#' @param x,m,start,end,by see \link{idx_r}.
#' @param .abortcall environment where the error message is passed to. \cr \cr
#'  
#' @returns
#' A list of sequence parameters. \cr \cr
#' 
#' @example inst/examples/idx_r.R
#

#' @rdname ci_seq
#' @export
ci_seq <- function(x, m = 0L, start = NULL, end = NULL, by = 1L, .abortcall = sys.call()) {
  
  # convert m:
  if(!is.numeric(m) && !is.complex(m)) {
    stop("`m` must be (complex) numeric")
  }
  maxdim <- ifelse(is.null(dim(x)), length(x), dim(x))
  m <- .ci_seq.convert_margin(m, maxdim, abortcall = .abortcall)
  
  
  # recycle lengths when necessary:
  m.len <- length(m)
  start <- .ci_seq.fix_length(start, m.len, .abortcall)
  end <- .ci_seq.fix_length(end, m.len, .abortcall)
  by <- .ci_seq.fix_length(by, m.len, .abortcall)
  
  
  # perform checks:
  .ci_seq.check_args(m, start, end, by, .abortcall)
  
  if(length(m) == 1L && m == 0L){
    lens <- length(x)
  }
  else {
    lens <- dim(x)[m]
  }
  
  
  .ci_seq.construct(start, end, by, lens, .abortcall)
  
}



#' @keywords internal
#' @noRd
.ci_seq.convert_margin <- function(x, n, abortcall) {
  if(is.complex(x)) {
    if(Re(x) == 0) {
      x <- 0
    }
    else {
      x <- .indx_convert_complex_multi(x, n, abortcall)
      if(.any_badmargin(x, n)) {
        stop(simpleError("index out of bounds", call = abortcall))
      }
    }
  }
  else if(is.numeric(x)) {
    if(.any_badmargin(x, n)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  
  if(length(x) > 1L && collapse::anyv(x, 0)) {
    stop(simpleError("improper `m` given", call = abortcall))
  }
  
  return(x)
}


#' @keywords internal
#' @noRd
.ci_seq.fix_length <- function(x, m.len, abortcall) {
  if(!is.null(x)) {
    if(length(x) != m.len) {
      if(length(x) == 1L) {
        x <- rep_len(x, m.len)
      }
      else {
        stop(simpleError(
          "`m`, `start`, `end` `by` must be equal length, length of 1, or `NULL`",
          call = abortcall
        ))
      }
    }
  }
  
  return(x)
}


#' @keywords internal
#' @noRd
.ci_seq.check_args <- function(m, start, end, by, abortcall) {

  startend.missing <- is.null(start) + is.null(end)
  if(startend.missing == 1L) {
    stop(simpleError(
      "either specify both `start` and `end`, or specify neither",
      call = abortcall
    ))
  }
  
  if(startend.missing == 2L) {
    if(missing(by) || is.null(by)) {
      stop(simpleError("`by` missing", call = abortcall))
    }
  }
  
  arg.list <- list(m, start, end, by)
  cls <- collapse::vclasses(arg.list, use.names = FALSE)
  if(any(!cls %in% c("numeric", "integer", "complex", "NULL"))) {
    stop(simpleError(
      "`m`, `start`, `end` `by` must be (complex) numeric or `NULL`",
      call = abortcall
    ))
  }
  
  if(collapse::anyv(by, 0)) {
    stop(simpleError("`by` cannot be zero", call = abortcall))
  }
  
}



#' @keywords internal
#' @noRd
.ci_seq.construct <- function(start, end, by, n, abortcall) {
  
  if(is.null(start) && is.null(end)) {
    return(.ci_seq.construct_withby(by, n, abortcall))
  }
  else {
    return(.ci_seq.construct_withall(start, end, by, n, abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.ci_seq.convert_startend <- function(x, n, abortcall) {
  if(is.complex(x)) {
    x <- .indx_convert_complex_multi(x, n, abortcall)
    if(any(x > n | x < 1L)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  else if(is.numeric(x)) {
    if(any(x > n | x < 1L)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  return(x)
}


#' @keywords internal
#' @noRd
.indx_convert_complex_multi <- function(indx, n, abortcall) {
  im <- Im(indx)
  re <- Re(indx)
  out <- .rcpp_indx_convert_cplx_multi(re, im, n)
  return(out)
}


#' @keywords internal
#' @noRd
.ci_seq.construct_withby <- function(by, n, abortcall) {
  if(length(by) == 1L) {
    if(by > 0L) {
      start <- 1L
      end <- n
      length <- floor((end - start )/by) + 1L
    }
    if(by < 0L) {
      start <- n
      end <- 1L
      length <- floor((start - end)/by) + 1L
    }
    return(list(start = start, end = end, by = by, length = length))
  }
  
  
  start <- ifelse(by > 0L, 1L, n)
  end <- ifelse(by > 0L, n, 1L)
  length <- ifelse(
    by > 0L,
    floor((end - start )/by) + 1L,
    floor((start - end)/by) + 1L
  )
  out <- list(
    start = start,
    end = end,
    by = by,
    length = length
  )
  return(out)
}


#' @keywords internal
#' @noRd
.ci_seq.construct_withall <- function(start, end, by, n, abortcall) {
  
  by <- abs(by)
  
  start <- .ci_seq.convert_startend(start, n, abortcall)
  end <- .ci_seq.convert_startend(end, n, abortcall)
  by <- ifelse(start < end, by, -by)
  length <- ifelse(
    start < end,
    floor((end - start )/by) + 1L,
    floor((start - end)/by) + 1L
  )
  
  out <- list(
    start = start,
    end = end,
    by = by,
    length = length
  )
  return(out)
  
}


