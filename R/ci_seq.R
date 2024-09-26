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
    if(Im(x) == 0) {
      x <- 0
    }
    else {
      x <- .C_convert_cplx(Im(x), n)
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

  # check start/end combination
  startend.missing <- is.null(start) + is.null(end)
  if(startend.missing == 1L) {
    stop(simpleError(
      "either specify both `start` and `end`, or specify neither",
      call = abortcall
    ))
  }
  
  # check by
  if(missing(by) || is.null(by)) {
    stop(simpleError("`by` missing", call = abortcall))
  }
  if(anyNA(by)) {
    stop(simpleError("`by` cannot be `NA`", call = abortcall))
  }
  if(collapse::anyv(by, 0)) {
    stop(simpleError("`by` cannot be zero", call = abortcall))
  }
  if(any(by != round(by))) {
    stop(simpleError("`by` cannot be fractional", call = abortcall))
  }
  
  arg.list <- list(m, start, end, by)
  cls <- collapse::vclasses(arg.list, use.names = FALSE)
  if(any(!cls %in% c("numeric", "integer", "complex", "NULL"))) {
    stop(simpleError(
      "`m`, `start`, `end` `by` must be (complex) numeric or `NULL`",
      call = abortcall
    ))
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
  
  if(anyNA(x)) {
    stop(simpleError("index out of bounds", call = abortcall))
  }
  
  if(is.complex(x)) {
    x <- .C_convert_cplx(Im(x), n)
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
.ci_seq.construct_withby <- function(by, n, abortcall) {
  if(length(by) == 1L) {
    if(by > 0L) {
      start <- 1L
      end <- n
      length.out <- .ci_seq.calc_length.out(start, end, by)
      end <- .ci_seq.recalc_end(start, by, length.out, n, abortcall)
      
    }
    if(by < 0L) {
      start <- n
      end <- 1L
      length.out <- .ci_seq.calc_length.out(start, end, by)
      end <- .ci_seq.recalc_end(start, by, length.out, n, abortcall)
    }
    return(list(start = start, end = end, by = by, length.out = length.out))
  }
  
  
  start <- data.table::fifelse(by > 0L, 1L, n)
  end <- data.table::fifelse(by > 0L, n, 1L)
  length.out <- .ci_seq.calc_length.out(start, end, by)
  end <- .ci_seq.recalc_end(start, by, length.out, n, abortcall)
  out <- list(
    start = start,
    end = end,
    by = by,
    length.out = length.out
  )
  return(out)
}


#' @keywords internal
#' @noRd
.ci_seq.construct_withall <- function(start, end, by, n, abortcall) {
  
  by <- abs(by)
  
  start <- .ci_seq.convert_startend(start, n, abortcall)
  end <- .ci_seq.convert_startend(end, n, abortcall)
  by <- data.table::fifelse(start < end, by, -by)
  length.out <- .ci_seq.calc_length.out(start, end, by)
  end <- .ci_seq.recalc_end(start, by, length.out, n, abortcall)
  
  out <- list(
    start = start,
    end = end,
    by = by,
    length.out = length.out
  )
  return(out)
  
}


#' @keywords internal
#' @noRd
.ci_seq.calc_length.out <- function(start, end, by) {
  out <- (end - start) / by
  out <- floor(abs(out)) + 1L
  return(out)
}

#' @keywords internal
#' @noRd
.ci_seq.recalc_end <- function(start, by, length.out, n, abortcall) {
  end <- start + by * (length.out - 1)
  if(any(end > n | end < 1L)) {
    stop(simpleError("index out of bounds", call = abortcall))
  }
  return(end)
  
}


.ci_seq.convert_cplx <- function(x, n) {
  if(n < (2^31 - 1)) {
    indx <- .C_convert_cplx_32(Im(indx), as.integer(n))
  }
  else {
    indx <- .C_convert_cplx_64(Im(indx), as.double(n))
  }
}
