#' Construct Parameters for a Sequence Based on Margins
#' 
#' @description
#' `cp_seq()` returns a list of parameters to construct a sequence based on the margins of an object. \cr
#' It is internally used by the \link{idx_r} function and \link{slice} method. \cr \cr
#' 
#' @param x the object for which to compute margin-based sequence parameters.
#' @param m integer or complex, giving the margin(s). \cr
#' For non-dimensional objects or for flat indices, specify `m = 0L`.
#' @param from integer or complex, of the same length as `m` or of length 1,
#' specifying the from point.
#' @param to integer or complex, of the same length as `m` or of length 1,
#' specifying the \bold{maximally allowed} end value.
#' @param by integer, of the same length as `m` or of length 1,
#' specifying the step size. \cr
#'  
#'  
#'  
#' @section Arguments Details: 
#' \bold{Multiple dimensions at once} \cr
#' The `cp_seq` function can construct the sequence parameters needed for multiple dimensions at once,
#' by specifying a vector for `m`. \cr
#' The lengths of the other arguments are then recycled if needed. \cr 
#' \cr
#' \bold{Using only `by`} \cr
#' If `from, to` are not specified,
#' using `by` will construct the following sequence: \cr
#' If `by` is positive, `seq.int(1L, n, by)`. \cr
#' If `by` is negative, `seq.int(n, 1L, by)`. \cr
#' Where `n` is the maximum index
#' (i.e. `length(x)` or `dim(x)[m]`, depending on the situation). \cr
#' \cr
#' \bold{Using `from, to, by`} \cr
#' If `from, to, by` are all specified,
#' `by` is stored as `abs(by)`,
#' and the sign  of `by` is automatically adjusted to ensure a sensible sequence is created. \cr
#' \cr
#' \cr
#' 
#' 
#'  
#'
#' @returns
#' A list of the following elements: \cr
#' \cr
#' \bold{`$start`}: \cr
#' The actual starting point of the sequence. \cr
#' This is simply `from` translated to regular numeric. \cr
#' \cr
#' \bold{`$end`}: \cr
#' The \bold{actual} ending point of the sequence. \cr
#' This is \bold{not} the same as `to`, not even whe translated to regular numeric. \cr
#' For example, the following code:
#' 
#' ```{r eval = TRUE, echo = TRUE}
#' seq(from = 1L, to = 10L, by = 2L)
#' 
#' ```
#' specifies `to = 10L`. \cr
#' But the sequence doesn't actually end at `10`; it ends at `9`. \cr
#' Therefore, `cp_seq(x, m, 1, 10, 2)` will return `end = 9`, not `end = 10`. \cr
#' This allows the user to easily predict where an sequence given in
#' \link{idx_r}/\link{slice} will actually end. \cr
#' \cr
#' \bold{`$by`}: \cr
#' This will give `by`, but with it's sign adjusted, if needed. \cr
#' \cr
#' \bold{`$length.out`}: \cr
#' The actual vector lengths the sequences would be,
#' given the translated parameters. \cr \cr
#' 
#' 
#' 
#' @example inst/examples/idx_r.R
#

#' @rdname cp_seq
#' @export
cp_seq <- function(x, m = 0L, from = NULL, to = NULL, by = 1L) {
  
  # convert m:
  if(!is.numeric(m) && !is.complex(m)) {
    stop("`m` must be (complex) numeric")
  }
  dimsizes <- ifelse(is.null(dim(x)), length(x), dim(x))
  m <- .cp_seq.convert_margin(m, dimsizes, abortcall = sys.call())
  
  
  # recycle lengths when necessary:
  m.len <- length(m)
  from <- .cp_seq.fix_length(from, m.len, sys.call())
  to <- .cp_seq.fix_length(to, m.len, sys.call())
  by <- .cp_seq.fix_length(by, m.len, sys.call())
  
  
  # perform checks:
  .cp_seq.check_args(m, from, to, by, sys.call())
  
  if(length(m) == 1L && m == 0L){
    lens <- length(x)
  }
  else {
    lens <- dim(x)[m]
  }
  
  
  .cp_seq.construct(from, to, by, lens, sys.call())
  
}



#' @keywords internal
#' @noRd
.cp_seq.convert_margin <- function(x, dimsizes, abortcall) {
  if(is.complex(x)) {
    if(length(Im(x)) == 1L && Im(x) == 0) {
      x <- 0
    }
    else {
      x <- .C_convert_cplx(Im(x), dimsizes)
      if(.any_badmargin(x, dimsizes)) {
        stop(simpleError("index out of bounds", call = abortcall))
      }
    }
  }
  else if(is.numeric(x)) {
    if(.any_badmargin(x, dimsizes)) {
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
.cp_seq.fix_length <- function(x, m.len, abortcall) {
  if(!is.null(x)) {
    if(length(x) != m.len) {
      if(length(x) == 1L) {
        x <- rep_len(x, m.len)
      }
      else {
        stop(simpleError(
          "`m`, `from`, `to` `by` must be equal length, length of 1, or `NULL`",
          call = abortcall
        ))
      }
    }
  }
  
  return(x)
}


#' @keywords internal
#' @noRd
.cp_seq.check_args <- function(m, from, to, by, abortcall) {

  # check from/to combination
  startend.missing <- is.null(from) + is.null(to)
  if(startend.missing == 1L) {
    stop(simpleError(
      "either specify both `from` and `to`, or specify neither",
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
  
  arg.list <- list(m, from, to, by)
  cls <- collapse::vclasses(arg.list, use.names = FALSE)
  if(any(!cls %in% c("numeric", "integer", "complex", "NULL"))) {
    stop(simpleError(
      "`m`, `from`, `to` `by` must be (complex) numeric or `NULL`",
      call = abortcall
    ))
  }
  
  
  
}



#' @keywords internal
#' @noRd
.cp_seq.construct <- function(from, to, by, dimsizes, abortcall) {
  
  if(is.null(from) && is.null(to)) {
    return(.cp_seq.construct_withby(by, dimsizes, abortcall))
  }
  else {
    return(.cp_seq.construct_withall(from, to, by, dimsizes, abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.cp_seq.construct_withby <- function(by, dimsizes, abortcall) {
  if(length(by) == 1L) {
    if(by > 0L) {
      from <- 1L
      to <- dimsizes
      length.out <- .cp_seq.calc_length.out(from, to, by)
      to <- .cp_seq.recalc_end(from, by, length.out, dimsizes, abortcall)
      
    }
    if(by < 0L) {
      from <- dimsizes
      to <- 1L
      length.out <- .cp_seq.calc_length.out(from, to, by)
      to <- .cp_seq.recalc_end(from, by, length.out, dimsizes, abortcall)
    }
    return(list(start = from, end = to, by = by, length.out = length.out))
  }
  
  
  from <- data.table::fifelse(by > 0L, 1L, dimsizes)
  to <- data.table::fifelse(by > 0L, dimsizes, 1L)
  length.out <- .cp_seq.calc_length.out(from, to, by)
  to <- .cp_seq.recalc_end(from, by, length.out, dimsizes, abortcall)
  out <- list(
    start = from,
    end = to,
    by = by,
    length.out = length.out
  )
  return(out)
}


#' @keywords internal
#' @noRd
.cp_seq.construct_withall <- function(from, to, by, dimsizes, abortcall) {
  
  by <- abs(by)
  
  from <- .cp_seq.convert_startend(from, dimsizes, abortcall)
  to <- .cp_seq.convert_startend(to, dimsizes, abortcall)
  by <- data.table::fifelse(from < to, by, -by)
  length.out <- .cp_seq.calc_length.out(from, to, by)
  to <- .cp_seq.recalc_end(from, by, length.out, dimsizes, abortcall)
  
  out <- list(
    start = from,
    end = to,
    by = by,
    length.out = length.out
  )
  return(out)
  
}

#' @keywords internal
#' @noRd
.cp_seq.convert_startend <- function(x, dimsizes, abortcall) {
  
  if(anyNA(x)) {
    stop(simpleError("index out of bounds", call = abortcall))
  }
  
  if(is.complex(x)) {
    x <- .C_convert_cplx(Im(x), dimsizes)
    if(any(x > dimsizes | x < 1L)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  else if(is.numeric(x)) {
    if(any(x > dimsizes | x < 1L)) {
      stop(simpleError("index out of bounds", call = abortcall))
    }
  }
  return(x)
}


#' @keywords internal
#' @noRd
.cp_seq.calc_length.out <- function(from, to, by) {
  out <- (to - from) / by
  out <- floor(abs(out)) + 1L
  return(out)
}

#' @keywords internal
#' @noRd
.cp_seq.recalc_end <- function(from, by, length.out, dimsizes, abortcall) {
  to <- from + by * (length.out - 1)
  if(any(to > dimsizes | to < 1L)) {
    stop(simpleError("index out of bounds", call = abortcall))
  }
  return(to)
  
}
