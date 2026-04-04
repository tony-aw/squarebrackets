#' Pattern Sequence-based stride
#'
#' @description
#' `stride_ptrn()` specifies a patterned sequence,
#' in the form of `(start:end)[ptrn]`,
#' for use in the \link[=long_x]{long_} methods. \cr
#' \cr
#' For example: \cr
#' the sub-set operation `long_x(x, stride_ptrn(start, end, ptrn))` \cr
#' is conceptually equivalent to \cr
#' `x[(start:end)[ptrn]]` \cr
#' \cr
#' `ptrn_len(start, end, ptrn)` calculates the length of `(start:end)[ptrn]`,
#' without actually allocating said vector. \cr
#' \cr
#' 
#' 
#' @param start natural scalar giving the starting point of the sequence.
#' @param end natural scalar giving the ending point of the sequence.
#' @param ptrn a logical vector, giving the pattern. \cr
#' `ptrn` must contain at least 1 `TRUE` and 1 `FALSE` element,
#' to avoid ambiguity. \cr
#' `ptrn` must have length that fulfils all of the following conditions:
#'  - it is `>= 2`
#'  - it is `<= length(start:end)`
#'  - it is `<= 2^31 - 1` \cr
#' 
#' @example inst/examples/stride_ptrn.R
#' 
#' 
#' @returns
#' An object of class "stride".
#' 
#' @seealso \link{squarebrackets_stride} \cr
#' 


#' @rdname stride_ptrn
#' @export
stride_ptrn <- function(start, end, ptrn) {
  
  # check args:
  .stride_ptrn_checkargs(start, end, ptrn, sys.call())
  
  # MAIN FUNCTION:
  tile_size <- length(ptrn)
  
  start <- start
  end <- end
  
  out <- pairlist(
    start = data.table::copy(start),
    end = data.table::copy(end),
    tile_size = tile_size,
    ptrn = ptrn
  )
  class(out) <- c("stride_ptrn", "stride")
  return(out)
}


#' @rdname stride_ptrn
#' @export
ptrn_len <- function(start, end, ptrn) {
  
  # check args:
  .stride_ptrn_checkargs(start, end, ptrn, sys.call())
  
  n <- abs(end - start) + 1
  l <- length(ptrn)
  
  full_reps <- n %/% l
  remainder <- n %% l
  total <- full_reps * sum(ptrn, na.rm = TRUE)
  
  if (remainder > 0) {
    total <- total + sum(ptrn[1:remainder], na.rm = TRUE)
  }
  
  return(total)
}


#' @keywords internal
#' @noRd
.stride_ptrn_checkargs <- function(start, end, ptrn, abortcall) {
  if(!.is.natural_scalar(start) || !.is.natural_scalar(end)) {
    stop(simpleError("`start` and `end` must be natural scalars", call = abortcall))
  }
  if(!is.logical(ptrn)) {
    stop(simpleError("`ptrn` must be a logical vector", call = abortcall))
  }
  if(length(ptrn) < 2) {
    stop(simpleError("`ptrn` must have at least 2 elements", call = abortcall))
  }
  if(anyNA(ptrn)) {
    stop(simpleError("`ptrn` cannot have NAs", call = abortcall))
  }
  if(length(ptrn) > (2^31 - 1)) {
    stop(simpleError("`ptrn` may not be a long vector (that would defeat the point)", call = abortcall))
  }
  
  rng_len <- abs(start - end) + 1
  if(length(ptrn) > rng_len) {
    stop(simpleError("`ptrn` may not be longer than the range start:end", call = abortcall))
  }
  
  ptrn.sum <- sum(ptrn)
  if(ptrn.sum == length(ptrn) || ptrn.sum == 0L) {
    txt <- "`ptrn` must contain at least one `TRUE` and one `FALSE` element"
    stop(simpleError(txt, call = abortcall))
  }
  
}



