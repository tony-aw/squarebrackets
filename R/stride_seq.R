#' Regular Sequence-based stride
#'
#' @description
#' `stride_seq()` specifies a regular sequence for use in the `long_` methods. \cr
#' For example: \cr
#' `long_x(x, stride_seq(from, to, by))` \cr
#' is conceptually equivalent to \cr
#' `x[seq(from, to, by)]` \cr
#' 
#' @param from natural scalar giving the starting point of the sequence.
#' @param to natural scalar giving the maximalle allowed end value.
#' @param by natural scalar giving the step-size.
#' 
#' @returns
#' An object of class "stride".
#' 
#' 
#' @example inst/examples/stride_seq.R
#' 
#' @seealso \link{squarebrackets_stride} \cr
#' 

#' @rdname stride_seq
#' @export
stride_seq <- function(from, to, by) {
  
  # check args:
  .stride_seq_checkargs(from, to, by, sys.call())
  
  # MAIN FUNCTION:
  step_size <- abs(by)
  by <- ifelse(from > to, -step_size, step_size)
  n_tiles <- .stride_ntiles_seq(from, to, by)
  
  start <- from
  end <- from + by * (n_tiles - 1)
  
  out <- pairlist(
    start = start,
    end = end,
    step_size = abs(by),
    n_tiles = n_tiles
  )
  class(out) <- c("stride_seq", "stride")
  return(out)
}


#' @keywords internal
#' @noRd
.stride_seq_checkargs <- function(from, to, by, abortcall) {
  if(!.is.natural_scalar(from) || !.is.natural_scalar(to) || !.is.natural_scalar(abs(by))) {
    stop(simpleError("`from`, `to`, and `by` must be natural scalars", call = abortcall))
  }
  if(abs(by) > (2^31 - 1)) {
    stop(simpleError("`by` must not be larger than `2^31 - 1`", call = abortcall))
  }
  
}



