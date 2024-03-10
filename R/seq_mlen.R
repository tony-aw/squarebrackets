#' Multiple seq_len
#'
#' @description
#' Create multiple sequences of certain lengths at once. \cr
#' It is a vectorized version of `lapply(x, seq_len)`. \cr \cr
#'  
#'
#' @param x an integer vector giving the lengths.
#' 
#'
#' @returns
#' A list with `length(x)` elements, where each element is the result of `seq_len`.
#'
#'
#' @examples
#' seq_mlen(c(10, 10, 3))
#'

#' @rdname seq_mlen
#' @export
seq_mlen <- function(x) {
  if(!is.numeric(x)) {
    stop("`x` must be a integer vector")
  }
  return(.rcpp_seq_mlen(as.integer(x)))
}
