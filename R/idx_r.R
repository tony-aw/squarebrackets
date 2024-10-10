#' Compute Integer Index Range
#' 
#' @description
#' `idx_r()` computes integer index range(s). \cr \cr
#' 
#' @param x the object for which to compute subset indices.
#' @param m,from,to,by see \link{cp_seq}. \cr \cr
#' 
#' 
#'
#' @returns
#' If `length(m) == 1L`: a vector of numeric indices. \cr
#' \cr
#' If `length(m) > 1L`: a list of the same length as `m`,
#' containing numeric vectors of indices.  \cr \cr
#' 
#' @example inst/examples/idx_r.R
#

#' @rdname idx_r
#' @export
idx_r <- function(x, m = 0L, from = NULL, to = NULL, by = 1L) {
  
  lst <- cp_seq(x, m, from, to, by)
  
  if(length(m) == 1L) {
    return(seq.int(lst$start, lst$end, lst$by))
  }
  
  from <- lst$start
  to <- lst$end
  by <- lst$by
  
  return(mapply(seq.int, from, to, by, SIMPLIFY = FALSE))
 
}
