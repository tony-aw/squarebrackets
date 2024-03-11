#' Recursive Sequence Generator
#'
#' @description
#' This is a recursive sequence generator.
#' The function is essentially a highly generalized version of a Fibonacci sequence generator.
#' One can change the initial values, the window size,
#' and even the window function used. \cr
#' This function assumes only the following about the sequence being generated:
#' 
#'  * The sequence consists of real numbers (i.e. class \code{integer} or class \code{double}).
#'  * The window size is the same for all iterations.
#'  * The window function is the same for all iterations.
#'  * The sequence grows until a vector of length \code{n} is achieved.
#'  
#'
#' @param inits a numeric (double or integer) vector giving the initial values. \cr
#' Any numbers are allowed, even negative and/or fractional numbers. \cr
#' Note that numbers given must give valid results when passed to function \code{f()}. \cr
#' IMPORTANT: The length of `inits` determines the window size `w`. \cr
#' For a regular Fibonacci, \code{inits = 0:1},
#' which of course means a window size of `w = 2`. \cr
#' @param n a single integer,
#' giving the size of the numeric vector to generate. \cr
#' NOTE: it must hold that `n` is larger than or equal to the window size `w`. \cr
#' The window size is equal to `w = length(inits)`.
#' @param f the function to be used on the last \code{w} numbers
#' to generate the next number of the sequence at each iteration. \cr
#' This must be a function that takes as input a numeric vector,
#' and returns a single numeric value. \cr
#' For a regular Fibonacci sequence, this would be either: \cr
#' \code{f = sum}, \cr
#' or (since window size is 2) \code{f = \(x) x[2] + x[1]} \cr
#' 
#'
#' @details
#' The default values of the arguments give the first 10 numbers
#' of a regular Fibonacci sequence. \cr
#' See examples for several number series created with this function. \cr
#' This function is written in C++ using \code{Rcpp} for better performance.
#'
#'
#' @returns
#' A sequence of numbers.
#'
#'
#' @example inst/examples/seq_rec.R
#'

#' @rdname seq_rec
#' @export
seq_rec <- function(inits = c(0, 1), n = 10L, f = sum) {
  n <- as.integer(n)
  if(length(n) != 1) {
    stop("`n` must be an integer of length 1")
  }
  w <- length(inits)
  if(w > n) {
    stop("`n` cannot be smaller than the window size (= `length(inits)`")
  }
  
  return(.rcpp_seq_rec(inits, w, n, f))
}
