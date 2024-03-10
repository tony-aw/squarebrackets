#' Specialized Sub-setting Functions
#'
#' @description
#' The `sb_a()` function subsets extracts one or more attributes from an object. \cr
#' \cr
#' The `sb_str()` function subsets characters of single string,
#' or replace a subset of the characters of a single string with the subsets of the characters of another string.
#' In both cases, a single string is treated as a iterable vector,
#' where each single character in a string is a single element.
#' The `sb_str()` function is considerably faster than doing the equivalent operation
#' in base 'R' or even 'stringi'. \cr
#' \cr
#' 
#' @param str a single string.
#' @param ind an integer vector, giving the positions of the string to subset.
#' @param rp.str,rp.ind similar to `str` and `ind`, respectively. \cr
#' If not specified, `sb_str()` will perform something like \cr
#' `str[ind]` \cr
#' treating `str` as an iterable vector. \cr
#' If these ARE specified, `sb_str()` will perform something like \cr
#' `str[ind] <- rp.str[rp.ind]` \cr
#' treating `str` and `rp.str` as iterable vectors. \cr
#' @param x an object
#' @param a a character vector of attribute names.
#' If `NULL` (default), ALL attributes are returned.
#'
#' 
#'
#' @returns
#' The sub-setted object.
#'
#'
#'
#' @examples
#' 
#' x <- matrix(1:10, ncol = 2)
#' colnames(x) <- c("a", "b")
#' attr(x, "test") <- "test"
#' sb_a(x, "test")
#' sb_a(x)
#'
#'
#' x <- "hello"
#' sb_str(x, 5:1) # this gives "olleh"
#' sb_str(x, c(1:5, 5)) # this gives "helloo"
#' sb_str(x, c(2:5)) # this gives "ello"
#' sb_str(x, seq(1, 5, by = 2)) # this gives "hlo"
#' sb_str(x, 1:4, "world", 1:4) # this gives "worlo"
#' 

#' @name sb_special
NULL



#' @rdname sb_special
#' @export
sb_str <- function(str, ind, rp.str, rp.ind) {
  if(missing(rp.str) && missing(rp.ind)) {
    return(.rcpp_sb_str(str, as.integer(ind - 1L)))
  } else {
    if(length(ind) != length(rp.ind)) {
      stop("`ind` and `rp.ind` must be of same size")
    }
    return(.rcpp_sb_str_rp(str, as.integer(ind - 1L), rp.str, as.integer(rp.ind - 1L)))
    
  }
  
}

#' @rdname sb_special
#' @export
sb_a <- function(x, a = NULL) {
  if(is.null(a)) {
    return(attributes(x))
  }
  if(!is.character(a)) stop("`a` must be character vector")
  return(attributes(x)[a])
}

