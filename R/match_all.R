#' Match All, Order-Sensitive and Duplicates-Sensitive
#'
#' @description
#' 
#' Find all indices of vector `haystack` that are equal to vector `needles`,
#' taking into account the order of both vectors, and their duplicate values. \cr
#' \cr
#' It is essentially a much more efficient version of:
#' 
#' ```
#' lapply(needles, \(i) which(haystack == i))
#' 
#' ```
#' 
#' Like `lapply(needles, \(i) which(haystack == i))`, `NA`s are ignored. \cr
#' \cr
#' Core of the code is based on a suggestion by Sebastian Kranz
#' (author of the 'collapse' package). \cr
#' \cr
#' 
#' 
#' 
#' @param needles,haystack vectors
#' @param unlist Boolean,
#' indicating if the result should be a vector (`TRUE`, default),
#' or a list (`FALSE`).
#' 
#' 
#' @returns
#' An integer vector, or list of integer vector. \cr
#' If a list, each element of the list corresponds to each value of `needles`. \cr
#' When `needles` and/or `haystack` is/are empty or fully `NA`,
#' `match_all()` returns an empty integer vector (if `unlist = TRUe`),
#' or an empty list (if `unlist = FALSE`). \cr
#'
#'
#'
#' @examples
#' n <- 200
#' haystack <- sample(letters, n, TRUE)
#' needles <- sample(letters, n/2, TRUE)
#' indices1 <- match_all(needles, haystack)
#' head(indices1)
#' 
#'  

#' @rdname match_all
#' @export
match_all <- function(needles, haystack, unlist = TRUE) {
  if(length(collapse::na_omit(needles)) == 0 || length(collapse::na_omit(haystack)) == 0) {
    if(unlist) return(integer(0))
    return(list())
  }
  v <- collapse::funique(needles)
  m <- collapse::fmatch(haystack, v)
  attr(m, "N.groups") <- length(v)
  oldClass(m) <- "qG"
  out <- collapse::gsplit(g = m)
  names(out) <- v
  out[collapse::whichNA(names(out))] <- NULL
  
  if(length(needles) == length(v)) {
    if(unlist) return(unlist(out, use.names = FALSE, recursive = FALSE))
    return(out)
  }
  
  if(unlist) return(unlist(out[needles], use.names = FALSE, recursive = FALSE))
  return(out[needles])
}

