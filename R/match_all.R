#' Match All, Order-Sensitive and Duplicates-Sensitive
#'
#' @description
#' 
#' Find all indices of vector `haystack` that are equal to vector `needles`,
#' taking into account the order of both vectors, and their duplicate values. \cr
#' \cr
#' `match_all()` is essentially a much more efficient version of:
#' 
#' ```
#' lapply(needles, \(i) which(haystack == i))
#' 
#' ```
#' 
#' Like `lapply(needles, \(i) which(haystack == i))`, `NA`s are ignored. \cr
#' \cr
#' `match_all()` internally calls \code{collapse::}\link[collapse]{fmatch}
#' and \code{collapse::}\link[collapse]{gsplit}. \cr
#' Core of the code is based on a suggestion by Sebastian Kranz
#' (author of the 'collapse' package). \cr
#' \cr
#' 
#' 
#' 
#' @param needles,haystack vectors of the same type. \cr
#' `needles` cannot contain `NA`/`NaN`. \cr
#' Long vectors are not supported.
#' @param unlist Boolean,
#' indicating if the result should be a single unnamed integer vector (`TRUE`, default),
#' or a named list of integer vectors (`FALSE`). \cr
#' 
#' 
#' @returns
#' An integer vector, or list of integer vectors. \cr
#' If a list, each element of the list corresponds to each value of `needles`. \cr
#' When `needles` and/or `haystack` is empty, or when `haystack` is fully `NA`,
#' `match_all()` returns an empty integer vector (if `unlist = TRUE`),
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
  
  if(length(needles) == 0L || length(haystack) == 0L) {
    if(unlist) return(integer(0L))
    return(list())
  }
  if(collapse::allNA(haystack)) {
    if(unlist) return(integer(0L))
    return(list())
  }
  if(typeof(needles) != typeof(haystack)) {
    stop("type of `needles` does not match type of `haystack`")
  }
  
  
  v <- collapse::funique(needles)
  if(anyNA(v)) {
    stop("`NA` not allowed in `needles`")
  }
  is_simple_match <- unlist && (length(v) == length(needles))
  
  if(length(v) == 1L) {
    out <- collapse::whichv(haystack, v)
    if(is_simple_match) {
      return(out)
    }
    out <- list(out)
    names(out) <- as.character(v)
  }
  else {
    m <- collapse::fmatch(haystack, v)
    if(is_simple_match) {
      out <- .C_match_range(collapse::radixorderv(m), m)
      return(out)
    }
    attr(m, "N.groups") <- length(v)
    oldClass(m) <- "qG"
    out <- collapse::gsplit(g = m)
    names(out) <- as.character(v)
    out[collapse::whichNA(names(out))] <- NULL
  }
  
  
  if(length(needles) == length(v)) {
    if(unlist) return(unlist(out, use.names = FALSE, recursive = FALSE))
    return(out)
  }
  
  if(unlist) return(unlist(out[needles], use.names = FALSE, recursive = FALSE))
  return(out[needles])
}


