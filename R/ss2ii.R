#' Convert Subscripts to Coordinates, Coordinates to Interior Indices, and Vice-Versa
#'
#' @description
#' These functions convert a list of integer subscripts to an integer matrix of coordinates,
#' an integer matrix of coordinates to an integer vector of interior indices,
#' and vice-versa. \cr
#' Inspired by the `sub2ind` function from 'MatLab'. \cr
#' 
#'  * `ss2coord()`
#'  converts a list of integer subscripts to an integer matrix of coordinates.
#'  * `coord2ii()`
#'  converts an integer matrix of coordinates to an integer vector of interior indices.
#'  * `ii2coord()`
#'  converts an integer vector of interior indices to an integer matrix of coordinates.
#'  * `coord2ss()`
#'  converts an integer matrix of coordinates to a list of integer subscripts; \cr
#'  it performs a very simple (one might even say naive) conversion. \cr
#'  * `ss2ii()`
#'  is a faster and more memory efficient version of \cr
#'  `ss2coord(sub, x.dims) |> coord2ii(x.dims)` \cr \cr
#' 
#' 
#' All of these functions are written to be memory-efficient. \cr
#' The `coord2ii()` is thus the opposite of \link{arrayInd},
#' and `ii2coord` is merely a convenient wrapper around \link{arrayInd}. \cr
#' \cr
#' 
#'
#'
#' @param sub a list of integer subscripts. \cr
#' The first element of the list corresponds to the first dimension (rows),
#' the second element to the second dimensions (columns),
#' etc. \cr
#' The length of `sub` must be equal to the length of `x.dim`. \cr
#' One cannot give an empty subscript;
#' instead fill in something like `seq_len(dim(x)[margin])`. \cr
#' NOTE: The `coord2ss()` function does not support duplicate subscripts.
#' @param coord an integer matrix, giving the coordinate indices (subscripts) to convert. \cr
#' Each row is an index, and each column is the dimension. \cr
#' The first columns corresponds to the first dimension,
#' the second column to the second dimensions,
#' etc. \cr
#' The number of columns of `coord` must be equal to the length of `x.dim`. \cr
#' @param x.dim an integer vector giving the dimensions of the array in question. I.e. `dim(x)`.
#' @param ind an integer vector, giving the interior position indices to convert.
#' @param checks Boolean, indicating if arguments checks should be performed. \cr
#' Defaults to `TRUE`. \cr
#' Can be set to `FALSE` for minor speed improvements. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' 
#' @details
#' The base S3 vector and array classes in 'R' use the standard Linear Algebraic convention,
#' as in academic fields like Mathematics and Statistics,
#' in the following sense: \cr
#'  * vectors are \bold{column} vectors (i.e. vertically aligned vectors);
#'  * index counting starts at `1`;
#'  * rows are the first dimension/subscript, columns are the second dimension/subscript, etc.
#' 
#' Thus, the orientation of interior indices in, for example,
#' a 4-rows-by-5-columns matrix, is as follows:
#' 
#' ```{r echo = FALSE, eval = TRUE, comment = NA}
#' matrix(1:20, ncol = 5)
#' ```
#' 
#' So in a 4 by 5 matrix, subscript `[1, 2]` corresponds to interior index `5`. \cr
#' Array subscripting in 'squarebrackets' also follows this standard convention. \cr
#' \cr
#' 
#' 
#' @note
#' These functions were not specifically designed for duplicate indices per-sé. \cr
#' For efficiency, they do not check for duplicate indices either. \cr 
#' 
#'
#' @returns
#' For `ss2coord()` and `ii2coord()`: \cr
#' Returns an integer matrix of coordinates
#' (with properties as described in argument `coord`). \cr
#' \cr
#' For `coord2ii()`: \cr
#' Returns an numeric vector of interior indices
#' (with properties as described in argument `ind`). \cr
#' \cr
#' For `coord2ss()`: \cr
#' Returns a list of integer subscripts
#' (with properties as described in argument `sub`) \cr
#' \cr
#' For `ss2ii()`: \cr
#' Returns an integer vector of interior indices(if `prod(x.dim) < (2^31 - 1)`),
#' or an numeric vector of interior indices (if `prod(x.dim) >= (2^31 - 1)`). \cr
#' \cr
#' 
#'
#'
#' @example inst/examples/ss2ii.R
#' 


#' @name ss2ii
NULL

#' @rdname ss2ii
#' @export
ss2coord <- function(sub, x.dim) {
  n <- length(x.dim)
  if (length(sub) != n) {
    stop("`length(sub) != length(x.dim)`")
  }
  ns <- collapse::vlengths(sub)
  total <- prod(ns)
  reps_each <- cumprod(c(1, ns))[1:n]
  reps_whole <- total/(ns * reps_each)
  coord <- .rcpp_sub2coord(sub, total, n, ns, reps_each, reps_whole)
  return(coord)
}


#' @rdname ss2ii
#' @export
coord2ss <- function(coord) {
  sub <- data.table::as.data.table(coord) |> as.list()
  names(sub) <- NULL
  sub <- lapply(sub, collapse::funique)
  return(sub)
}


#' @rdname ss2ii
#' @export
coord2ii <- function(coord, x.dim, checks = TRUE) {
  n <- length(x.dim)
  
  if(checks) {
    if(n == 0L) {
      stop("`length(x.dim) == 0`")
    }
    
    if(!is.numeric(x.dim) || !is.numeric(coord)) {
      stop("`x.dim` and `coord` must both be numeric")
    }
    
    if(!isTRUE(collapse::fncol(coord) == n)) {
      stop("`ncol(coord) != length(x.dim)`")
    }
  }
  
  ind2 <- coord[, 1L, drop = TRUE]
  
  return(.rcpp_coord2ind(ind2, coord, x.dim))
}


#' @rdname ss2ii
#' @export
ii2coord <- function(ind, x.dim) {
  return(arrayInd(ind, x.dim, useNames = FALSE))
}


#' @rdname ss2ii
#' @export
ss2ii <- function(sub, x.dim, checks = TRUE) {
  
  n <- length(x.dim)
  
  ind <- !vapply(sub, is.integer, logical(1L))
  if(length(ind) > 0L) {
    sub[ind] <- lapply(sub[ind], as.integer)
  }
  
  
  if(checks) {
    if(n == 0L) {
      stop("`length(x.dim) == 0`")
    }
    
    if(length(sub) != n) {
      stop("`length(sub) != length(x.dim)`")
    }
  }
  
  if(n == 1L) {
    return(sub[[1L]])
  }
  else if(n <= 16L) {
    if(prod(x.dim) < (2^31 - 1)) {
      return(.ss2ii_d32(sub, x.dim))
    }
    else {
      return(.ss2ii_d64(sub, x.dim))
    }
  }
  else {
    if(prod(x.dim) < (2^31 - 1)) {
      return(.ss2ii_general32(sub, x.dim))
    }
    else {
      return(.ss2ii_general64(sub, x.dim))
    }
  }
}

#' @keywords internal
#' @noRd
.ss2ii_d32 <- function(sub, x.dim) {
  n <- length(x.dim)
  dimcumprod <- as.double(cumprod(x.dim)[1L:(n - 1L)])
  return(.rcpp_sub2ind_d_32(sub, dimcumprod))
}

#' @keywords internal
#' @noRd
.ss2ii_d64 <- function(sub, x.dim) {
  n <- length(x.dim)
  dimcumprod <- as.double(cumprod(x.dim)[1L:(n - 1L)])
  return(.rcpp_sub2ind_d_64(sub, dimcumprod))
}



#' @keywords internal
#' @noRd
.ss2ii_general32 <- function(sub, x.dim) {
  n <- length(x.dim)
  ns <- collapse::vlengths(sub)
  total <- prod(ns)
  reps_each <- cumprod(c(1L, ns))[1L:n]
  reps_whole <- total/(ns * reps_each)
  dimcumprod <- cumprod(x.dim)[1L:(n - 1L)]
  
  return(.rcpp_sub2ind_general32(
    sub, total, reps_each, reps_whole, as.integer(x.dim), as.integer(dimcumprod)
  ))
}

#' @keywords internal
#' @noRd
.ss2ii_general64 <- function(sub, x.dim) {
  n <- length(x.dim)
  ns <- collapse::vlengths(sub)
  total <- prod(ns)
  reps_each <- cumprod(c(1L, ns))[1L:n]
  reps_whole <- total/(ns * reps_each)
  dimcumprod <- cumprod(x.dim)[1L:(n - 1L)]
  
  return(.rcpp_sub2ind_general64(
    sub, total, reps_each, reps_whole, as.integer(x.dim), dimcumprod
  ))
}
