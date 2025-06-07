#' A Class of Mutable Atomic Objects
#'
#' @description
#' The `mutatomic` class is a mutable version of atomic classes. \cr
#' It works exactly the same in all aspects as regular atomic classes. \cr
#' There is only one real difference: \cr
#' Pass-by-reference functions in 'squarebrackets'
#' only accept atomic objects when they are of class `mutatomic`,
#' for greater safety. \cr
#' In all other aspects, `mutatomic` objects are the same as R's regular atomic objects,
#' including the behaviour of the `[<-` operator . \cr
#' \cr
#' Exposed functions (beside the S3 methods):
#' 
#'  * `mutatomic()`: create a `mutatomic` object from given data.
#'  * `couldb.mutatomic()`: checks if an object could become `mutatomic`. \cr
#' An objects can become `mutatomic` if it is one of the following types: \cr
#' \link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
#' Factors can never be `mutatomic`. \cr \cr
#'
#'
#'
#' @param x an atomic object.
#' @param data atomic vector giving data to fill the `mutatomic` object.
#' @param names,dim,dimnames see \link[stats]{setNames} and \link[base]{array}.
#' @param ... method dependent arguments.
#' 
#' 
#' @note 
#' Always use
#' the exported functions given by 'squarebrackets'
#' to create a `mutatomic` object,
#' as they make necessary safety checks. \cr
#' \cr
#' 
#' 
#' @returns
#' For `mutatomic()`, `as.mutatomic()`: \cr
#' Returns a `mutatomic` object. \cr
#' \cr
#' For `is.mutatomic()`: \cr
#' Returns `TRUE` if the object is `mutatomic`,
#' and returns `FALSE` otherwise. \cr
#' \cr
#' For `couldb.mutatomic()`: \cr
#' Returns `TRUE` if the object is one of the following types: \cr
#' \link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
#' Returns `FALSE` otherwise. \cr \cr
#'
#'
#' @example inst/examples/class_mutatomic.R
#' 


#' @name mutatomic_class
NULL



#' @rdname mutatomic_class
#' @export
mutatomic <- function(data, names = NULL, dim = NULL, dimnames = NULL) {
  
  if(!couldb.mutatomic(data)) {
    stop("non-atomic or non-convertible data given")
  }
  
  y <- as.vector(data)
  dim(y) <- dim
  dimnames(y) <- dimnames
  names(y) <- names
  
  if(.C_is_altrep(y)) {
    y <- .ma_materialize(y)
  }
  
  .internal_set_ma(y)
  
  return(y)
  
}

#' @rdname mutatomic_class
#' @export
as.mutatomic <- function(x, ...) {
  if(!couldb.mutatomic(x)) {
    stop("not atomic or not convertible")
  }
  if(is.mutatomic(x)) {
    return(x)
  }
  
  y <- x
  if(.C_is_altrep(y)) {
    return(.ma_materialize(y))
  }
  else {
    y <- .C_copy(y)
  }
  
  
  .internal_set_ma(y)

  return(y)
  
}


#' @keywords internal
#' @noRd
.ma_materialize <- function(x) {
  
  y <- vector(typeof(x), length(x))
  .rcpp_set_all_atomic(y, rp = x)
  mostattributes(y) <- attributes(x)
  
  .internal_set_ma(y)

  return(y)
}



#' @rdname mutatomic_class
#' @export
is.mutatomic <- function(x) {
  
  if(!couldb.mutatomic(x)) return(FALSE)
  check_protected <- .C_any_address(
    .pkgenv_mutatomic[["protected"]],
    .rcpp_address(x)
  )
  if(check_protected) return(FALSE)
  
  # Note: cannot check for altrep,
  # since things like logical vectors are stored as altrep during package testing
  
  check <- .rcpp_is_ma(x)
  return(check)
  
}


#' @rdname mutatomic_class
#' @export
couldb.mutatomic <- function(x) {
  check1 <- is.logical(x) || is.integer(x) || is.double(x) || is.character(x) || is.complex(x) || is.raw(x)
  check2 <- !is.null(x) && !is.factor(x)
  check3 <- !isS4(x)
  return(
    check1 && check2 && check3
  )
}


