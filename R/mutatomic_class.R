#' A Class of Mutable Atomic Array-Like Objects
#'
#' @description
#' The `mutatomic` class is a class of mutable atomic array-like objects. \cr
#' It works exactly the same in all aspects as regular atomic vectors and arrays. \cr
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
#'
#'
#' @param x an atomic object.
#' @param data atomic vector giving data to fill the `mutatomic` object.
#' @param names,dim,dimnames,comment see their respective help pages. 
#' @param ... method dependent arguments.
#' 
#' 
#' @details
#' \bold{`couldb.mutatomic()`}: \cr
#' The `couldb.mutatomic()` function checks if an object can become a `mutatomic` object. \cr
#' Only objects with the following properties can become `mutatomic`: \cr
#' 
#'  - \bold{`atomic` data type}: \cr
#'  The data type of the object is `atomic`. \cr
#'  I.e. \link{raw}, \link{logical}, \link{integer}, \link{double}, \link{complex}, or \link{character}. \cr
#'  Thus recursive objects, like lists, and S4 objects, and so on cannot become `mutatomic`. \cr
#'  `ALTREP` objects will be materialized when coerced to `mutatomic`.
#'  - \bold{length equals number of elements}: \cr
#'  The length of the object equals the number of elements. \cr
#'  This is the case for the vast majority of objects. \cr
#'  But some objects, like the various classes of the 'bit' package,
#'  break this principle and are thus not compatible with `mutatomic`. \cr
#'  - \bold{array-like}: \cr
#'  The object is "array-like",
#'  meaning that the object is a vector where one can \emph{meaningfully} set, remove, or modify the dimensions,
#'  without breaking its functionality. \cr
#'  Thus a factor
#'  (which already represents a dummy matrix),
#'  and most data-time objects (which are often complex classes with temporal dimensions)
#'  cannot be `mutatomic`. \cr
#'  - \bold{consistent with base 'R'}: \cr
#'  For example, `integer64` has a different definition of `NA` than base 'R',
#'  which is not supported by the `mutatomic` class. \cr
#'  \cr
#' 
#' 
#' @section Footnotes: 
#' - Always use
#' the exported functions given by 'squarebrackets'
#' to create a `mutatomic` object,
#' as they make necessary safety checks.
#' - Mutable objects that can only exist as vectors can be modified via functions from the 'data.table' or 'collapse' packages.
#' - Mutable date-time objects already exist in the form of S4 classes, and need no coverage from the 'mutatomic' class.
#' - While one can force dimension attributes upon factors and base R's date-time objects,
#' this may break or corrupt it's interactions with methods
#' (like the `[` and `print()` methods),
#' coercions (like `as.data.frame()`), and array manipulators (like `aperm()`). \cr
#' 'squarebrackets' does not allow such sloppiness,
#' and thus strictly consider factors and date-time objects to be vectors but not 'array-like'. \cr
#' - It would not make much sense to make factors dimensional,
#' since they are primarily used to represent a dummy matrix for statistical modelling. \cr
#' A dimensional array of categorical values that's not used for modelling
#' is better off being represented as either character array or integer array. \cr \cr
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
#' Returns `TRUE` if the object can be converted to `mutatomic`. \cr
#' Returns `FALSE` otherwise. \cr \cr
#'
#'
#' @example inst/examples/class_mutatomic.R
#' 


#' @name mutatomic_class
NULL



#' @rdname mutatomic_class
#' @export
mutatomic <- function(data, names = NULL, dim = NULL, dimnames = NULL, comment = NULL) {
  
  if(!couldb.mutatomic(data)) {
    stop("non-atomic or non-convertible data given")
  }
  
  y <- as.vector(data)
  dim(y) <- dim
  dimnames(y) <- dimnames
  names(y) <- names
  comment(y) <- comment
  
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
  
  
  if(.C_is_altrep(x)) {
    altrep_class <- as.character(.C_altrep_attr(x)[[1]])
    if(altrep_class %in% c("compact_intseq", "compact_realseq")) {
      return(FALSE)
    }
  }
  
  check <- .rcpp_is_ma(x)
  return(check)
  
}


#' @rdname mutatomic_class
#' @export
couldb.mutatomic <- function(x) {
  check_atomic <- is.logical(x) || is.integer(x) || is.double(x) || is.character(x) || is.complex(x) || is.raw(x)
  check_len <- .C_n_elements(x) == length(x)
  check_array_like <- .is.array_like(x)
  check_consistent <- .is.baseconsistent(x)
  check_special <- !is.null(x) && !isS4(x)
  return(
    check_atomic && check_len && check_array_like && check_consistent && check_special
  )
}


#' @keywords internal
#' @noRd
.is.array_like <- function(x) {
  if(is.array(x)) return(TRUE)
  if(is.factor(x) || .is.datetime(x)) return(FALSE)
  return(TRUE)
}


#' @keywords internal
#' @noRd
.is.datetime <- function(x) {
  x.classes <- tolower(class(x))
  datetime.classes <- c("Date", "datetime", "POSIXct", "POSIXlt", "difftime", "ts")
  out <- any(x.classes %in% tolower(datetime.classes))
  return(out)
}

#' @keywords internal
#' @noRd
.is.baseconsistent <- function(x) {
  !inherits(x, "integer64")
}

#' @importFrom methods setOldClass
setOldClass("mutatomic")

