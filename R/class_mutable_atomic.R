#' Mutable Atomic Classes
#'
#' @description
#' The `mutable_atomic` class is a mutable version of atomic classes. \cr
#' It works exactly the same in all aspects as regular atomic classes,
#' with only one real difference: \cr
#' The 'squarebrackets' methods and functions that perform modification by reference
#' (basically all methods and functions with "set" in the name)
#' accept `mutable_atomic`,
#' but do not accept regular `atomic`. \cr
#' See \link{squarebrackets_PassByReference} for details. \cr
#' \cr
#' Like `data.table`, `[<-` performs R's default copy-on-modification semantics. \cr
#' For modification  by reference, use \link{sb_set}. \cr
#' \cr
#' Exposed functions (beside the S3 methods):
#' 
#'  * `mutable_atomic()`: create a `mutable_atomic` object.
#'  * `is.mutable_atomic()`: checks if an object is atomic.
#'  * `as.mutable_atomic()`: converts a regular atomic object to `mutable_atomic`.
#'  * `couldb.mutable_atomic()`: checks if an object could be `mutable_atomic`. \cr
#' An objects can become `mutable_atomic` if it is one of the following types: \cr
#' \link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
#' \code{bit64::}\link[bit64]{integer64} type is also supported,
#' since it is internally defined as \link{double}. \cr \cr
#'
#' @param x an atomic object.
#' @param data atomic vector giving data to fill the `mutable_atomic` object.
#' @param value see \link[base]{Extract}.
#' @param names,dim,dimnames see \link[stats]{setNames} and \link[base]{array}.
#' @param ... method dependent arguments.
#' 
#' @section Warning: 
#' 
#' Always use `mutable_atomic()` or `as.mutable_atomic()` to create a mutable object,
#' as they make necessary checks. \cr
#' Circumventing these checks may break things. \cr \cr
#' 
#' 
#' @returns
#' For `mutable_atomic()`: \cr
#' Returns a `mutable_atomic` object. \cr
#' \cr
#' For `as.mutable_atomic()`: \cr
#' Converts an atomic object (vector, matrix, array)
#' to the same object, but with additional class `"mutable_atomic"`,
#' and the additional attribute `"typeof"`. \cr
#' \cr
#' For `is.mutable_atomic()`: \cr
#' Returns `TRUE` if the object is atomic, has
#' the class `"mutable_atomic"`,
#' has the correctly set attribute `"typeof"`,
#' \bold{and} has an address that does not overlap with the addresses of base objects. \cr
#' `is.mutable_atomic()` returns `FALSE` otherwise. \cr
#' \cr
#' For `couldb.mutable_atomic()`: \cr
#' Returns `TRUE` if the object is one of the following types: \cr
#' \link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
#' \code{bit64::}\link[bit64]{integer64} type is also supported,
#' since it is internally defined as \link{double}. \cr
#' Returns `FALSE` otherwise. \cr \cr
#'
#'
#' @example inst/examples/class_mutable_atomic.R
#' 


#' @name class_mutable_atomic
NULL



#' @rdname class_mutable_atomic
#' @export
mutable_atomic <- function(data, names = NULL, dim = NULL, dimnames = NULL) {
  
  if(!couldb.mutable_atomic(data)) {
    stop("non-atomic data given")
  }
  
  if(!is.null(names)) {
    names <- data.table::copy(names) # protection against pass-by-reference
  }
  if(!is.null(dimnames)) {
    dimnames <- data.table::copy(dimnames) # protection against pass-by-reference
  }
  
  if(isTRUE(length(dim) == 2)) {
    x <- structure(
      as.vector(data), class = c("mutable_atomic", "matrix", "array"), typeof = typeof(data),
      names = names, dim = dim, dimnames = dimnames
    )
    return(x)
  }
  
  if(isTRUE(length(dim) > 2)) {
    x <- structure(
      as.vector(data), class = c("mutable_atomic", "array"), typeof = typeof(data),
      names = names, dim = dim, dimnames = dimnames
    )
    return(x)
  }
  
  x <- structure(
    data, class = c("mutable_atomic", class(data)), typeof = typeof(data),
    names = names, dim = dim, dimnames = dimnames
  )
  return(x)
  
}


#' @rdname class_mutable_atomic
#' @export
as.mutable_atomic <- function(x, ...) {
  if(!couldb.mutable_atomic(x)) {
    stop("not atomic")
  }
  if(is.mutable_atomic(x)) {
    return(x)
  }
  
  y <- data.table::copy(x)
  attr(y, "typeof") <- typeof(x)
  class(y) <- c("mutable_atomic", class(y))
  
  if(!is.null(names(y))) {
    nms <- data.table::copy(names(y)) # protection against pass-by-reference
    names(y) <- NULL
    names(y) <- nms
  }
  if(!is.null(dimnames(y))) {
    nms <- data.table::copy(dimnames(y)) # protection against pass-by-reference
    dimnames(y) <- NULL
    dimnames(y) <- nms
  }
  
  return(y)
}


#' @rdname class_mutable_atomic
#' @export
is.mutable_atomic <- function(x) {
  
  if(!couldb.mutable_atomic(x)) return(FALSE)
  check_protected <- data.table::`%chin%`(
    .rcpp_address(x),
    getOption("squarebrackets.protected", default = .protected_addresses())
  )
  if(check_protected) return(FALSE)
  check <- inherits(x, "mutable_atomic") && isTRUE(attr(x, "typeof") == typeof(x))
  return(check)
  
}

#' @rdname class_mutable_atomic
#' @export
couldb.mutable_atomic <- function(x) {
  check1 <- is.logical(x) || is.integer(x) || is.double(x) || is.character(x) || is.complex(x) || is.raw(x)
  check2 <- !is.null(x) && !is.factor(x)
  return(
    check1 && check2
  )
}


#' @rdname class_mutable_atomic
#' @export
`[.mutable_atomic` <- function(x, ...) {
  y <- NextMethod("[")
  attr(y, "typeof") <- typeof(x)
  class(y) <- c("mutable_atomic", class(y))
  y
}


#' @rdname class_mutable_atomic
#' @export
`[<-.mutable_atomic` <- function(x, ..., value) {
  
  if(getOption("squarebrackets.ma_messages", TRUE)) {
    message("copying on modification; for modification by reference, use `sb_set()`")
  }
  
  oc <- oldClass(x)
  class(x) <- NULL
  x[...] <- value
  attr(x, "typeof") <- typeof(x)
  class(x) <- oc
  x
}


#' @rdname class_mutable_atomic
#' @export
format.mutable_atomic <- function(x, ...) {
  class(x) <- setdiff(class(x), "mutable_atomic")
  format(x, ...)
}


#' @rdname class_mutable_atomic
#' @export
print.mutable_atomic <- function(x, ...) {
  class(x) <- setdiff(class(x), "mutable_atomic")
  attr(x, "typeof") <- NULL
  print(x, ...)
  cat("mutable_atomic \n")
  cat(paste("typeof: ", typeof(x), "\n"))
}
