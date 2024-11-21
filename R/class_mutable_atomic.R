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
#'  * `mutable_atomic()`: create a `mutable_atomic` object from given data.
#'  * `couldb.mutable_atomic()`: checks if an object could become `mutable_atomic`. \cr
#' An objects can become `mutable_atomic` if it is one of the following types: \cr
#' \link{logical}, \link{integer}, \link{double}, \link{character}, \link{complex}, \link{raw}. \cr
#' \code{bit64::}\link[bit64]{integer64} type is also supported,
#' since it is internally defined as \link{double}. \cr
#'  * `typecast.mutable_atomic()` type-casts and possibly reshapes a (mutable) atomic object,
#'  and returns a `mutable_atomic` object. \cr
#'  Does not preserve dimension names if dimensions are changed. \cr \cr
#'
#'
#'
#' @param x an atomic object.
#' @param data atomic vector giving data to fill the `mutable_atomic` object.
#' @param value see \link[base]{Extract}.
#' @param names,dim,dimnames see \link[stats]{setNames} and \link[base]{array}.
#' @param type a string giving the type; see \link[base]{typeof}.
#' @param dims integer vector, giving the new dimensions.
#' @param use.names Boolean, indicating if \link[base]{names} should be preserved.
#' @param ... method dependent arguments.
#' 
#' 
#' @section Warning: 
#' Always use
#' the exported functions given by 'squarebrackets'
#' to create a `mutable_atomic` object,
#' as they make necessary checks. \cr
#' Circumventing these checks may break things! \cr
#' \cr
#' 
#' 
#' @returns
#' For `mutable_atomic()`, `as.mutable_atomic()`, `typecast.mutable_atomic()`: \cr
#' Returns a `mutable_atomic` object. \cr
#' \cr
#' For `is.mutable_atomic()`: \cr
#' Returns `TRUE` if the object is `mutable_atomic`,
#' and returns `FALSE` otherwise. \cr
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
  
  y <- as.vector(data)
  dim(y) <- dim
  dimnames(y) <- dimnames
  names(y) <- names
  
  if(.C_is_altrep(y)) {
    y <- .internal_materialize(y)
  }
  
  .internal_set_ma(y)
  
  return(y)
  
}

#' @rdname class_mutable_atomic
#' @export
as.mutable_atomic <- function(x, ...) {
  UseMethod("as.mutable_atomic", x)
}

#' @rdname class_mutable_atomic
#' @export
as.mutable_atomic.default <- function(x, ...) {
  if(!couldb.mutable_atomic(x)) {
    stop("not atomic")
  }
  if(is.mutable_atomic(x)) {
    return(x)
  }
  
  y <- x
  if(.C_is_altrep(y)) {
    y <- .internal_materialize(y)
  }
  
  y <- data.table::copy(y)
  
  .internal_set_ma(y)
  .internal_ma_set_DimsAndNames(y, names(x), dim(x), dimnames(x))
  
  return(y)
  
}


#' @rdname class_mutable_atomic
#' @export
is.mutable_atomic <- function(x) {
  
  if(!couldb.mutable_atomic(x)) return(FALSE)
  check_protected <- collapse::anyv(
    .pkgenv_squarebrackets[["protected"]],
    .rcpp_address(x)
  )
  if(check_protected) return(FALSE)
  
  # Note: cannot check for altrep,
  # since things like logical vectors are stored as altrep during package testing
  
  check <- .rcpp_is_ma(x)
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
typecast.mutable_atomic <- function(x, type = typeof(x), dims = dim(x)) {
  
  if(length(x) != prod(dims)) {
    stop("dimension product does not match the length of object")
  }
  
  # set type:
  if(type == "logical") {
    y <- as.logical(x)
  }
  else if(type == "integer") {
    y <- as.integer(x)
  }
  else if(type == "double") {
    y <- as.double(x)
  }
  else if(type == "character") {
    y <- as.character(x)
  }
  else if(type == "complex") {
    y <- as.complex(x)
  }
  else if(type == "raw") {
    y <- as.raw(x)
  }
  else {
    stop("unsupported type")
  }
  
  # set dimensions:
  if(!is.null(dims)) {
    if(length(x) == prod(dims)) {
      data.table::setattr(y, "dim", dims)
    }
  }
  
  # convert:
  .internal_set_ma(y)
  
  # set names:
  if(!is.null(names(x))) {
    nms <- data.table::copy(names(x)) # protection against pass-by-reference
    data.table::setattr(y, "names", NULL)
    data.table::setattr(y, "names", nms)
  }
  if(!is.null(dimnames(x)) && all(dim(x) == dim(y))) {
    nms <- data.table::copy(dimnames(y)) # protection against pass-by-reference
    data.table::setattr(y, "dimnames", NULL)
    data.table::setattr(y, "dimnames", nms)
  }
  
  return(y)
  
}


#' @rdname class_mutable_atomic
#' @export
c.mutable_atomic <- function(..., use.names = TRUE) {
  y <- unlist(list(...), recursive = FALSE, use.names = use.names)
  .internal_set_ma(y)
  return(y)
}



#' @rdname class_mutable_atomic
#' @export
`[.mutable_atomic` <- function(x, ...) {
  y <- NextMethod("[")
  
  if(!inherits(y, "mutable_atomic")) {
    class(y) <- c("mutable_atomic", class(y))
  }
  
  attr(y, "serial") <- .C_serial(y)
  y
}


#' @rdname class_mutable_atomic
#' @export
`[<-.mutable_atomic` <- function(x, ..., value) {
  
  oldtype <- typeof(x)
  
  oc <- oldClass(x)
  class(x) <- NULL
  x[...] <- value
  class(x) <- oc
  
  newtype <- typeof(x)
  if(oldtype != newtype) {
    message(sprintf("coercing type from `%s` to `%s`", oldtype, newtype))
    attr(x, "serial") <- .C_serial(x)
  }
  
  x
}


#' @rdname class_mutable_atomic
#' @export
format.mutable_atomic <- function(x, ...) {
  class(x) <- setdiff(class(x), "mutable_atomic")
  attr(x, "serial") <- NULL
  format(x, ...)
}


#' @rdname class_mutable_atomic
#' @export
print.mutable_atomic <- function(x, ...) {
  class(x) <- setdiff(class(x), "mutable_atomic")
  attr(x, "serial") <- NULL
  print(x, ...)
  cat("mutable_atomic \n")
  cat(paste("typeof: ", typeof(x), "\n"))
}
