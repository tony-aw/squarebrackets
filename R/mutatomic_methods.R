
#' @export
as.logical.mutatomic <- function(x, ...) {
  out <- as.logical(unclass(x))
  .mutatomic_typecast(out) <- x
  return(out)
}


#' @export
as.integer.mutatomic <- function(x, ...) {
  out <- as.integer(unclass(x))
  .mutatomic_typecast(out) <- x
  return(out)
}


#' @export
as.double.mutatomic <- function(x, ...) {
  out <- as.double(unclass(x))
  .mutatomic_typecast(out) <- x
  return(out)
}



#' @export
as.complex.mutatomic <- function(x, ...) {
  out <- as.complex(unclass(x))
  .mutatomic_typecast(out) <- x
  return(out)
}



#' @export
as.character.mutatomic <- function(x, ...) {
  out <- as.character(unclass(x))
  .mutatomic_typecast(out) <- x
  return(out)
}


#' @export
as.raw.mutatomic <- function(x, ...) {
  out <- as.raw(unclass(x))
  .mutatomic_typecast(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
`.mutatomic_typecast<-` <- function(x, value) {
  if(!is.mutatomic(value)) {
    x
  }
  else {
    dim(x) <- dim(value)
    dimnames(x) <- dimnames(value)
    names(x) <- names(value)
    class(x) <- unique(c("mutatomic", oldClass(value)))
    attr(x, "serial") <- .C_serial(x)
    x
  }
  
}


#' @export
c.mutatomic <- function(..., use.names = TRUE) {
  y <- unlist(list(...), recursive = FALSE, use.names = use.names)
  .internal_set_ma(y)
  return(y)
}


#' @export
`[.mutatomic` <- function(x, ...) {
  
  if(!is.mutatomic(x)) {
    stop("malformed mutatomic")
  }
  y <- NextMethod("[")
  class(y) <- oldClass(x)
  attr(y, "serial") <- .C_serial(y)
  y
}


#' @export
`[[.mutatomic` <- function(x, ...) {
  
  if(!is.mutatomic(x)) {
    stop("malformed mutatomic")
  }
  y <- NextMethod("[[")
  class(y) <- oldClass(x)
  attr(y, "serial") <- .C_serial(y)
  y
}


#' @export
`[<-.mutatomic` <- function(x, ..., value) {
  
  if(!is.mutatomic(x)) {
    stop("malformed mutatomic")
  }
  
  oldtype <- typeof(x)
  
  oc <- oldClass(x)
  class(x) <- NULL
  x[...] <- value
  class(x) <- oc
  
  newtype <- typeof(x)
  if(oldtype != newtype) {
    message(sprintf("coercing type from `%s` to `%s`", oldtype, newtype))
  }
  
  attr(x, "serial") <- .C_serial(x)
  x
}


#' @export
`[[<-.mutatomic` <- function(x, ..., value) {
  
  if(!is.mutatomic(x)) {
    stop("malformed mutatomic")
  }
  
  oldtype <- typeof(x)
  
  oc <- oldClass(x)
  class(x) <- NULL
  x[[...]] <- value
  class(x) <- oc
  
  newtype <- typeof(x)
  if(oldtype != newtype) {
    message(sprintf("coercing type from `%s` to `%s`", oldtype, newtype))
  }
  
  attr(x, "serial") <- .C_serial(x)
  x
}


#' @export
format.mutatomic <- function(x, ...) {
  
  if(!is.mutatomic(x)) {
    stop("malformed mutatomic")
  }
  
  class(x) <- setdiff(class(x), "mutatomic")
  attr(x, "serial") <- NULL
  format(x, ...)
}


#' @export
print.mutatomic <- function(x, ...) {
  
  if(!is.mutatomic(x)) {
    stop("malformed mutatomic")
  }
  
  class(x) <- setdiff(class(x), "mutatomic")
  attr(x, "serial") <- NULL
  print(x, ...)
  cat("mutatomic \n")
  cat(paste("typeof: ", typeof(x), "\n"))
}


