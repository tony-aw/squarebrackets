


#' @rdname mutatomic_class
#' @export
couldb.mutatomic <- function(x) {
  
  check <- is.logical(x) || is.integer(x) || is.double(x) || is.character(x) || is.complex(x) || is.raw(x)
  if(!check) return(FALSE)
  
  check <- .C_n_elements(x) == length(x)
  if(!check) return(FALSE)
  
  if(any(dim(x) != attr(x, "dim"))) {
    return(FALSE)
  }
  
  check <- TRUE
  if(!is.null(dim(x))) {
    check <- prod(dim(x)) == .C_n_elements(x)
  }
  if(!check) return(FALSE)
  
  check <- .is.array_like(x)
  if(!check) return(FALSE)
  
  if(.is.table(x)) return(FALSE)
  
  check <- .is.baseconsistent(x)
  if(!check) return(FALSE)
  
  check <- !is.null(x) && !isS4(x)
  if(!check) return(FALSE)
  
  return(TRUE)
}


#' @keywords internal
#' @noRd
.is.array_like <- function(x) {
  if(is.factor(x) || .is.datetime(x) || .is.onlyvector(x)) {
    return(FALSE)
  }
  if(!is.null(dim(x)) && !is.array(x)) {
    # it's dimensions are not NULL, yet it's not an array
    return(FALSE)
  }
  if(!is.null(attr(x, "dim")) && !is.array(x)) {
    # it's dim attribute is not NULL, yet it's not an array
    return(FALSE)
  }
  return(TRUE)
}


#' @keywords internal
#' @noRd
.is.datetime <- function(x) {
  x.classes <- class(x)
  datetime.classes <- c("Date", "datetime", "POSIXct", "POSIXlt", "ts")
  out <- any(x.classes %in% datetime.classes)
  return(out)
}


#' @keywords internal
#' @noRd
.is.table <- function(x) {
  return(is.table(x) || inherits(x, "ftable"))
}


#' @keywords internal
#' @noRd
.is.onlyvector <- function(x) {
  x.classes <- class(x)
  onlyvector.classes <- c("roman", "octmode", "hexmode")
  out <- any(x.classes %in% onlyvector.classes)
  return(out)
}


#' @keywords internal
#' @noRd
.is.baseconsistent <- function(x) {
  
  if(is.raw(x)) {
    return(TRUE)
  }
  
  myNA <- .C_make_NA(x)
  class(myNA) <- oldClass(x)
  return(is.na(myNA))
  
}

