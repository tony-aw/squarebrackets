



#' @rdname mutatomic_class
#' @export
couldb.mutatomic <- function(x) {
  
  check <- is.logical(x) || is.integer(x) || is.double(x) || is.character(x) || is.complex(x) || is.raw(x)
  if(!check) return(FALSE)
  
  check <- .C_n_elements(x) == length(x)
  if(!check) return(FALSE)
  
  check <- TRUE
  if(!is.null(dim(x))) {
    check <- prod(dim(x)) == .C_n_elements(x)
  }
  if(!check) return(FALSE)
  
  check <- .is.array_like(x)
  if(!check) return(FALSE)
  
  check <- .is.baseconsistent(x)
  if(!check) return(FALSE)
  
  check <- !is.null(x) && !isS4(x)
  if(!check) return(FALSE)
  
  return(TRUE)
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
  
  if(is.raw(x)) {
    return(TRUE)
  }
  
  myNA <- .C_make_NA(x)
  class(myNA) <- oldClass(x)
  return(is.na(myNA))
  
}

#' @importFrom methods setOldClass
setOldClass("mutatomic")

