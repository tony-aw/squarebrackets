



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


#' @importFrom methods setOldClass
setOldClass("mutatomic")

