#' Developer Functions for the mutatomic Class
#'
#' @description
#' The `stopifnot_ma_safe2mutate()` function
#' checks if an atomic object is actually safe to mutate. \cr
#' The `.internal_set_ma()` function
#' sets an object to class 'mutatomic' by reference. \cr
#' 
#'  
#' @param x atomic object
#' @param sym the symbol of the object; i.e. `substitute(x)`.
#' @param envir the environment where the object resides; i.e. `parent.frame(n = 1)`.
#' @param .abortcall environment where the error message is passed to.
#' 
#' 
#' @returns
#' Nothing. Only gives an error if the object is not safe to mutate.
#' 
#' 
#' @example inst/examples/mutatomic_developer.R
#' 
#' 

#' @rdname mutatomic_developer
#' @export
stopifnot_ma_safe2mutate <- function(sym, envir, .abortcall) {
  
  .check_bindingIsLocked(sym, envir, .abortcall)
  x <- get(sym, envir = envir)
  if(!is.atomic(x)) {
    txt <- paste0("'", sym, "' is not atomic")
    stop(simpleError(txt, call = .abortcall))
  }
  if(!is.mutatomic(x)) {
    txt <- paste0("'", sym, "' is not a 'mutatomic' object")
    stop(simpleError(txt, call = .abortcall))
  }
  
}


#' @rdname mutatomic_developer
#' @export
address <- function(x) {
  .rcpp_address(x)
}

#' @rdname mutatomic_developer
#' @export
.internal_set_ma <- function(x) {
  if(identical(parent.frame(n = 1L), globalenv())) {
    stop("DO NOT call this function!!!")
  }
  if(!couldb.mutatomic(x)) {
    stop("not atomic or not convertible")
  }
  
  
  if(!inherits(x, "mutatomic")) {
    newclass <- c("mutatomic", oldClass(x))
  }
  else {
    newclass <- oldClass(x)
  }
  
  .rcpp_set_ma(x, newclass)
  
  return(invisible(NULL))
}

