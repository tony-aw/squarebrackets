#' Mutable Atomic Classes
#'
#' @description
#' The `mutable_atomic` class is a mutable version of atomic classes. \cr
#' It works exactly the same in all aspects as regular atomic classes,
#' with only one real difference: \cr
#' \link{sb_set} accepts `mutable_atomic`, but does not accept regular `atomic`. \cr
#' See \link{squarebrackets_PassByReference} for details. \cr
#' \cr
#' Like `data.table`, `[<-` performs R's default copy-on-modification semantics. \cr
#' For modification  by reference, use \link{sb_set}. \cr
#' \cr
#' The `is.mutable_atomic()` function checks if an object is atomic.
#' 
#'
#' @param x an atomic object.
#' @param data atomic vector giving data to fill the `mutable_atomic` object.
#' @param value see \link[base]{Extract}.
#' @param names,dim,dimnames see \link[stats]{setNames} and \link[base]{array}.
#' @param ... method dependent arguments.
#' 
#' @section Warning: 
#' 
#' Always use `mutable_atomic()` or `as.mutable_atomic` to create a mutable object. \cr
#' Do NOT attempt to manually create a mutable atomic object by tweaking attributes: \cr
#' `mutable_atomic()` and `as.mutable_atomic` make some necessary checks so that other functions,
#' such as \link{sb_set},
#' don't have to spend computation time to perform these checks also. \cr
#' Circumventing these checks may break things. \cr \cr
#' 
#' 
#' @returns
#' `as.mutable_atomic` converts an atomic object (vector, matrix, array)
#' to the same object, but with additional class `"mutable_atomic"`,
#' and the additional attribute `"typeof"`. \cr
#' \cr
#' `is.mutable_atomic` returns `TRUE` if the object is atomic, has
#' the class `"mutable_atomic"`, \bold{and} has the correctly set attribute `"typeof"`; \cr
#' `is.mutable_atomic` returns `FALSE` otherwise.
#'
#'
#' @example inst/examples/class_mutable_atomic.R
#' 


#' @name class_mutable_atomic
NULL



#' @rdname class_mutable_atomic
#' @export
mutable_atomic <- function(data, names = NULL, dim = NULL, dimnames = NULL) {
  
  if(!is.atomic(data)) {
    stop("non-atomic data given")
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
  if(!is.atomic(x)) {
    stop("not atomic")
  }
  if(is.mutable_atomic(x)) {
    return(x)
  }
  
  y <- x
  attr(y, "typeof") <- typeof(x)
  class(y) <- c("mutable_atomic", class(y))
  return(y)
}


#' @rdname class_mutable_atomic
#' @export
is.mutable_atomic <- function(x) {
  
  if(!is.atomic(x)) return(FALSE)
  check1 <- !data.table::`%chin%`(.rcpp_address(x), getOption("squarebrackets.protected"))
  check2 <- inherits(x, "mutable_atomic") && isTRUE(attr(x, "typeof") == typeof(x))
  return(check1 && check2)
  
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
  
  message("copying on modification; for modification by reference, use `sb_set()`")
  
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


#' @keywords internal
#' @noRd
.protected_addresses <- function() {
  tempfun <- function(x) {
    if(!is.function(x)) {
      return(.rcpp_address(x))
    }
  }
  lst <- eapply(baseenv(), tempfun, all.names = TRUE, USE.NAMES = TRUE)
  lst <- lst[sapply(lst, \(x)!is.null(x))]
  protected_bnds <- sapply(
    names(lst), \(x) bindingIsLocked(x, env = baseenv()) || bindingIsActive(x, env = baseenv())
  )
  lst <- lst[protected_bnds]
  lst <- lst[!names(lst) %in% c(".Last.value", "Last.value")]
  
  return(unlist(lst))
}
