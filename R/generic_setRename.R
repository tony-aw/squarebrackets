#' Method to Change the Names of a Mutable Object By Reference
#'
#' @description
#' This is an S3 Method to rename a
#' \link[=squarebrackets_mutable_classes]{supported mutable object}
#' using
#' \link[=squarebrackets_PassByReference]{pass-by-reference}
#' semantics. \cr
#' \cr
#' This method takes extra care
#' not to modify any objects that happen to share the same address as
#' the (dim)names of `x`. \cr
#' I.e. the following code: 
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' x <- mutable_atomic(1:26)
#' names(x) <- base::letters
#' y <- x
#' sb_setRename(x, newnames = rev(names(x)))
#' 
#' ```
#' will not modify `base::letters`, even though `names(x)` shared the same address. \cr
#' Thus, `sb_setRename()` can be used safely without fearing such accidents. \cr
#' \cr
#' Use `sb_setRename(x, ...)` if `x` is a non-recursive object
#' (i.e. \link{mutable_atomic}). \cr
#' Use `sb2_setRename(x, ...)` if `x` is a recursive object
#' (i.e. \link{data.table}). \cr \cr
#' 
#'
#' @param x a \bold{variable} belonging to one of the
#' \link[=squarebrackets_mutable_classes]{supported mutable classes}. \cr
#' @param newnames atomic character vector giving the new names. \cr
#' Specifying `NULL` will remove the names.
#' @param newdimnames a list of the same length as `dim(x)`. \cr
#' The first element of the list corresponds to the first dimension,
#' the second element to the second dimension, and so on. \cr
#' The components of the list can be either `NULL`,
#' or a character vector with the same length as the corresponding dimension. \cr
#' Instead of a list,
#' simply `NULL` can be specified,
#' which will remove the `dimnames` completely.
#' @param old the old column names
#' @param new the new column names, in the same order as `old`
#' @param skip_absent Skip items in old that are missing
#' (i.e. absent) in `names(x)`. \cr
#' Default `FALSE` halts with error if any are missing.
#' @param ... further arguments passed to or from other methods. \cr \cr
#' 
#' 
#' @returns
#' Returns: VOID. This method modifies the object by reference. \cr
#' Do not use assignment like `names(x) <- sb_setRename(x, ...)`. \cr
#' Since this function returns void, you'll just get `NULL`. \cr \cr
#'
#'
#' @example inst/examples/generic_setRename.R
#' 

#' @rdname sb_setRename
#' @export
sb_setRename <- function(x, ...) {
  
  UseMethod("sb_setRename", x)
}

#' @rdname sb_setRename
#' @export
sb_setRename.default <- function(x, newnames, ...) {
  
  # error checks:
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  if(!missing(newnames)) {
    if(!is.null(newnames)) {
      if(!is.character(newnames)) {
        stop("improper `newnames` given")
      }
      if(length(newnames) != length(x)) {
        stop("improper `newnames` given")
      }
      data.table::setattr(x, "names", NULL) # protecting original names
      newnames <- data.table::copy(newnames) # protecting original names
    }
    data.table::setattr(x, "names", newnames) 
  }
  
  return(invisible(NULL))
}



#' @rdname sb_setRename
#' @export
sb_setRename.array <- function(x, newnames, newdimnames, ...) {
  
  # error checks:
  if(!is.mutable_atomic(x)){
    stop("`x` is not a (supported) mutable object")
  }
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  if(!missing(newnames)) {
    if(!is.null(newnames)) {
      if(!is.character(newnames)) {
        stop("improper `newnames` given")
      }
      if(length(newnames) != length(x)) {
        stop("improper `newnames` given")
      }
      data.table::setattr(x, "names", NULL) # protecting original names
      newnames <- data.table::copy(newnames) # protecting original names
    }
    data.table::setattr(x, "names", newnames) 
  }
  
  
  if(!missing(newdimnames)) {
    if(!is.null(newdimnames)) {
      
      if(!is.list(newdimnames)) {
        stop("improper `newdimnames` given")
      }
      if(length(newdimnames) != length(dim(x))) {
        stop("improper `newdimnames` given")
      }
      
      nulldims <- sapply(newdimnames, is.null)
      if(!all(nulldims)) {
        if(any(collapse::vlengths(newdimnames[!nulldims]) != dim(x)[!nulldims])) {
          stop("improper `newdimnames` given")
        }
        if(any(collapse::vclasses(newdimnames[!nulldims]) != "character")) {
          stop("improper `newdimnames` given")
        }
      }
      
      data.table::setattr(x, "dimnames", NULL) # protecting original names
      newdimnames <- data.table::copy(newdimnames) # protecting original names
    }
    data.table::setattr(x, "dimnames", newdimnames) 
  }
  
  return(invisible(NULL))
  
}

#' @rdname sb_setRename
#' @export
sb2_setRename <- function(x, ...) {
  
  UseMethod("sb2_setRename", x)
}


#' @rdname sb_setRename
#' @export
sb2_setRename.data.table <- function(x, old, new, skip_absent = FALSE, ...) {
  if(!data.table::is.data.table(x)) {
    stop("`x` is not a (supported) mutable object")
  }
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  new <- data.table::copy(new)
  
  data.table::setnames(x, old, new, skip_absent = skip_absent)
  
  return(invisible(NULL))
  
}



