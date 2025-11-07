#' Safely Change the Names of a Mutable Object By Reference
#'
#' @description
#' Functions to rename a
#' \link[=squarebrackets_supported_structures]{supported mutable object}
#' using
#' \link[=squarebrackets_PassByReference]{pass-by-reference semantics}:
#' 
#'  * `sb_setFlatnames()` renames the (flat) names of a `mutatomic` object. 
#'  * `sb_setDimnames()` renames the dimension names of a `mutatomic` object.
#'  * `sb2_setVarnames()` renames the variable names of a `data.table` object. \cr \cr
#' 
#' 
#'
#' @param x a \bold{variable} belonging to one of the
#' \link[=squarebrackets_supported_structures]{supported mutable classes}. \cr
#' @param i logical, numeric, character, or imaginary indices, indicating which flatnames should be changed. \cr
#' If `i = NULL`, the names will be completely replaced.
#' @param newnames Atomic character vector giving the new names. \cr
#' Specifying `NULL` will remove the names. \cr
#' @param m integer vector giving the margin(s) for which to change the names
#' (`m = 1L` for rows, `m = 2L` for columns, etc.).
#' @param newdimnames a list of the same length as `m`. \cr
#' The first element of the list corresponds to margin `m[1]`,
#' the second element to `m[2]`,
#' and so on. \cr
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
#' @param ... see \link{squarebrackets_method_dispatch}. \cr \cr
#' 
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


#' @name sb_setRename
NULL

#' @rdname sb_setRename
#' @export
sb_setFlatnames <- function(x, i = NULL, newnames, ...) {
  
  # error checks:
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  
  # Case 1: remove names (also works if `x` didn't have names in the first place)
  if(is.null(newnames)) {
    data.table::setattr(x, "names", NULL)
    return(invisible(NULL))
  }
  
  
  # Case 2: add names when `x` didn't have any
  if(!is.character(newnames)) {
    stop("`newnames` must be a character vector")
  }
  if(is.null(names(x))) {
    data.table::setattr(x, "names", data.table::copy(newnames))
    return(invisible(NULL))
  }
  
  
  # Case 3: Complete replace existing names
  if(is.null(i) && !is.null(names(x)) && !is.null(newnames)) {
    if(length(newnames) != length(x)) {
      stop("`newnames` of wrong length")
    }
    data.table::setattr(x, "names", NULL) # protecting original names
    data.table::setattr(x, "names", data.table::copy(newnames))
    return(invisible(NULL))
  }
  
  
  # Case 4: Replace subset of names
  if(!is.null(i) && !is.null(names(x)) && !is.null(newnames)) {
    nms <- names(x) # protecting original names
    i <- .sb_setrename_indx(i, nms, sys.call())
    if(length(i) != length(newnames)) {
      stop("`newnames` of wrong length")
    }
    nms[i] <- newnames
    data.table::setattr(x, "names", NULL) # protecting original names
    data.table::setattr(x, "names", nms)
    return(invisible(NULL))
  }
  
  
  stop("improper combination of arguments given")
  
}


#' @rdname sb_setRename
#' @export
sb_setDimnames <- function(x, m, newdimnames, ...) {
  
  # error checks:
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
  
  
  # remove dimnames:
  if(is.null(newdimnames)) {
    data.table::setattr(x, "dimnames", NULL)
    return(invisible(NULL))
  }
  
  # check m:
  if(length(m) > length(dim(x)) || any(m <= 0)) {
    stop("improper `m` given")
  }
  
  # check dimnames:
  if(!is.list(newdimnames) || length(newdimnames) != length(m)) {
    stop("`newdimnames` must be a list of the same length as `m`")
  }
  
  nulldims <- sapply(newdimnames, is.null)
  if(!all(nulldims)) {
    if(any(collapse::vlengths(newdimnames[!nulldims]) != dim(x)[m][!nulldims])) {
      stop("improper `newdimnames` given")
    }
    if(any(collapse::vclasses(newdimnames[!nulldims]) != "character")) {
      stop("improper `newdimnames` given")
    }
  }
  
  dimnames <- dimnames(x)
  dimnames[m] <- data.table::copy(newdimnames)
  
  data.table::setattr(x, "dimnames", NULL) # protecting original names
  data.table::setattr(x, "dimnames", dimnames)
  return(invisible(NULL))
  
}



#' @rdname sb_setRename
#' @export
sb2_setVarnames <- function(x, old, new, skip_absent = FALSE, ...) {
  if(!data.table::is.data.table(x)) {
    stop("`x` is not a (supported) mutable object")
  }
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  new <- data.table::copy(new)
  
  data.table::setnames(x, old, new, skip_absent = skip_absent)
  
  return(invisible(NULL))
  
}

#' @keywords internal
#' @noRd
.sb_setrename_indx <- function(i, nms, abortcall) {
  if(is.logical(i)) {
    return(tci_bool(i, length(nms), .abortcall = abortcall))
  }
  else if(is.numeric(i)) {
    return(tci_int(i, length(nms), .abortcall = abortcall))
  }
  else if(is.character(i)) {
    return(tci_chr(i, nms, .abortcall = abortcall))
  }
  else if(is.complex(i)) {
    return(tci_im(i, length(nms), .abortcall = abortcall))
  }
  else {
    .indx_stop(abortcall)
  }
}
