#

#' @keywords internal
#' @noRd
.check_bindingIsLocked <- function(subx, env, abortcall) {
  if(!is.symbol(subx)) {
    stop(simpleError(
      "only existing variables can be modified by reference", call = abortcall
      ))
  }
  if(bindingIsLocked(subx, env = env)){
    stop(simpleError("object is locked", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_check_names <- function(dnames, abortcall) {
  
  if(length(dnames) == 0L) {
    stop(simpleError("`x` has no names; fix this before subsetting", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_check_logical <- function(n.indx, dlength, abortcall) {
  if(n.indx != dlength) {
    stop(simpleError("incorrect length of logical indices", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_check_int <- function(indx, dlength, abortcall) {
  if(.C_any_badindx(indx, dlength)) {
    stop(simpleError("integers must be >= 1 and <= bounds", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_stop <- function(abortcall) {
  stop(simpleError(
    "incorrect index type",
    call = abortcall
  ))
}


#' @keywords internal
#' @noRd
.indx_convert_chr <- function(indx, dnames, chkdup, inv, abortcall) {
  
  if(chkdup) {
    if(collapse::any_duplicated(indx)) {
      stop(simpleError("duplicate integers or names not allowed", call = abortcall))
    }
  }
  
  if(!inv) { return(match_all(indx, dnames)) }
  if(inv){ return(collapse::`%!iin%`(dnames, indx)) }
  
}



#' @keywords internal
#' @noRd
.indx_convert_int <- function(indx, n, chkdup, inv, abortcall) {
  
  if(chkdup) {
    if(anyDuplicated(indx)) { # base::anyDuplicated faster for numeric
      stop(simpleError("duplicate integers or names not allowed", call = abortcall))
    }
  }
  if(!inv) { return(indx) }
  if(inv) { return(seq_len(n)[-indx]) }
  
}

.indx_convert_complex <- function(indx, n, abortcall) {
  unim <- Im(indx[1])
  if(!collapse::allv(Im(indx), unim)) {
    stop(simpleError(
      "imaginary number in complex indices must be a constant", call = abortcall
    ))
  }
  if(unim < 0) {
    return(n - Re(indx) + 1L)
  }
  else {
    return(Re(indx))
  }
}

.indx_convert_complex_multi <- function(indx, n, abortcall) {
  im <- as.integer(Im(indx))
  re <- as.integer(Re(indx))
  out <- .rcpp_indx_convert_cplx_multi(re, im, as.integer(n))
  return(out)
}


#' @keywords internal
#' @noRd
.lvl2indx <- function(indx, x, chkdup, inv, abortcall) {
  
  n <- length(x)

  
  if(length(indx) == 0L) {
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(n))
  }
  
  if(chkdup) {
    if(collapse::any_duplicated(indx)) {
      stop(simpleError("duplicate integers or names not allowed", call = abortcall))
    }
  }
  
  if(!inv) { return(match_all(indx, x)) }
  if(inv){ return(collapse::`%!iin%`(x, indx)) }
  
  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.prep_relevel <- function(indx, rp, x, abortcall) {
  
  n.indx <- length(indx)
  if(n.indx == 0L) {
    return(logical(0L))
  }

  if(collapse::any_duplicated(indx)) {
    stop(simpleError("duplicate integers or names not allowed", call = abortcall))
  }

  if(n.indx != length(rp)) {
    error.txt <- "recycling not allowed"
    stop(simpleError(error.txt, call = abortcall))
  }

}


#' @keywords internal
#' @noRd
.indx_make_element <- function(indx, x, is_list, chkdup, inv, abortcall) {
  
  if(is.function(indx)) {
    if(is_list){
      indx <- vapply(x, indx, FUN.VALUE = logical(1L), USE.NAMES = FALSE) |> unlist()
    } else {indx <- indx(x)}
    
    if(!is.logical(indx) || length(indx) != length(x)) {
      stop(simpleError(
        "if elements are given through a function, the function must return a logical vector",
        call = abortcall
      ))
    }
    if(!inv) return(which(indx))
    if(inv) return(which(!indx))
  }
  
  n.indx <- length(indx)
  
  if(n.indx == 0L) {
    n <- length(x)
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(n))
  }
  
  if(is.complex(indx)) {
    n <- length(x)
    indx <- .indx_convert_complex(indx, n)
    .indx_check_int(indx, n, abortcall)
    return(.indx_convert_int(indx, n, chkdup, inv, abortcall))
  }
  
  if(is.numeric(indx)) {
    n <- length(x)
    .indx_check_int(indx, n, abortcall)
    return(.indx_convert_int(indx, n, chkdup, inv, abortcall))
  }
  
  if(is.character(indx)) {
    nms <- names(x)
    .indx_check_names(nms, abortcall)
    return(.indx_convert_chr(indx, nms, chkdup, inv, abortcall))
    
  }
  
  if(is.logical(indx)) {
    n <- length(x)
    .indx_check_logical(n.indx, n, abortcall)
    
    if(!inv){return(which(indx))}
    if(inv){return(which(!indx))}
    
  }
  
  
  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.indx_make_dim <- function(
    indx, x, dim.L, chkdup, inv, abortcall # removed =1L from dim.L argument spec
) {
  

  n.indx <- length(indx)
  
  
  if(n.indx == 0L) {
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(dim(x)[dim.L]))
  }
  
  if(is.complex(indx)) {
    n <- dim(x)[dim.L]
    indx <- .indx_convert_complex(indx, n)
    .indx_check_int(indx, n, abortcall)
    return(.indx_convert_int(indx, n, chkdup, inv, abortcall))
  }
  
  if(is.numeric(indx)) {
    dlength <- dim(x)[dim.L]
    .indx_check_int(indx, dlength, abortcall)
    return(.indx_convert_int(indx, dlength, chkdup, inv, abortcall))
  }
  
  if(is.character(indx)) {
    dnames <- dimnames(x)[[dim.L]]
    .indx_check_names(dnames, abortcall)
    return(.indx_convert_chr(indx, dnames, chkdup, inv, abortcall))
    
  }

  if(is.logical(indx)) {
    dlength <- dim(x)[dim.L]
    .indx_check_logical(n.indx, dlength, abortcall)

    if(!inv){return(which(indx))}
    if(inv){return(which(!indx))}
    
  }

  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.indx_make_tableind <- function(
    indx, x, dim.L, chkdup, inv, abortcall
) {

  
  n.indx <- length(indx)
  
  
  if(n.indx == 0L) {
    if(!inv) return(integer(0L))
    if(inv) return(seq_len(dim(x)[dim.L]))
  }
  
  if(is.complex(indx)) {
    n <- dim(x)[dim.L]
    indx <- .indx_convert_complex(indx, n)
    .indx_check_int(indx, n, abortcall)
    return(.indx_convert_int(indx, n, chkdup, inv, abortcall))
  }
  
  if(is.numeric(indx)) {
    dlength <- dim(x)[dim.L]
    .indx_check_int(indx, dlength, abortcall)
    return(.indx_convert_int(indx, dlength, chkdup, inv, abortcall))
  }
  
  if(is.character(indx)) {
    if(dim.L == 1L) dnames <- rownames(x)
    if(dim.L == 2L) dnames <- names(x)

    .indx_check_names(dnames, abortcall)
    return(.indx_convert_chr(indx, dnames, chkdup, inv, abortcall))
  }
  
  if(is.logical(indx)) {
    if(dim.L == 1L) dlength <- collapse::fnrow(x)
    if(dim.L == 2L) dlength <- collapse::fncol(x)
    .indx_check_logical(n.indx, dlength, abortcall)
    
    if(!inv){return(which(indx))}
    if(inv){return(which(!indx))}
  }
  
  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.indx_make_filter <- function(x, filter, inv, abortcall) {
  
  is_formula <- inherits(filter, "formula") && is.call(filter) && filter[[1L]] == quote(`~`)
  if(!is_formula) {
    stop(simpleError("`filter` must be a formula", call = abortcall))
  }
  if(length(filter) != 2L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  
  mm <- .with_internal(x, filter, abortcall)
  environment(filter) <- NULL
  
  if(!is.logical(mm)) {
    stop(simpleError("invalid formula given", call = abortcall))
  }
  if(!inv)return(which(mm))
  if(inv)return(which(!mm))
  
}


#' @keywords internal
#' @noRd
.indx_make_vars <- function(x, vars, inv, abortcall) {
  if(!is.function(vars)) {
    stop(simpleError("`vars` must be a function", call = abortcall))
  }
  out <- collapse::get_vars(x, vars, return = "logical")
  if(!inv)return(which(out))
  if(inv)return(which(!out))
}



#' @keywords internal
#' @noRd
.check_args_factor <- function(i, lvl, drop, abortcall) {
  if(!is.null(i) && !is.null(lvl)) {
    stop(simpleError("cannot specify both elements and levels", call = abortcall))
  }
}



#' @keywords internal
#' @noRd
.check_args_array <- function(x, sub, dims, i, abortcall) {
  
  present_dims <- !is.null(sub) || !is.null(dims)
  if(present_dims && !is.null(i)) {
    stop(simpleError(
      "cannot specify both `sub`/`dims` and elements",
      call = abortcall
    ))
  }
}



#' @keywords internal
#' @noRd
.check_args_df <- function(x, row, col, filter, vars, abortcall) {
  if(!is.null(filter) && !is.null(row)) {
    stop(simpleError(
      "cannot specify both `filter` and `row`",
      call = abortcall
    ))
  }
  if(!is.null(vars) && !is.null(col)) {
    stop(simpleError(
      "cannot specify both `vars` and `col`",
      call = abortcall
    ))
  }
  if(collapse::any_duplicated(names(x))) {
    stop(simpleError(
      "`x` does not have unique variable names for all columns; \n fix this before subsetting",
      call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.all_NULL_indices <- function(lst) {
  check <- vapply(lst, \(x) is.null(x), FUN.VALUE = logical(1L))
  if(all(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @keywords internal
#' @noRd
.any_empty_indices <- function(lst) {
  check <- vapply(lst, \(x)!is.null(x) && length(x) == 0L, FUN.VALUE = logical(1L))
  if(any(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @keywords internal
#' @noRd
.internal_fix_names <- function(x, f) {
  x.names <- names(x)
  dim(x.names) <- dim(x)
  x <- f(x)
  names(x) <- f(x.names)
  return(x)
}


#' @keywords internal
#' @noRd
.internal_check_rptf <- function(rp, tf, abortcall) {
  if(!missing(rp) && !missing(tf)) {
    stop(simpleError("cannot specify both `rp` and `tf`", call = abortcall))
  }
  if(missing(rp) && missing(tf)) {
    stop(simpleError("must specify either `rp` or `tf`", call = abortcall))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
  }
}

#' @keywords internal
#' @noRd
.check_rp_atomic <- function(rp, sslength, abortcall) {
  n.rp <- length(rp)
  if(is.recursive(rp)) {
    stop(simpleError("`rp` must be non-recursive", call = abortcall))
  }
  if(n.rp != sslength && n.rp != 1L) {
    stop(simpleError("recycling not allowed", call = abortcall))
  }
  # if(typeof(rp) != sstype) stop("type coercion not allowed")
}


#' @keywords internal
#' @noRd
.check_rp_df <- function(rp, abortcall) {
  if(!is.list(rp)) {
    stop(simpleError("`rp` must be a data.frame-like object or a list", call = abortcall))
  }
  # if(any(collapse::vtypes(rp) != sstypes)) stop("type coercion not allowed")
}


#' @keywords internal
#' @noRd
.check_rp_list <- function(rp, sslength, abortcall) {
  n.rp <- length(rp)
  if(!is.list(rp)) {
    stop(simpleError("`rp` must be a list", call = abortcall))
  }
  if(sslength != n.rp && n.rp != 1L) {
    stop(simpleError("recycling not allowed", call = abortcall))
  }
  # if(any(collapse::vtypes(rp) != sstypes)) stop("type coercion not allowed")
}

#' @keywords internal
#' @noRd
.with_internal <- function(data, form, abortcall) {
  vars <- all.vars(form)
  env <- environment(form)
  search_names <- c(names(data), names(env))
  if(any(!vars %in% search_names)) stop("unknown variable(s) given")
  txt <- as.character(form)[2L]
  out <- eval(parse(text = txt), data, enclos = env)
  environment(form) <- NULL
  return(out)
}


.internal_check_dots <- function(dots.list, abortcall) {
  # this check will not take much performance
  if(length(dots.list) > 0L) {
    error.txt <- paste0(
      "unknown arguments given:",
      "\n",
      paste(names(dots.list), collapse = ", ")
    )
    stop(simpleError(error.txt, call = abortcall))
  }
}


.internal_is_formula <- function(form) {
  check <- inherits(form, "formula") && is.call(form) && isTRUE(form[[1]] == "~")
  return(check)
}
