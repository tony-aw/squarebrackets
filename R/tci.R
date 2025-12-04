#' Type Cast Indices
#'
#' @description
#' These functions typecast indices to proper integer indices. \cr
#'
#'
#' @param indx the indices to typecast
#' @param nms the relevant names, when typecasting character indices. \cr
#' Examples:
#'  * If the target is row indices, input row names for `nms`.
#'  * If the target is flat indices, input flat names for `nms`.
#' @param n the relevant size, when typecasting integer or logical indices. \cr
#' Examples:
#'  * If the target is row indices, input nrow for `n`.
#'  * If the target is flat indices, input the length for `n`.
#' @param inv Boolean, indicating if the indices should be inverted. \cr
#' See \link{squarebrackets_indx_args}.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param uniquely_named Boolean,
#' indicating if the user knows a-priori that the relevant names of `x` are unique. \cr
#' If set to `TRUE`, speed may increase. \cr
#' But specifying `TRUE` when the relevant names are not unique will result in incorrect output.
#' @param .abortcall environment where the error message is passed to.
#' 
#'
#' @returns
#' An integer vector of type-cast indices.
#'
#'
#' @example inst/examples/tci.R
#' 

#' @name developer_tci
NULL

#' @rdname developer_tci
#' @export
tci_bool <- function(indx, n, inv = FALSE, .abortcall = sys.call()) {
  if(length(indx) != n) {
    stop(simpleError("incorrect length of logical indices", call = .abortcall))
  }
  if(!inv) return(which(indx))
  if(inv) return(collapse::whichv(indx, FALSE))
}


#' @rdname developer_tci
#' @export
tci_int <- function(indx, n, inv = FALSE, chkdup = FALSE, .abortcall = sys.call()) {
  
  if(.any_badindx(indx, n)) {
    stop(simpleError("integers must be >= 1 and <= bounds", call = .abortcall))
  }
  
  if(chkdup) {
    if(anyDuplicated(indx)) { # base::anyDuplicated faster for numeric
      stop(simpleError("duplicate integers or names not allowed", call = .abortcall))
    }
  }
  
  if(!inv) { return(indx) }
  
  if(inv && length(indx) == 1L && n >= 2L) {
    if(indx == 1L) { return(2L:n) }
    else if(indx == n) { return(1:(n - 1L)) }
    else { return(seq_len(n)[-indx]) }
  }
  if(inv && length(indx) == 2L && n >= 3L) {
    if(all(sort(indx) == c(1L, n))) {
      return(2L:(n - 1L))
    }
    else { return(seq_len(n)[-indx]) }
  }
  
  if(inv) { return(seq_len(n)[-indx]) }
  
}


#' @rdname developer_tci
#' @export
tci_chr <- function(
    indx, nms, inv = FALSE, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  if(length(nms) == 0L) {
    stop(simpleError("no names present", call = .abortcall))
  }
  
  if(chkdup) {
    if(collapse::any_duplicated(indx)) {
      stop(simpleError("duplicate integers or names not allowed", call = .abortcall))
    }
  }
  
  if(!inv) { 
    if(uniquely_named) {
      return(collapse::fmatch(collapse::na_omit(indx), nms))
    }
    else {
      return(match_all(indx, nms))
    }
  }
  if(inv){ return(collapse::`%!iin%`(nms, indx)) }
  
}


#' @rdname developer_tci
#' @export
tci_im <- function(indx, n, inv = FALSE, chkdup = FALSE, .abortcall = sys.call()) {
  
  if(Re(indx[1L]) != 0 || Re(indx[length(1L)]) != 0L) {
    txt <- paste0(
      "non-zero real parts detected in complex indices.",
      "\n",
      "complex numbers are treated as imaginary numbers;",
      "\n",
      "the real part is ignored"
    )
    warning(simpleWarning(txt, call = .abortcall))
  }
  
  indx <- .C_convert_cplx(indx, n)
  
  return(tci_int(indx, n, inv, chkdup, .abortcall = .abortcall))
}


tci_zerolen <- function(n, inv = FALSE) {
  if(!inv) return(integer(0L))
  if(inv) return(1:n)
}
