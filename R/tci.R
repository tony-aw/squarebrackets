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
#' @param use 1 or -1, indicating how to use the indices. \cr
#' See \link{squarebrackets_index_args}.
#' @param m a number giving the margin; set to `0` for dimensionless vectors.
#' @param form a formula; see \link{keywords}.
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
#' @concept tci
#' @example inst/examples/tci.R
#' 

#' @name developer_tci
NULL


#' @rdname developer_tci
#' @export
tci_atomic <- function(
    indx, n, nms, use = 1, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  if(is.logical(indx)) {
    return(tci_bool(indx, n, use, .abortcall))
  }
  else if(is.numeric(indx)) {
    return(tci_int(indx, n, use, chkdup, .abortcall))
  }
  else if(is.character(indx)) {
    return(tci_chr(indx, nms, use, chkdup, uniquely_named, .abortcall))
  }
}

#' @rdname developer_tci
#' @export
tci_bool <- function(indx, n, use = 1L, .abortcall = sys.call()) {
  if(length(indx) != n) {
    stop(simpleError("incorrect length of logical indices", call = .abortcall))
  }
  if(use > 0L) return(which(indx))
  if(use < 0L) return(collapse::whichv(indx, FALSE))
}


#' @rdname developer_tci
#' @export
tci_int <- function(indx, n, use = 1L, chkdup = FALSE, .abortcall = sys.call()) {
  
  if(.any_badindx(indx, n)) {
    stop(simpleError("integers must be >= 1 and <= bounds", call = .abortcall))
  }
  
  if(use > 0L) {
    if(chkdup) {
      if(anyDuplicated(indx)) { # base::anyDuplicated faster for numeric
        stop(simpleError("duplicate integers or names not allowed", call = .abortcall))
      }
    }
    return(indx) 
  }
  
  # the following is if use < 0L
  if(length(indx) == 1L && n >= 2L) {
    if(indx == 1L) { return(2L:n) }
    else if(indx == n) { return(1:(n - 1L)) }
    else { return(seq_len(n)[-indx]) }
  }
  
  if(length(indx) == 2L && n >= 3L) {
    if(all(sort(indx) == c(1L, n))) {
      return(2L:(n - 1L))
    }
    else { return(seq_len(n)[-indx]) }
  }
  
  return(seq_len(n)[-indx])
  
}


#' @rdname developer_tci
#' @export
tci_chr <- function(
    indx, nms, use = 1L, chkdup = FALSE, uniquely_named = FALSE, .abortcall = sys.call()
) {
  
  if(use > 0L) {
    return(.match_names(indx, nms, chkdup, .abortcall))
  }
  
  if(use < 0L) {
    
    if(length(nms) == 0L) {
      stop(simpleError("no names present", call = .abortcall))
    }
    return(collapse::`%!iin%`(nms, indx))
  }
  
  
}




#' @rdname developer_tci
#' @export
tci_formula <- function(form, m, n, nms, .abortcall) {
  
  keywords <- list(
    .M = m,
    .Nms = nms,
    .N = n,
    .I = seq_len(n),
    .bi = \(...) .keyword_bi(list(...),  n),
    .ptrn = \(ptrn, start = 1L, end = n) .keyword_ptrn(ptrn, start, end, n)
  )
  
  if(!is.formula(form)) {
    stop(simpleError("invalid formula given", call = .abortcall))
  }
  if(length(form) != 2L) {
    stop(simpleError("invalid formula given", call = .abortcall))
  }
  env <- environment(form)
  txt <- as.character(form)[2L]
  out <- eval(parse(text = txt), keywords, enclos = env)
  environment(form) <- NULL
  return(out)
}

#' @rdname developer_tci
#' @export
tci_zerolen <- function(n, use = 1L) {
  if(use > 0L) return(integer(0L))
  if(use < 0L) return(seq_len(n))
}

