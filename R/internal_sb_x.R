#


#' @keywords internal
#' @noRd
.indx_convert_chr.sb_x <- function(indx, dnames, is_unique, abortcall) {
  if(is_unique) {
    out <- collapse::fmatch(indx, dnames)
  } else { out <- match_all(indx, dnames) }
  return(out)
}



#' @keywords internal
#' @noRd
.indx_make_element.sb_x <- function(indx, x, is_list, abortcall) {
  
  if(is.function(indx)) {
    if(is_list){
      indx <- vapply(x, indx, FUN.VALUE = logical(1), USE.NAMES = FALSE) |> unlist()
    } else {indx <- indx(x)}
    
    if(!is.logical(indx)) {
      error.txt <- simpleError(
        "if elements are given through a function, the function must return a logical vector",
        call = abortcall
      )
      stop(error.txt)
    }
    return(which(indx))
  }
  
  n.indx <- length(indx)
  
  .indx_check_general(indx, abortcall)
  
  if(n.indx == 0) {
    return(integer(0))
  }
  
  if(is.numeric(indx)) {
    n <- length(x)
    .indx_check_int(indx, n, abortcall)
    return(indx)
  }
  
  if(is.character(indx)) {
    nms <- names(x)
    .indx_check_names(nms, abortcall)
    return(.indx_convert_chr.sb_x(indx, nms, FALSE, abortcall))
    
  }
  
  if(is.logical(indx)) {
    n <- length(x)
    .indx_check_logical(n.indx, n, abortcall)
    return(which(indx))
  }
  
  .indx_stop(abortcall)
}



#' @keywords internal
#' @noRd
.lvl2indx.sb_x <- function(indx, x, abortcall) {
  
  .indx_check_general(indx, abortcall)
  
  if(length(indx)==0) {
    return(integer(0))
  }
  
  return(match_all(indx, x))
}



#' @keywords internal
#' @noRd
.indx_make_dim.sb_x <- function(
    indx, x, dim.L=1, abortcall
) {
  

  .indx_check_general(indx, abortcall)
  n.indx <- length(indx)
  
  if(n.indx == 0) {
    return(integer(0))
  }
  
  if(is.numeric(indx)) {
    dlength <- dim(x)[[dim.L]]
    .indx_check_int(indx, dlength, abortcall)
    return(indx)
  }
  
  if(is.character(indx)) {
    dnames <- dimnames(x)[[dim.L]]
    .indx_check_names(dnames, abortcall)
    return(.indx_convert_chr.sb_x(indx, dnames, FALSE, abortcall))
    
  }
  
  if(is.logical(indx)) {
    dlength <- dim(x)[[dim.L]]
    .indx_check_logical(n.indx, dlength, abortcall)
    return(which(indx))
  }

  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.indx_make_tableind.sb_x <- function(
    indx, x, dim.L=1, abortcall
) {
  
  .indx_check_general(indx, abortcall)
  
  n.indx <- length(indx)
  
  if(n.indx == 0) {
    return(integer(0))
  }
  
  if(is.numeric(indx)) {
    if(dim.L == 1) dlength <- collapse::fnrow(x)
    if(dim.L == 2) dlength <- collapse::fncol(x)
    .indx_check_int(indx, dlength, abortcall)
    return(indx)
  }
  
  if(is.character(indx)) {
    if(dim.L == 1) dnames <- rownames(x)
    if(dim.L == 2) dnames <- names(x)

    .indx_check_names(dnames, abortcall)
    return(.indx_convert_chr.sb_x(indx, dnames, TRUE, abortcall))
  }
  
  if(is.logical(indx)) {
    if(dim.L == 1) dlength <- collapse::fnrow(x)
    if(dim.L == 2) dlength <- collapse::fncol(x)
    .indx_check_logical(n.indx, dlength, abortcall)
    
    return(which(indx))
  }
  
  .indx_stop(abortcall)
}
