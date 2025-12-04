#' Methods to Extract, Exchange, Exclude, or Duplicate Subsets of an Object
#'
#' @description
#' Methods to extract, exchange, exclude,
#' or duplicate (i.e. repeat x times) subsets of an object. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,use,s,row,col See \link{squarebrackets_indx_args}. \cr
#' Duplicates are allowed, resulting in duplicated indices. \cr
#' An empty index selection results in an empty object of length 0. \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#'
#'
#'
#' @returns
#' Returns a copy of the sub-setted object.
#'
#'
#'
#' @example inst/examples/generic_x.R
#' 

#' @rdname sb_x
#' @export
ii_x <- function(x, i = NULL, use = 1, ...) {
  
  .methodcheck.ii(x, i, use, sys.call())
  
  UseMethod("ii_x", x)
}


#' @rdname sb_x
#' @export
ss_x <- function(x, s = NULL, use = 1:ndim(x), ...) {
  
  .methodcheck.ss(x, s, use, sys.call())
  UseMethod("ss_x", x)
}


#' @rdname sb_x
#' @export
sbt_x <- function(x, row = NULL, col = NULL, use = 1:2, ...) {
  .methodcheck.sbt(x, row, col, use, sys.call())
  UseMethod("sbt_x", x)
}



#' @rdname sb_x
#' @export
ii_x.default <- function(
    x, i = NULL, use = 1, ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(.C_is_missing_idx(i)){
    return(x)
  }
  
  elements <- ci_ii(x, i, use, chkdup = FALSE, .abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_x
#' @export
ss_x.default <- function(
    x, s = NULL, use = 1:ndim(x), ...
) {
  
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_array(x, s, use, sys.call()))
}


#' @rdname sb_x
#' @export
sbt_x.default <- function(x, row = NULL, col = NULL, use = 1:2, ...) {
  .internal_check_dots(list(...), sys.call())
  use <- .internal_make_use_tabular(use, sys.call())
  return(.sb_x_array(x, n(row, col), use, sys.call()))
}


#' @rdname sb_x
#' @export
sbt_x.data.frame <- function(x, row = NULL, col = NULL, use = 1:2, ...) {
  .internal_check_dots(list(...), sys.call())
  use <- .internal_make_use_tabular(use, sys.call())
  return(.sb_x_data.frame(x, row, col, use, sys.call()))
}





#' @keywords internal
#' @noRd
.sb_x_array <- function(
    x, s = NULL, use = 1:ndim(x), abortcall
) {
  
  # check arguments:
  .check_args_array(x, s, use, abortcall)
  
  # empty arguments:
  if(.all_missing_indices(s)) {
    return(x)
  }
  
  # zero-length subscripts:
  if(length(use) == 0L) {
    return(x)
  }
  
  lst <- ci_ss(x, s, use, chkdup = FALSE, .abortcall = abortcall)
  return(.arr_x(x, lst, abortcall = abortcall))
}



#' @keywords internal
#' @noRd
.sb_x_data.frame <- function(x, row, col, use, abortcall) {
 
  if(length(x) == 0L) {
    return(x)
  }
  
  # all missing arguments:
  if(.all_missing_indices(list(row, col))) {
    return(x)
  }
  
  # make arguments:
  if(!.C_is_missing_idx(row)) {
    row <- ci_margin(
      x, row, 1L, use[1], chkdup = FALSE, uniquely_named = TRUE, sys.call()
    )
  }
  if(is.function(col)) {
    col <- collapse::get_vars(x, col, return = "logical")
    if(use[2] > 0) col <- which(col)
    if(use[2] < 0) col <- collapse::whichv(col, FALSE)
  }
  else if(!.C_is_missing_idx(col)) {
    col <- ci_margin(
      x, col, 2L, use[2], chkdup = FALSE, uniquely_named =  TRUE, sys.call()
    )
  }
  if(.C_is_missing_idx(row)) row <- base::quote(expr = )
  if(.C_is_missing_idx(col)) col <- base::quote(expr = )
  
  x <- collapse::ss(x, row, col, check = FALSE)
  
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}
