#' Methods to Extract, Exchange, or Duplicate Subsets of an Object
#'
#' @description
#' Methods to extract, exchange,
#' or duplicate (i.e. repeat x times) subsets of an object. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,s,d,row,col,obs,vars See \link{squarebrackets_indx_args}. \cr
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
ii_x <- function(x, ...) {
  
  .methodcheck.ii(x, sys.call())
  
  UseMethod("ii_x", x)
}


#' @rdname sb_x
#' @export
ss_x <- function(x, ...) {
  
  .methodcheck.ss(x, sys.call())
  UseMethod("ss_x", x)
}


#' @rdname sb_x
#' @export
sbt_x <- function(x, ...) {
  .methodcheck.sbt(x, sys.call())
  UseMethod("sbt_x", x)
}


#' @rdname sb_x
#' @export
ii_x.default <- function(x, i = NULL, ...) {
  
  .internal_check_dots(list(...), sys.call())
  if(.C_is_missing_idx(i)){
    return(x)
  }
  
  elements <- ci_ii(x, i, .abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_x
#' @export
ss_x.default <- function(
    x, s = NULL, d = 1:ndim(x), ...
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, s, d, FALSE, FALSE, FALSE, sys.call()))
  
}


#' @rdname sb_x
#' @export
sbt_x.default <- function(x, row = NULL, col = NULL, ...) {
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_array(x, n(row, col), 1:2))
  return(x[row, col, drop = FALSE])
}


#' @rdname sb_x
#' @export
sbt_x.data.frame <- function(x, obs = NULL, vars = NULL, ...) {
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_data.frame(x, obs, vars, inv = FALSE, chkdup = FALSE, sys.call()))
}



#' @keywords internal
#' @noRd
.sb_x_array <- function(
    x, s = NULL, d = 1:ndim(x), inv = FALSE, red = FALSE, chkdup = FALSE, abortcall
) {
  
  # check arguments:
  .check_args_array(x, s, d, abortcall)
  
  # empty arguments:
  if(.all_missing_indices(list(s))) {
    return(x)
  }
  
  # zero-length subscripts:
  if(length(d) == 0L || .C_is_missing_idx(d)) {
    return(x)
  }
  
  # 1d:
  if(ndim(x) == 1L) {
    i <- .flat_s2i(x, s, d, abortcall)
    return(.flat_a1d_x(x, i, inv, red, chkdup, abortcall))
  }
  
  # s, d arguments:
  lst <- ci_ss(x, s, d, inv = inv, chkdup, .abortcall = abortcall)
  
  return(.arr_x(x, lst, abortcall = abortcall))
}



#' @keywords internal
#' @noRd
.sb_x_data.frame <- function(x, obs, vars, inv, chkdup, abortcall) {
 
  
  # all missing arguments:
  if(.all_missing_indices(list(obs, vars))) {
    return(x)
  }
  
  # make arguments:
  row <- col <- NULL
  if(!.C_is_missing_idx(obs)) {
    row <- ci_obs(
      x, obs, inv, chkdup, TRUE, sys.call()
    )
  }
  if(!.C_is_missing_idx(vars)) {
    col <- ci_vars(
      x, vars, inv, chkdup, TRUE, sys.call()
    )
  }
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  x <- collapse::ss(x, row, col, check = FALSE)
  
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}
