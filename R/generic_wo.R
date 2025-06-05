#' Methods to Return Object Without Specified Subset
#'
#' @description
#' S3 Methods to return an object \bold{without} the specified subset. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,s,d,obs,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection results in nothing being removed,
#' and the entire object is returned. \cr
#' @param red Boolean, for recursive objects only,
#' indicating if the result should be reduced. \cr
#' If `red = TRUE`,
#' selecting a single element will give the simplified result,
#' like using `[[]]`. \cr
#' If `red = FALSE`, a list is always returned regardless of the number of elements. \cr
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param ... see \link{squarebrackets_method_dispatch}.
#'
#'
#'
#' @returns
#' A copy of the sub-setted object.
#'
#'
#' @example inst/examples/generic_wo.R
#'
#'

#' @rdname sb_wo
#' @export
i_wo <- function(x, ...) {
  
  .methodcheck.i(x, sys.call())
  
  UseMethod("i_wo", x)
}

#' @rdname sb_wo
#' @export
i2_wo <- function(x, ...) {
  
  .methodcheck.i2(x, sys.call())
  
  UseMethod("i2_wo", x)
}

#' @rdname sb_wo
#' @export
ss_wo <- function(x, ...) {
  
  .methodcheck.ss(x, sys.call())
  UseMethod("ss_wo", x)
}


#' @rdname sb_wo
#' @export
ss2_wo <- function(x, ...) {
  
  .methodcheck.ss2(x, sys.call())
  
  
  UseMethod("ss2_wo", x)
}


#' @rdname sb_wo
#' @export
i_wo.default <- function(
    x, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(is.null(i)){
    return(x)
  }
  
  elements <- ci_flat(x, i, inv = TRUE, chkdup = chkdup, .abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_wo
#' @export
ss_wo.default <- function(
    x, s = NULL, d = 1:ndim(x), ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, s, d, NULL, TRUE, FALSE, chkdup, sys.call()))
}



#' @rdname sb_wo
#' @export
i2_wo.default <- function(
    x, i = NULL, red = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  if(is.null(i)) {
    return(x)
  }
  
  return(.flat_x(x, i, TRUE, red, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
ss2_wo.default <- function(
    x, s = NULL, d = 1:ndim(x), red = FALSE, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(!isTRUE(red) && !isFALSE(red)) {
    stop("`red` must be either `TRUE` or `FALSE`")
  }
  
  return(.sb_x_array(x, s, d, NULL, TRUE, red, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
ss2_wo.data.frame <- function(
    x, s = NULL, d = 1:2, obs = NULL, vars = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  .check_args_df(x, s, d, obs, vars, abortcall = sys.call())
  
  # all missing arguments:
  if(.all_missing_s_d(s, d) && .all_NULL_indices(list(obs, vars))) {
    return(x)
  }
  
  # make arguments:
  rowcol <- .dt_make_args(x, s, d, obs, vars, TRUE, chkdup, sys.call())
  row <- rowcol[[1L]]
  col <- rowcol[[2L]]
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  out <- collapse::ss(x, row, col, check = FALSE)
  
  names(out) <- make.names(names(out), unique = TRUE)
  return(out)
}


