#' Methods to Return Object Without Specified Subset
#'
#' @description
#' S3 Methods to return an object \bold{without} the specified subset. \cr \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,s,d,row,col,obs,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection results in nothing being removed,
#' and the entire object is returned. \cr
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
ii_wo <- function(x, ...) {
  
  .methodcheck.ii(x, sys.call())
  
  UseMethod("ii_wo", x)
}


#' @rdname sb_wo
#' @export
ss_wo <- function(x, ...) {
  
  .methodcheck.ss(x, sys.call())
  UseMethod("ss_wo", x)
}


#' @rdname sb_wo
#' @export
sbt_wo <- function(x, ...) {
  .methodcheck.sbt(x, sys.call())
  UseMethod("sbt_wo", x)
}



#' @rdname sb_wo
#' @export
ii_wo.default <- function(
    x, i = NULL, ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  
  if(is.null(i)){
    return(x)
  }
  
  elements <- ci_ii(x, i, inv = TRUE, chkdup = chkdup, .abortcall = sys.call())
  return(x[elements])
}


#' @rdname sb_wo
#' @export
ss_wo.default <- function(
    x, s = NULL, d = 1:ndim(x), ...,
    chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_array(x, s, d, TRUE, FALSE, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
sbt_wo.default <- function(x, row = NULL, col = NULL, ..., chkdup = getOption("squarebrackets.chkdup", FALSE)) {
  .internal_check_dots(list(...), sys.call())
  
  return(.sb_x_array(x, n(row, col), 1:2, TRUE, FALSE, chkdup, sys.call()))
}


#' @rdname sb_wo
#' @export
sbt_wo.data.frame <- function(x, obs = NULL, vars = NULL, ..., chkdup = getOption("squarebrackets.chkdup", FALSE)) {
  .internal_check_dots(list(...), sys.call())
  return(.sb_x_data.frame(x, obs, vars, inv = TRUE, chkdup, sys.call()))
}


