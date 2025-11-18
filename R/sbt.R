#' #' Programmatically Friendly subset-like Methods for data.frames and matrices
#' #'
#' #' @description
#' #' The set of `sbt_*` methods are programmatically friendly \link[base]{subset}-like methods. \cr
#' #' They work on data.frames, atomic matrices, and recursive matrices. \cr \cr
#' #' 
#' #' 
#' #' 
#' #' @param x a data.frame, atomic matrix, or recursive matrix.
#' #' @param row,col similar to the `slice` argument as described in \link{squarebrackets_indx_args}.
#' #' @param obs,vars,inv,chkdup,.lapply see \link{squarebrackets_indx_args}.
#' #' @param rp,tf see \link{squarebrackets_modify}.
#' #' @param ... see \link{squarebrackets_method_dispatch}.
#' #' 
#' #' 
#' #' 
#' #'
#' #' @returns
#' #' See the respective `ss_*`/ `ss_*` methods. \cr \cr
#' #' 
#' #'
#' #'
#' #'
#' #' @example inst/examples/sbt.R
#' #'
#' 
#' 
#' # Methods ====
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_x <- function(x, ...) {
#'   .methodcheck.sbt(x, sys.call())
#'   UseMethod("sbt_x", x)
#' }
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_wo <- function(x, ...) {
#'   .methodcheck.sbt(x, sys.call())
#'   UseMethod("sbt_wo", x)
#' }
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_mod <- function(x, ...) {
#'   .methodcheck.sbt(x, sys.call())
#'   UseMethod("sbt_mod", x)
#' }
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_set <- function(x, ...) {
#'   .methodcheck.sbt(x, sys.call())
#'   UseMethod("sbt_set", x)
#' }
#' 
#' 
#' # Dispatches ====
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_x.default <- function(x, row = NULL, col = NULL, ...) {
#'   .internal_check_dots(list(...), sys.call())
#'   if(!.C_is_missing_idx(row)) row <- ci_margin(x, row, 1L, inv = FALSE, chkdup = FALSE, FALSE, sys.call())
#'   if(!.C_is_missing_idx(col)) col <- ci_margin(x, col, 1L, inv = FALSE, chkdup = FALSE, FALSE, sys.call())
#'   if(.C_is_missing_idx(row)) row <- base::quote(expr = )
#'   if(.C_is_missing_idx(col)) col <- base::quote(expr = )
#'   return(collapse::ss(x, row, col))
#' }
#' 
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_x.data.frame <- function(x, obs = NULL, vars = NULL, ...) {
#'   .internal_check_dots(list(...), sys.call())
#'   return(.sb_x_data.frame(x, NULL, NULL, obs, vars, inv = FALSE, chkdup = FALSE, sys.call()))
#' }
#' 
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_wo.default <- function(x, row = NULL, col = NULL, ..., chkdup = getOption("squarebrackets.chkdup", FALSE)) {
#'   .internal_check_dots(list(...), sys.call())
#'   if(!.C_is_missing_idx(row)) row <- ci_margin(x, row, inv = TRUE, chkdup, FALSE, sys.call())
#'   if(!.C_is_missing_idx(col)) col <- ci_margin(x, col, inv = TRUE, chkdup, FALSE, sys.call())
#'   if(.C_is_missing_idx(row)) row <- base::quote(expr = )
#'   if(.C_is_missing_idx(col)) col <- base::quote(expr = )
#'   return(collapse::ss(x, row, col))
#' }
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_wo.data.frame <- function(x, obs = NULL, vars = NULL, ..., chkdup = getOption("squarebrackets.chkdup", FALSE)) {
#'   .internal_check_dots(list(...), sys.call())
#'   return(.sb_x_data.frame(x, NULL, NULL, obs, vars, inv = TRUE, chkdup, sys.call()))
#' }
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_mod.default <- function(
#'   x, row = NULL, col = NULL, inv = FALSE, ...,
#'   rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
#' ) {
#'   return(ss_mod(x, n(row, col), 1:2, inv, rp = rp, tf = tf, chkdup = chkdup))
#' }
#' 
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_mod.data.frame <- function(
#'     x, obs = NULL, vars = NULL, inv = FALSE, ...,
#'     rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
#' ) {
#'   
#'   # checks:
#'   .internal_check_dots(list(...), sys.call())
#'   .internal_check_rptf(rp, tf, sys.call())
#'   .check_args_df(x, s, d, obs, vars, abortcall = sys.call())
#'   
#'   # make arguments:
#'   row <- col <- NULL
#'   if(!.C_is_missing_idx(obs)) {
#'     row <- ci_obs(
#'       x, obs, inv, chkdup, TRUE, sys.call()
#'     )
#'   }
#'   if(!.C_is_missing_idx(vars)) {
#'     col <- ci_vars(
#'       x, vars, inv, chkdup, TRUE, sys.call()
#'     )
#'   }
#'   
#'   # empty indices:
#'   if(.any_empty_indices(n(row, col))) {
#'     return(x)
#'   }
#'   
#'   # prep col:
#'   if(is.null(col)) {
#'     message("copying all columns")
#'     col <- as.integer(1:ncol(x))
#'   }
#'   
#'   # copy specified columns, but not the rest of the data.frame:
#'   x <- collapse::ftransformv(x, vars = col, FUN = data.table::copy, apply = TRUE)
#'   
#'   # prep replacement just in case:
#'   if(!missing(rp)) {
#'     rp <- .dt_prep_rp(rp)
#'   }
#'   
#'   # tramsformation:
#'   if(!missing(tf)) {
#'     rp <- .dt_transform(x, row, col, tf)
#'   }
#'   
#'   # modify:
#'   if(is.null(row)) {
#'     return(.dt_mod_whole(x, col, rp, abortcall = sys.call()))
#'   }
#'   needcoe <- .dt_check_needcoe(x, col, rp)
#'   if(needcoe) {
#'     return(.dt_mod_partialcoe(x, row, col, rp, sys.call()))
#'   }
#'   else {
#'     return(.dt_mod_partialset(x, row, col, rp, sys.call()))
#'   }
#'   
#' }
#' 
#' 
#' #' @rdname sbt
#' #' @export
#' sbt_set.default <- function(
#'     x, row = NULL, col = NULL, inv = FALSE, ...,
#'     rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE)
#' ) {
#'   return(ss_set(x, n(row, col), 1:2, inv, rp = rp, tf = tf, chkdup = chkdup))
#' }

#' 
#' #' @rdname sbt
#' #' @export
#' sbt_set.data.table <- function(
#'     x, obs = NULL, vars = NULL, inv = FALSE, ...,
#'     rp, tf, chkdup = getOption("squarebrackets.chkdup", FALSE), .lapply = lapply
#' ) {
#'   
#'   # checks:
#'   .internal_check_dots(list(...), sys.call())
#'   if(!data.table::is.data.table(x)) {
#'     stop("`x` is not a (supported) mutable object")
#'   }
#'   .internal_check_rptf(rp, tf, sys.call())
#'   .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
#'   
#'   
#'   # make arguments:
#'   row <- col <- NULL
#'   if(!.C_is_missing_idx(obs)) {
#'     row <- ci_obs(
#'       x, obs, inv, chkdup, TRUE, sys.call()
#'     )
#'   }
#'   if(!.C_is_missing_idx(vars)) {
#'     col <- ci_vars(
#'       x, vars, inv, chkdup, TRUE, sys.call()
#'     )
#'   }
#'   # don't use if(is.null(row or col)) row or col <- 1:... -> will mess up the rest of this function
#'   
#'   
#'   # empty indices:
#'   if(.any_empty_indices(n(row, col))) {
#'     return(invisible(NULL))
#'   }
#'   
#'   # prep col:
#'   if(is.null(col)) {
#'     col <- as.integer(1:ncol(x))
#'   }
#'   
#'   # prep replacement just in case:
#'   if(!missing(rp)) {
#'     rp <- .dt_prep_rp(rp)
#'   }
#'   
#'   # tramsformation:
#'   if(!missing(tf)) {
#'     rp <- .dt_transform(x, row, col, tf)
#'   }
#'   
#'   
#'   # SET:
#'   if(is.null(row)) {
#'     data.table::set(x, j = col, value = rp)
#'     return(invisible(NULL))
#'   }
#'   else {
#'     row <- as.integer(row)
#'     data.table::set(x, i = row, j = col, value = rp)
#'     return(invisible(NULL))
#'   }
#'   
#'   return(invisible(NULL))
#'   
#' }
#' 
#' 
#' 
