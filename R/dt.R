#' Functional Forms of data.table Operations
#'
#' @description
#' Functional forms of special data.table operations - ALL programmatically friendly 
#' (no Non-Standard Evaluation). \cr
#' \cr
#' `dt_aggregate()`
#' aggregates a data.table or tidytable, and returns the aggregated copy. \cr
#' `dt_setcoe()`
#' coercively transforms columns of a data.table or tidytable
#' using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' `dt_setrm()`
#' removes columns of a data.table or tidytable
#' using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' `dt_setadd(x, new)`
#' adds the columns from data.table/tidytable `new` to data.table/tidytable `x`,
#' thereby modifying `x`
#' using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#' 
#' 
#' 
#' @param x a `data.table` or `tidytable`.
#' @param new a `data.table` or `tidytable`. \cr
#' It must have column names that do not already exist in `x`.
#' @param f the aggregation function
#' @param col,vars columns to select for coercion; see \link{squarebrackets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param SDcols atomic vector,
#' giving the columns to which the aggregation function `f()` is to be applied on.
#' @param by atomic vector,
#' giving the grouping columns.
#' @param order_by logical (`TRUE` or `FALSE`),
#' indicating if the aggregated result should be ordered by the columns specified in `by`.
#' @param chkdup see \link{squarebrackets_duplicates}.
#'
#' @returns
#' For `dt_aggregate()`: \cr
#' The aggregated `data.table` object. \cr
#' \cr
#' For the rest of the functions: \cr
#' Returns: VOID. These functions modify the object by reference. \cr
#' Do not use assignments like `x <- dt_setcoe(x, ...)`. \cr
#' Since these functions return void, you'll just get `NULL`. \cr \cr
#'
#'
#'
#' @examplesIf requireNamespace("sf")
#' requireNamespace("sf")
#' 
#' 
#' # dt_aggregate on sf-data.table ====
#' 
#' x <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#' x <- data.table::as.data.table(x)
#' 
#' x$region <- ifelse(x$CNTY_ID <= 2000, 'high', 'low')
#' d.aggr <- dt_aggregate(
#'   x, SDcols = "geometry", f= sf::st_union, by = "region"
#' )
#' 
#' head(d.aggr)
#' 
#' 
#' #############################################################################
#' 
#' 
#' # dt_setcoe ====
#' 
#' obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_set(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' )
#' str(obj)
#' obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' dt_setcoe(obj, vars = is.numeric, f = as.numeric) # integers are now numeric
#' str(obj)
#' sb_set(obj,
#'   filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # SAFE: coercion performed; so no warnings
#' ) 
#' str(obj)
#'
#'
#'
#' #############################################################################
#' 
#' 
#' # dt_setrm ====
#' 
#' obj <- data.table::data.table(
#'   a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
#' )
#' str(obj)
#' dt_setrm(obj, col = 1)
#' str(obj)
#' 
#' obj <- data.table::data.table(
#'   a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
#' )
#' str(obj)
#' dt_setrm(obj, vars = is.numeric)
#' str(obj)
#' 
#' #############################################################################
#' 
#' 
#' # dt_setadd ====
#' 
#' obj <- data.table::data.table(
#'   a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
#' )
#' new <- data.table::data.table(
#'   e = sample(c(TRUE, FALSE), 10, TRUE),
#'   f = sample(c(TRUE, FALSE), 10, TRUE)
#' )
#' dt_setadd(obj, new)
#' print(obj)
#' 
#' 

#' @name dt
NULL


#' @rdname dt
#' @export
#' @importFrom data.table .SD .N .I ':='
dt_aggregate <- function(
    x, SDcols = NULL, f, by, order_by = FALSE
) {
  
  if(!data.table::is.data.table(x)) stop("`x` must be a data.table")
  if(anyDuplicated(names(x))) stop("`x` does not have unique variable names for all columns; \n fix this before subsetting")
  
  if(!is.atomic(SDcols) || !is.atomic(by)) stop("`SDcols` and `by` must be atomic vectors")
  
  if(isFALSE(order_by)) return(x[, lapply(.SD, f), .SDcols = c(SDcols), by = c(by)])
  if(isTRUE(order_by)) return(x[, lapply(.SD, f), .SDcols = c(SDcols), keyby = c(by)])
  
  stop("`order_by` must be `TRUE` or `FALSE`")
}


#' @rdname dt
#' @export
dt_setcoe <- function(
    x, col = NULL, vars = NULL, f, chkdup = TRUE
) {
  
  
  if(!is.function(f)) stop("`f` must be a function")
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  
  .check_args_df(x, row = NULL, col = col, filter = NULL, vars = vars, abortcall = sys.call())
  
  if(!is.null(col)) {
    col <- .indx_make_tableind(
      col, x,  2, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    col <- names(x)[col]
  }
  
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
    col <- names(x)[col]
  }
  
  if(is.null(col)) col <- names(x)
  
  for(j in col) { # using loop instead of lapply to reduce memory to only one column at a time
    data.table::set(x, j = j, value = f(x[[j]]))
  }
  
  return(invisible(NULL))
  
}


#' @rdname dt
#' @export
dt_setrm <- function(x, col = NULL, vars = NULL, chkdup = TRUE) {
  
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  
  .check_args_df(x, row = NULL, col = col, filter = NULL, vars = vars, abortcall = sys.call())
  
  if(!is.null(col)) {
    col <- .indx_make_tableind(
      col, x,  2, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    col <- names(x)[col]
  }
  
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
    col <- names(x)[col]
  }
  
  if(is.null(col) || length(col) == 0) {
    stop("must specify at least one column")
  }
  
  data.table::set(x, j = col, value = NULL)
  
  return(invisible(NULL))
}


#' @rdname dt
#' @export
dt_setadd <- function(x, new) {
  
  # error handling:
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  if(anyDuplicated(names(x))) {
    stop("`x` does not have unique variable names for all columns; \n fix this before subsetting")
  }
  
  if(!data.table::is.data.table(new)) {
    stop("`new` must be a data.table")
  }
  if(ncol(new) == 0) {
    stop("must give at least one new column")
  }
  if(anyDuplicated(names(new))) {
    stop("`new` does not have unique variable names for all columns; \n fix this before subsetting")
  }
  
  if(any(data.table::`%chin%`(names(new), names(x)))) {
    stop("columns already exist")
  }
  
  data.table::set(x, j = names(new), value = new)
  
  return(invisible(NULL))
}
