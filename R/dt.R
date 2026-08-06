#' Functional Forms of data.table Operations
#'
#' @description
#' Functional forms of special data.table operations. \cr
#' These functions do not use Non-Standard Evaluation. \cr
#' These functions also benefit from the security measures that
#' 'squarebrackets' implements for 
#' the \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' 
#'  * `dt_aggr()`
#'  aggregates a data.table or tidytable, applying functions over columns specified in `col`.
#'  * `dt_setcoe()`
#'  coercively transforms whole columns of a data.table,
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * `dt_setmutate()` modifies, adds, or removes columns,
#'  possibly based on other existing columns,
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#' 
#' 
#' 
#' @param x a `data.table` or `tidytable`.
#' @param fun an aggregation function, or a list of aggregation functions. \cr
#' A named list is the preferred form.
#' @param newnames a vector of names for the aggregated columns. \cr
#' If `NULL` (default),
#' `newnames` will be constructed as: \cr
#' `paste0(names(fun), "(", names(tt_x(x, 0L, col)), ")")`.
#' @param v the coercive transformation function
#' @param row,col,use see \link{squarebrackets_index_args}. \cr
#' For `dt_setcoe()`, `use` must be either a scalar positive number to select columns,
#' or a scalar negative number to exclude columns.
#' @param by a character vector, giving the names of the grouping column(s).
#' @param keyby Boolean,
#' indicating if the aggregated result should be ordered by the columns specified in `by`.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param mutations a named list, or a formula that evaluates to a named list. \cr
#' List names that correspond to existing columns, will modify those columns. \cr
#' List names that don't will new create columns with those names. \cr
#' List contents can be `NULL` to remove a column, or a vector to replace the column. \cr
#' In formula form, columns from  `x` can be used as variables. \cr
#' For example,
#' the following formula will transform the existing column named "column3" using other existing columns,
#' and it will create a new column called "newcolumn" as functions from existing columns: \cr
#' `~ list(column3 = column1 / column2, newcolumn = column1 + column2)` \cr
#' 
#' 
#'
#' @returns
#' For `dt_aggr()`: \cr
#' The aggregated `data.table` object. \cr
#' \cr
#' For the rest of the functions: \cr
#' Returns: VOID. These functions modify the object by reference. \cr
#' Do not use assignments like `x <- dt_setcoe(x, ...)`. \cr
#' Since these functions return void, you'll just get `NULL`. \cr \cr
#'
#'
#'
#' @example inst/examples/dt.R
#' 
#' 
#
#' 
#' 

#' @name dt
NULL


#' @rdname dt
#' @export
#' @importFrom data.table .SD .N .I ':='
dt_aggr <- function(
    x, row = NULL, col = NULL, use = 1:2, fun, by, newnames = NULL, keyby = FALSE
) {
  
  .methodcheck.dt(x, sys.call())
  
  # Set-up sub-setting args:
  rowcol <- ci_df(x, row, col, use, FALSE, sys.call())
  row <- rowcol[[1L]]
  col <- rowcol[[2L]]
  if(.C_is_missing_idx(row)) row <- base::quote(expr = )
  # needed for newnames anyways:
  if(.C_is_missing_idx(col)) {
    col <- names(x)
  }
  else {
    col <- names(x)[col]
  }
  
  
  # check by:
  if(!is.character(by) || length(by) < 1) {
    stop("`by` must be a character vector of one or more column names")
  }
  if(anyDuplicated(by)) {
    stop("`by` cannot have duplicate values")
  }
  if(any(!by %in% names(x))) {
    stop("`by` specifies an unknown column")
  }
  if(!isTRUE(keyby) && !isFALSE(keyby)) {
    stop("`keyby` must be `TRUE` or `FALSE`")
  }

  # recycle & check `fun`, `col`, and `newnames`
  if(!is.list(fun)) {
    fun <- list(fun)
  }
  check.fun <- all(vapply(fun, is.function, logical(1L)))
  if(!check.fun) {
    stop("`fun` must be a function or a list of functions")
  }
  n.fun <- length(fun)
  n.cols <- length(col)
  if(n.fun == 0L || n.cols == 0L) {
    stop("zero-length `fun` or `col` not supported")
  }
  if(!.is.multiple(n.fun, n.cols)) {
    stop("`length(fun)` and `length(col)` are not multiple of each other")
  }
  if(n.fun < n.cols) {
    fun <- rep(fun, length.out = n.cols)
  }
  else if(n.cols < n.fun) {
    col <- rep(col, length.out = n.fun)
  }
  if(is.null(newnames)) {
    if(is.null(names(fun))) {
      newnames <- make.unique(col)
    }
    else {
      newnames <- paste0(names(fun), "(", col, ")")
    }
    
  }
  if(length(newnames) != length(fun)) {
    stop("`newnames` must be the same length as the replicated lengths of `fun` and `col`")
  }
  
  
  # FUNCTION:
  out <- x[row, Map(function(f, x) f(x), fun, .SD), .SDcols = col, by = c(by), keyby = c(keyby)]
  ind2rename <- (length(by) + 1L):ncol(out)
  data.table::setnames(out, ind2rename, newnames)
  return(out)
  
}


#' @rdname dt
#' @export
dt_setcoe <- function(
    x, col = NULL, use = 2L, v, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  .methodcheck.dt(x, sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  if(ncol(x) == 0L || nrow(x) == 0L) {
    return(x)
  }
  
  if(is.function(col)) {
    col <- collapse::get_vars(x, col, return = "logical")
    if(use > 0) col <- which(col)
    if(use < 0) col <- collapse::whichv(col, FALSE)
  }
  else if(!.C_is_missing_idx(col)) {
    col <- ci_margin(
      x, col, 2L, use, chkdup = FALSE, uniquely_named =  TRUE, sys.call()
    )
  }
  
  if(is.null(col)) col <- seq_len(ncol(x))
  
  for(j in col) { # using loop instead of lapply to reduce memory to only one column at a time
    data.table::set(x, j = j, value = v(x[[j]]))
  }
  
  return(invisible(NULL))
  
}


#' @rdname dt
#' @export
dt_setmutate <- function(
    x, mutations
) {
  
  .methodcheck.dt(x, sys.call())
  
  if(is.formula(mutations)) {
    if(length(mutations) != 2L) {
      stop("improper formula given")
    }
    mutations <- .with_internal(x, mutations, sys.call())
  }
  if(!is.list(mutations) || is.null(names(mutations))) {
    stop("`mutations` must be a named list, or a formula that evaluates to a named list")
  }
  if(length(mutations) == 0L) {
    return(invisible(NULL))
  }
  
  vars <- names(mutations)
  data.table::set(x, j = vars, value = mutations)
  return(invisible(NULL))
  
}
  
  
  
