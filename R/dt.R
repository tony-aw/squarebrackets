#' Functional Forms of data.table Operations
#'
#' @description
#' Functional forms of special data.table operations. \cr
#' These functions do not use Non-Standard Evaluation. \cr
#' These functions also benefit from the security measures that
#' 'squarebrackets' implements for 
#' the \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#' 
#'  * `dt_aggregate()`
#'  aggregates a data.table or tidytable, and returns the aggregated copy.
#'  * `dt_setcoe()`
#'  coercively transforms columns of a data.table or tidytable
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * `dt_setrm()`
#'  removes columns of a data.table or tidytable
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * `dt_setadd(x, new)`
#'  adds the columns from data.table/tidytable `new` to data.table/tidytable `x`,
#'  thereby modifying `x`
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * `dt_setreorder()`
#'  reorders the rows and/or variables of a `data.table`
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#' 
#' 
#' 
#' @param x a `data.table` or `tidytable`.
#' @param new a `data.table` or `tidytable`. \cr
#' It must have column names that do not already exist in `x`.
#' @param f the aggregation function
#' @param v the coercive transformation function
#' @param col,vars see \link{squarebrackets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param SDcols atomic vector,
#' giving the columns to which the aggregation function `f()` is to be applied on.
#' @param by atomic vector,
#' giving the grouping columns.
#' @param order_by Boolean,
#' indicating if the aggregated result should be ordered by the columns specified in `by`.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")` \cr
#' @param roworder a integer vector of the same length as `nrow(x)`,
#' giving the order in which the rows are to be re-order.
#' Internally,
#' this numeric vector will be turned into an order using \link[base]{order},
#' thus ensuring it is a strict permutation of `1:nrow(x)`.
#' @param varorder integer or character vector of the same length as `ncol(x)`,
#' giving the new column order. \cr
#' See \code{data.table::}\link[data.table]{setcolorder}.
#' 
#' @details
#' `dt_setreorder(x, roworder = roworder)`
#' internally creates a new column to reorder the data.table by,
#' and then removes the new column. \cr
#' The column name is randomized,
#' and extra care is given to ensure it does not overwrite any existing columns. \cr \cr
#' 
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
    x, col = NULL, vars = NULL, v, chkdup = getOption("sb.chkdup", FALSE)
) {
  
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  
  .check_args_df(x, row = NULL, col = col, filter = NULL, vars = vars, abortcall = sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
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
    data.table::set(x, j = j, value = v(x[[j]]))
  }
  
  return(invisible(NULL))
  
}


#' @rdname dt
#' @export
dt_setrm <- function(x, col = NULL, vars = NULL, chkdup = getOption("sb.chkdup", FALSE)) {
  
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  
  .check_args_df(x, row = NULL, col = col, filter = NULL, vars = vars, abortcall = sys.call())
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
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
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
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



#' @rdname dt
#' @export
dt_setreorder <- function(x, roworder = NULL, varorder = NULL) {
  
  
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  .check_bindingIsLocked(substitute(x), parent.frame(n = 1), abortcall = sys.call())
  
  
  if(!is.null(varorder)) {
    if(anyDuplicated(names(x))) {
      stop("`x` does not have unique variable names for all columns; \n fix this before subsetting")
    }
    data.table::setcolorder(x, neworder = varorder)
  }
  
  
  if(!is.null(roworder)) {
    if(!is.numeric(roworder) || length(roworder) != nrow(x)) {
      stop("`roworder` must be a strict permutation/shuffle of 1:nrow(x)")
    }
    roworder <- order(roworder)
    nms <- names(x)
    nms_lens <- stringi::stri_length(nms)
    if(any(nms_lens) == 0L) {
      stop("zero-length names detected")
    }
    if(min(nms_lens) > 1L) {
      j <- stringi::stri_rand_strings(1L, 1L)
    } else {
      j <- stringi::stri_rand_strings(1L, max(nms_lens) + 1L)
    }
    if(data.table::`%chin%`(j, nms)) {
      stop(
        "the names of `x` have changed while `dt_setreorder()` was running",
        "\n",
        "exiting function"
      )
    }
    data.table::set(x, j = j, value = roworder)
    data.table::setorderv(x, cols = j)
    data.table::set(x, j = j, value = NULL)
  }
  
  
  return(invisible(NULL))
  
}
