#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' This is an S3 Method to return a copy of an object with modified subsets.
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,lvl,row,col,idx,dims,rcl,filter,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... further arguments passed to or from other methods.
#' @param tf the transformation function.
#' @param rp an object of somewhat the same type as the selected subset of \code{x},
#' and the same same length as the selected subset of \code{x} or a length of 1.
#' @param coe For data.frame-like objects,
#' `sb_mod()` can only coerce whole columns, not subsets of columns. \cr
#' So it does not automatically coerce column types
#' when `row` or `filter` is also specified. \cr
#' Therefore, the user can specify a coercion function,
#' to be applied on the entirety of every column specified in `col` or `vars`;
#' columns outside this subset are not affected. \cr
#' This coercion function is, of course, applied before replacement (`rp`) or transformation (`tf()`). \cr
#' By default, `coe = NULL` which means no columns are coercively transformed. \cr
#' See also \link{sb_coe}. \cr
#' @param chkdup see \link{squarebrackets_duplicates}.
#' @param .lapply `sb_mod()` by default uses \link[base]{lapply}
#' for lists and \link[collapse]{dapply} data.frame-like objects
#' to compute `tf()` on every list element or data.frame column. \cr
#' The user may supply a custom `lapply()/dapply()`-like function
#' in this argument to use instead. \cr
#' For example, the perform parallel transformation,
#' the user may supply `future.apply::`\link[future.apply]{future_lapply}. \cr
#' The supplied function must use the exact same argument convention as
#' \link[base]{lapply},
#' otherwise errors or unexpected behaviour may occur.
#' 
#' 
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset. \cr
#' Specifying `rp` will replace the subset. \cr
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' Note that the `tf` argument is not available for factors: this is intentional. \cr
#' \cr
#' 
#' 
#' 
#' @returns
#' A copy of the object with replaced/transformed values. \cr \cr
#'
#'
#' @examples
#' 
#' # atomic objects ====
#' 
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' rp <- -1:-9
#' sb_mod(obj, 1:3, 1:3, rp = rp)
#' # above is equivalent to  obj[1:3, 1:3] <- -1:-9; obj
#' sb_mod(obj, i = \(x)x<=5, rp = -1:-5)
#' # above is equivalent to  obj[obj <= 5] <- -1:-5; obj
#' sb_mod(obj, col = "a", rp = -1:-8)
#' # above is equivalent to  obj[, which(colnames(obj) %in% "a")] <- -1:-8; obj
#' sb_mod(obj, 1:3, 1:3, tf = \(x) -x)
#' # above is equivalent to  obj[1:3, 1:3] <- (-1 * obj[1:3, 1:3]); obj
#' sb_mod(obj, i = \(x)x<=5, tf = \(x) -x)
#' # above is equivalent to  obj[obj <= 5] <- (-1 * obj[obj <= 5]); obj
#' 
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_mod(obj, 1:3, 1:3, tf = \(x) -x)
#' # above is equivalent to  obj[1:3, 1:3] <- -1 * obj[1:3, 1:3]
#' sb_mod(obj, i = \(x)x<=5, tf = \(x) -x)
#' # above is equivalent to  obj[obj <= 5] <- -1:-5; obj
#' sb_mod(obj, col = "a", tf = \(x) -x)
#' # above is equivalent to  obj[, which(colnames(obj) %in% "a")] <- -1:-8; obj
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' sb_mod(obj, list(1:3, 1:2), c(1,3), rp = -1:-24)
#' # above is equivalent to obj[1:3, , 1:2] <- -1:-24
#' sb_mod(obj, i = \(x)x<=5, rp = -1:-5)
#' # above is equivalent to obj[obj <= 5] <- -1:-5
#' 
#' #############################################################################
#' 
#' 
#' # lists ====
#' 
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_mod(obj, "a", rp = list(1L))
#' # above is equivalent to  obj[["a"]] <- 1L; obj
#' sb_mod(obj, is.numeric, rp = list(-1:-10, -11:-20))
#' # above is equivalent to  obj[which(sapply(obj, is.numeric))] <- list(-1:-10, -11:-20); obj
#' 
#' #############################################################################
#' 
#' 
#' # data.frame-like objects ====
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' 
#' sb_mod(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' ) 
#' sb_mod(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   coe = as.double, tf = sqrt # SAFE: coercion performed
#' ) 
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_mod(
#'   obj, vars = is.numeric,
#'   tf = sqrt # SAFE: row=NULL & filter = NULL, so coercion performed
#' )
#'
#'
#'

#' @rdname sb_mod
#' @export
sb_mod <- function(x, ...) {
  UseMethod("sb_mod", x)
}


#' @rdname sb_mod
#' @export
sb_mod.default <- function(x, i, ..., rp, tf, chkdup = TRUE) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(
    i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )
  
  n.i <- length(elements)
  if(n.i == 0) return(x)
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i, abortcall = sys.call())
  x[elements] <- rp
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp, tf, chkdup = TRUE) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- tf(x[elements])
    }
    .check_rp_atomic(rp, n.i, abortcall = sys.call())
    x[elements] <- rp
    return(x)
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[row, col, drop = FALSE])
  }
  
  .check_rp_atomic(rp, (length(row) * length(col)), abortcall = sys.call())
  x[row, col] <- rp
  
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.array <- function(
    x, idx = NULL, dims = NULL, rcl = NULL, i = NULL, ..., rp, tf, chkdup = TRUE
) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- tf(x[elements])
    }
    .check_rp_atomic(rp, n.i, abortcall = sys.call())
    x[elements] <- rp
    return(x)
  }
  
  if(!is.null(rcl)) {
    if(length(dim(x)) != 3) stop("`rcl` only applicable for arrays with exactly 3 dimensions")
    if(!is.list(rcl) || length(rcl) != 3) stop("`rcl` must be a list of length 3")
    return(.sb3d_mod(
      x, rcl[[1]], rcl[[2]], rcl[[3]], rp = rp, tf = tf, chkdup = chkdup, abortcall = sys.call()
    ))
  }
  
  if(!missing(rp)) {
    if(is.recursive(rp)) stop("`rp` must be non-recursive")
    return(.arr_repl(x, idx, dims, rp, chkdup = chkdup, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    return(.arr_tf(x, idx, dims, tf, chkdup = chkdup, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb_mod.factor <- function(x, i = NULL, lvl = NULL, ..., rp, chkdup = TRUE) {
  
  .check_args_factor(i, lvl, drop = FALSE, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
    )
    n.i <- length(elements)
    if(n.i == 0) return(x)
    .check_rp_atomic(rp, n.i, abortcall = sys.call())
    x[elements] <- rp
    return(x)
  }
  if(!is.null(lvl)) {
    if(length(lvl) == 0) return(x)
    .prep_relevel(lvl, rp, x, sys.call())
    set.lvls <- levels(x)
    set.lvls[set.lvls == lvl] <- rp
    levels(x) <- set.lvls
    return(x)
  }
}


#' @rdname sb_mod
#' @export
sb_mod.list <- function(x, i, ..., rp, tf, chkdup = TRUE, .lapply = lapply) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(
    i, x, is_list = TRUE, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )
  
  n.i <- length(elements)
  
  if(n.i == 0) {
    return(x)
  }
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- .lapply(x[elements], tf)
  }
  
  .check_rp_list(rp, n.i, abortcall = sys.call())
  x[elements] <- rp
  
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, coe = NULL,
    ..., rp, tf, chkdup = TRUE, .lapply = lapply
) {
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = chkdup, inv = FALSE, abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(col)) col <- collapse::seq_col(x)
  col <- as.integer(col)
  if(!is.null(coe)) {
    x <- collapse::ftransformv(x, vars = col, FUN = coe, apply = TRUE)
  } else { x <- data.table::copy(x) }
  
  
  if(is.null(row)) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- .lapply(collapse::ss(x, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp, abortcall = sys.call())
    data.table::set(x, j = col, value = rp)
  }
  
  if(!is.null(row)) {
    row <- as.integer(row)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- .lapply(collapse::ss(x, i = row, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp, abortcall = sys.call())
    data.table::set(x, i = row, j = col, value = rp)
  }
  
  return(x)
}


