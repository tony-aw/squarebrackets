#' Method to Un-Select/Remove Subsets of an Object
#'
#' @description
#' This is an S3 Method to un-select/remove subsets from an object.
#'
#' @param x see \link{squarebrackets_immutable_classes} and \link{squarebrackets_mutable_classes}.
#' @param i,lvl,row,col,idx,dims,rcl,filter,vars See \link{squarebrackets_indx_args}. \cr
#' An empty index selection results in nothing being removed,
#' and the entire object is returned. \cr
#' @param drop logical.
#'  * For factors: If `drop = TRUE`, unused levels are dropped, if `drop = FALSE` they are not dropped.
#'  * For lists: if `drop = TRUE`, selecting a single element will give the simplified result,
#'  like using `[[]]`. If `drop = FALSE`, a list is always returned regardless of the number of elements.
#' @param rat `TRUE` or `FALSE`,
#' indicating if attributes should be returned with the sub-setted object. \cr
#' See Details section for more info.
#' @param chkdup see \link{squarebrackets_duplicates}.
#' @param ... further arguments passed to or from other methods.
#'
#'
#' @details
#' \bold{One the \code{rat} argument} \cr
#' Most `[` - methods strip most (but not all) attributes. \cr
#' If `rat = FALSE, chkdup = TRUE`, this default behaviour is preserved,
#' for compatibility with special classes. This is the fastest option. \cr
#' If `rat = TRUE`,
#' attributes from `x` missing after sub-setting are re-assigned to `x`.
#' Already existing attributes after sub-setting will not be overwritten. \cr
#' There is no `rat` argument for data.frame-like object:
#' their attributes will always be preserved. \cr
#' NOTE: In the following situations, the `rat` argument will be ignored,
#' as the attributes necessarily have to be dropped:
#'  * when `x` is a list, AND `drop = TRUE`, AND a single element is selected.
#'  * when `x` is a matrix or array, and sub-setting is done through the `i` argument.
#'
#'
#' @returns
#' A copy of the sub-setted object.
#'
#'
#' @examples
#' 
#' # atomic objects ====
#' 
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_rm(obj, 1:3, 1:3)
#' # above is equivalent to  obj[-1:-3, -1:-3, drop = FALSE]
#' sb_rm(obj, i = \(x)x>5)
#' # above is equivalent to  obj[!obj > 5]
#' sb_rm(obj, col = "a")
#' # above is equivalent to  obj[, which(!colnames(obj) %in% "a")]
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' sb_rm(obj, n(1, c(1, 3)), c(1, 3))
#' sb_rm(obj, rcl = n(1, NULL, c(1, 3)))
#' # above 2 lines are equivalent to obj[-1, c(-1, -3), drop = FALSE]
#' sb_rm(obj, i = \(x)x>5)
#' # above is equivalent to obj[!obj > 5]
#' 
#' #############################################################################
#' 
#' 
#' # lists ====
#' 
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_rm(obj, "a")
#' # above is equivalent to obj[which(!names(obj) %in% "a")]
#' sb_rm(obj, 1) # obj[-1]
#' sb_rm(obj, 1:2)
#' # above is equivalent to obj[seq_len(length(obj))[-1:-2]]
#' sb_rm(obj, is.numeric, drop = TRUE)
#' # above is equivalent to obj[[!sapply(obj, is.numeric)]] IF this returns a single element
#' obj <- list(a = 1:10, b = letters[1:11], c = letters)
#' sb_rm(obj, is.numeric)
#' # above is equivalent to obj[!sapply(obj, is.numeric)] # this time singular brackets?
#' # for recusive indexing, see sb_rec()
#' 
#' #############################################################################
#' 
#' 
#' # factors ====
#' 
#' obj <- factor(rep(letters[1:5], 2))
#' sb_rm(obj, lvl = "a")
#' # above is equivalent to obj[which(!obj %in% "a")]
#' 
#' #############################################################################
#' 
#' 
#' # data.frame-like objects ====
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' print(obj)
#' sb_rm(obj, 1:3, 1:3)
#' # above is equivalent to obj[-1:-3, -1:-3, drop = FALSE]
#' sb_rm(obj, filter = ~ (a > 5) & (c < 19), vars = is.numeric)
#' 
#'
#'

#' @rdname sb_rm
#' @export
sb_rm <- function(x, ...) {
  UseMethod("sb_rm", x)
}


#' @rdname sb_rm
#' @export
sb_rm.default <- function(x, i, ..., rat = FALSE, chkdup = TRUE) {
  elements <- .indx_make_element(i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
  if(rat) {
    x <- .fix_attr(x[elements], attributes(x))
  } else{ x <- x[elements] }
  return(x)
}


#' @rdname sb_rm
#' @export
sb_rm.matrix <- function(
    x, row = NULL, col = NULL, i = NULL, ..., rat = FALSE, chkdup = TRUE
) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
    return(x[elements])
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
  }
  
  if(is.null(row) && is.null(col)) {
    return(x)
  }
  if(is.null(row)) {
    if(rat) {
      x <- .fix_attr(x[, col, drop = FALSE], attributes(x))
    } else{ x <- x[, col, drop = FALSE] }
    
    return(x)
  }
  if(is.null(col)) {
    if(rat) {
      x <- .fix_attr(x[row, , drop = FALSE], attributes(x))
    } else{ x <- x[row, , drop = FALSE] }
    
    return(x)
  }
  
  if(rat) {
    x <- .fix_attr(x[row, col, drop = FALSE], attributes(x))
  } else{ x <- x[row, col, drop = FALSE] }
  
  return(x)
}


#' @rdname sb_rm
#' @export
sb_rm.array <- function(
    x, idx = NULL, dims = NULL, rcl = NULL, i = NULL, ..., rat = FALSE, chkdup = TRUE
) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element(
      i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call()
    )
    return(x[elements])
  }
  
  
  if(!is.null(rcl)) {
    if(length(dim(x)) != 3) stop("`rcl` only applicable for arrays with exactly 3 dimensions")
    if(!is.list(rcl) || length(rcl) != 3) stop("`rcl` must be a list of length 3")
    
    x <- .sb3d_rm(
      x, rcl[[1]], rcl[[2]], rcl[[3]], rat = rat, chkdup = chkdup, abortcall = sys.call()
    )
    return(x)
  }
  
  if(rat) {
    x <- .fix_attr(.arr_rm(x, idx, dims, chkdup = chkdup, abortcall = sys.call()), attributes(x))
  } else{ x <- .arr_rm(x, idx, dims, chkdup = chkdup, abortcall = sys.call()) }
  
  return(x)
}


#' @rdname sb_rm
#' @export
sb_rm.factor <- function(x, i = NULL, lvl = NULL, drop = FALSE, ..., rat = FALSE, chkdup = TRUE) {
  .check_args_factor(i, lvl, drop, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
    if(rat) {
      x <- .fix_attr(x[elements, drop = drop], attributes(x))
    } else{ x <- x[elements, drop = drop] }
    return(x)
  }
  if(!is.null(lvl)) {
    indx <- .lvl2indx(lvl, x, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
    if(rat) {
      x <- .fix_attr(x[indx, drop = drop], attributes(x))
    } else{ x <- x[indx, drop = drop] }
    return(x)
  }
}

#' @rdname sb_rm
#' @export
sb_rm.list <- function(x, i, drop = FALSE, ..., rat = FALSE, chkdup = TRUE) {
  
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop("`drop` must be either `TRUE` or `FALSE`")
  }
  
  elements <- .indx_make_element(i, x, is_list = TRUE, chkdup = chkdup, inv = TRUE, abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && drop) {
    x <- x[[elements]]
  } else {
    if(rat) x <- .fix_attr(x[elements], attributes(x))
    if(!rat) x <- x[elements]
  }
  return(x)
}


#' @rdname sb_rm
#' @export
sb_rm.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ...,
    chkdup = TRUE
) {
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, chkdup = chkdup, inv = TRUE, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, chkdup = chkdup, inv = TRUE, abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = TRUE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = TRUE, abortcall = sys.call())
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  out <- collapse::ss(x, row, col, check = FALSE)
  
  names(out) <- make.names(names(out), unique = TRUE)
  return(out)
}


