#' Compute Ordered Indices
#'
#' @description
#' Computes ordered indices.
#' Similar to \link[base]{order},
#' except the user must supply a vector,
#' a list of equal-length vectors,
#' a data.frame or a matrix (row-wise and column-wise are both supported),
#' as the input. \cr
#' \cr
#' For a vector `x`,
#' `idx_ord_v(x)` is equivalent to \link[base]{order}\code{(x)}. \cr
#' \cr
#' For a data.frame or a list of equal-length vectors `x`, with `p` columns/elements,
#' `idx_ord_df(x)` is equivalent to `order(x[[1]], ..., x[[p]])`. \cr
#' \cr
#' For a matrix (or array) `x` with `p` rows,
#' `idx_ord_m(x, margin = 1)` is equivalent to `order(x[1, ], ..., x[p, ], ...)`. \cr
#' \cr
#' For a matrix (or array) `x` with `p` columns,
#' `idx_ord_m(x, margin = 2)` is equivalent to `order(x[, 1], ..., x[, p], ...)`. \cr
#' \cr
#' Note that these are merely a convenience functions,
#' and that these are actually slightly slower than \link[base]{order} (except for `idx_ord_v()`),
#' due to the additional functionality.
#' 
#' 
#' @param x a vector, data.frame, or array
#' @param margin the margin over which to cut the matrix/array into vectors. \cr
#' I.e. `margin = 1` will cut `x` into individual rows,
#' and apply the \link[base]{order} on those rows. \cr
#' And `margin = 2` will cut `x` into columns, etc.
#' @param decr see argument `decreasing` in \link[base]{order}
#' @param na.last,method see \link[base]{order} and \link[base]{sort}.
#' 
#' 
#' @returns
#' See \link[base]{order}.
#'
#'
#'
#' @examples
#' 
#' # vectors ====
#' x <- sample(1:10)
#' order(x)
#' idx_ord_v(x)
#' idx_ord_m(rbind(x,x), 1)
#' idx_ord_m(cbind(x,x), 2)
#' idx_ord_df(data.frame(x,x))
#'  

#' @rdname idx_ord
#' @export
idx_ord_v <- function(x, na.last = TRUE, decr = FALSE,
                    method = c("auto", "shell", "radix")) {
  return(order(
    x, na.last = na.last, decreasing = decr, method = method
  ))
}


#' @rdname idx_ord
#' @export
idx_ord_m <- function(x, margin, na.last = TRUE, decr = FALSE,
                      method = c("auto", "shell", "radix")) {
  x <- apply(x, margin, \(x) x, simplify = FALSE)
  args <- list(na.last = na.last, decreasing = decr, method = method)
  return(do.call(order, c(x, args)))
}


#' @rdname idx_ord
#' @export
idx_ord_df <- function(x, na.last = TRUE, decr = FALSE,
                       method = c("auto", "shell", "radix")) {
  if(!is.list(x)) {
    stop("`x` must be a data.frame or list")
  }
  args <- list(na.last = na.last, decreasing = decr, method = method)
  return(do.call(order, c(x, args)))
}
