#' Unnest Tree-like List into a Recursive Matrix or Flattened Recursive Vector
#'
#' @description
#' `[[`, `[[<-`, \link{sb2_rec}, and \link{sb2_recin},
#' can perform recursive subset operations on a nested list. \cr
#' Such recursive subset operations only operate on a single element. \cr
#' Performing recursive subset operations on multiple elements is not vectorized,
#' and requires a (potentially slow) loop. \cr
#' \cr
#' The `lst_untree()` function takes a nested tree-like list,
#' and turns it into a recursive matrix (a matrix of list-elements),
#' allowing vectorized subset operations to be performed on the nested list. \cr
#' `lst_untree()` can also simply flatten the list, making it a non-nested list. \cr
#' See the Examples section to understand how the list will be arranged and named. \cr
#' \cr
#' The `lst_nlists()` counts the total number of recursive list-elements inside a list. \cr \cr
#' 
#' @param x a tree-like nested list.
#' @param margin a single integer, indicating how the result should be arranged:
#'  * `margin = 0` produces a simple flattened recursive vector (i.e. list) without dimensions.
#'  * `margin = 1` produces a recursive matrix (i.e. a matrix of list-elements), \cr
#'  with `length(x)` rows and `n` columns, \cr
#'  where \code{n = sapply(x, }\link{lst_nlists}\code{) |> max()}. \cr
#'  Empty elements will be filled with `list(NULL)`.
#'  * `margin = 2` produces a recursive matrix (i.e. a matrix of list-elements), \cr
#'  with `length(x)` columns and `n` rows, \cr
#'  where \code{n = sapply(x, }\link{lst_nlists}\code{) |> max()}. \cr
#'  Empty elements will be filled with `list(NULL)`.
#' @param use.names Boolean, indicating if the result should be named. \cr
#' See section "use.names" for more information.
#' 
#' 
#' @section use.names:
#' \bold{`margin = 0` and `use.names = TRUE`} \cr
#' If `margin = 0` and `use.names = TRUE`,
#' every element in the flattened list will be named. \cr
#' Names of nested elements, such as `x[["A"]][["B"]][["C"]]`,
#' will become `"A.B.C"`,
#' as that is the behaviour of the \link[base]{rapply} function
#' (which `lst_untree()` calls internally). \cr
#' It is therefore advised not to use dots (`"."`) in your list names,
#' and use underscores (`"_"`) instead,
#' before calling `lst_untree()`. \cr
#' See the \code{rrapply::}\link[rrapply]{rrapply} function
#' for renaming
#' (and other forms of transforming)
#' recursive subsets of lists. \cr
#' \cr
#' \bold{`margin = 1` and `use.names = TRUE`} \cr
#' If `margin == 1` and `use.names = TRUE`,
#' the rows of resulting recursive matrix will be equal to `names(x)`,
#' but recursive names will not be assigned. \cr
#' \cr
#' \bold{`margin = 2` and `use.names = TRUE`} \cr
#' If `margin == 2` and `use.names = TRUE`,
#' the columns of resulting recursive matrix will be equal to `names(x)`,
#' but recursive names will not be assigned. \cr
#' \cr
#' \bold{`use.names = FALSE`} \cr
#' If `use.names = FALSE`, the result will not have any names assigned at all. \cr
#' \cr
#'  
#'
#' @returns
#' For `lst_untree()`: \cr
#' A non-nested (dimensional) list. \cr
#' Note that if `margin = 1` or `margin = 2`, `lst_untree()` returns a recursive matrix
#' (i.e. a recursive array with 2 dimensions),
#' \bold{not} a data.frame. \cr
#' To turn a nested list into a data.frame instead, one option would be to use: \cr
#' \link[rrapply]{rrapply}\code{(x, how = "melt")} \cr
#' \cr
#' For `lst_nlists()`: \cr
#' A single integer,
#' giving the total number of recursive list-elements in the given list. \cr \cr
#' 
#'
#'
#' @example inst/examples/lst.R
#'


#' @name lst
NULL


#' @rdname lst
#' @export
lst_nlists <- function(x) {
  sum(rapply(x, function(x) 1L, classes = "ANY"))
}


#' @keywords internal
#' @noRd
.lst_is.treelist <- function(x) {
  return(is.list(x) && all(vapply(x, is.list, logical(1)) |> unlist()))
}

#' @rdname lst
#' @export
lst_untree <- function(x, margin, use.names = TRUE) {
  
  if(!.lst_is.treelist(x)) {
    stop("`x` must be a tree-like nested list")
  }
  if(length(margin) != 1L || !is.numeric(margin)) {
    stop("`margin` must be a single integer")
  }
  if(!margin %in% 0:2) {
    stop("`margin` must be 0, 1, or 2")
  }
  
  if(margin == 0) {
    return(.lst_flatten(x, use.names = use.names))
  }
  
  
  out <- vector(mode = "list", length = length(x))
  for(i in seq_along(x)) {
    out[[i]] <- .lst_flatten(x[i], use.names = use.names)
  }
  names(out) <- names(x)
  
  return(.lst_untree_dim(out, margin, use.names, abortcall = sys.call()))
  
}

#' @keywords internal
#' @noRd
.lst_flatten <- function(x, use.names) {
  len <- lst_nlists(x)
  y <- vector("list", len)
  i <- 1L
  items <- rapply(x, function(x) {
    # changing be reference here, as y is a completely new recursive vector
    collapse::setv(y, i, list(x), vind1 = TRUE, xlist = TRUE)
    i <<- i + 1L
    return(TRUE)
  }, classes = "ANY")
  if(use.names && !is.null(names(items))) names(y) <- names(items)
  return(y)
}

#' @keywords internal
#' @noRd
.lst_untree_dim <- function(input, margin, use.names, abortcall) {
  maxlen <- max(collapse::vlengths(input))
  len <- max(collapse::vlengths(input)) * length(input)
  out <- vector(mode = "list", length = len)
  
  if(margin == 1) {
    
    dim(out) <- c(length(input), maxlen)
    
    for(i in seq_along(input)) {
      myref <- input[[i]]
      out[i, seq_along(myref)] <- myref
    }
    if(use.names) {
      rownames(out) <- names(input)
    }
    
    return(out)
  }
  
  if(margin == 2) {
    
    dim(out) <- c(maxlen, length(input))
    
    for(i in seq_along(input)) {
      myref <- input[[i]]
      out[seq_along(myref), i] <- myref
    }
    if(use.names) {
      colnames(out) <- names(input)
    }
    
    return(out)
  }
  else {
    stop(simpleError("`margin` must be 0, 1, or 2", call = abortcall))
  }
}
