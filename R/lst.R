#' Unnest Tree-like List to Recursive 2d Array or Flattened Recursive Vector
#'
#' @description
#' `[[`, `[[<-`, \link{sb2_rec}, and \link{sb2_reccom},
#' can performing recursive subset operation on a nested list. \cr
#' Such recursive subset operations only operate on a single element. \cr
#' Performing recursive subset operations on multiple elements is not vectorized,
#' and requires a (potentially slow) loop. \cr
#' \cr
#' The `lst_untree()` function takes a nested tree-like list,
#' and turns it into a 2d recursive array (i.e. a list-matrix),
#' allowing vectorized subset operations to be performed on the list. \cr
#' `lst_untree()` can also simply flatten the list, making it a non-nested list. \cr
#' See the Examples section to understand how the list will be arranged and named. \cr
#' \cr
#' The `lst_nlists()` counts the total number of recursive list-elements inside a list. \cr \cr
#' 
#' @param x a tree-like nested list.
#' @param margin a single integer, indicating how the result should be arranged:
#'  * `margin = 0` produces a simple flattened recursive vector (i.e. list) without dimensions.
#'  * `margin = 1` produces a 2D recursive array (i.e. a matrix of lists), \cr
#'  with `length(x)` rows and `n` columns, \cr
#'  where \code{n = sapply(x, }\link{lst_nlists}\code{) |> max()}. \cr
#'  Empty elements will be filled with `list(NULL)`.
#'  * `margin = 2` produces a 2D recursive array (i.e. a matrix of lists), \cr
#'  with `length(x)` columns and `n` rows, \cr
#'  where \code{n = sapply(x, }\link{lst_nlists}\code{) |> max()}. \cr
#'  Empty elements will be filled with `list(NULL)`.
#' @param use.names Boolean,
#'  indicating if the elements returned from `lst_untree()` should be named. \cr
#'  Names of nested elements, such as `x[[c("A", "B", "C")]]`,
#'  will become `"A.B.C"`,
#'  as that is the behaviour of the \link[base]{rapply} function
#'  (which `lst_untree()` calls internally). \cr
#'  It is therefore advised not to use dots (`"."`) in your list names,
#'  and use underscores (`"_"`) instead,
#'  before calling `lst_untree()`. \cr
#'  See the \code{rrapply::}\link[rrapply]{rrapply} function
#'  for renaming
#'  (and other forms of transforming)
#'  recursive subsets of lists. \cr \cr
#'  
#' 
#'
#' @returns
#' For `lst_untree()`: \cr
#' A non-nested (dimensional) list. \cr
#' Note that if `margin = 1` or `margin = 2`, `lst_untree()` returns a recursive matrix
#' (i.e. a recursive array with 2 dimensions),
#' \bold{not} a data.frame. \cr
#' (One advantage of a recursive matrix over a data.frame,
#' is that a recursive matrix can have separate column names and regular names,
#' whereas the names of a data.frame are necessarily equal to the column names). \cr
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
lst_untree <- function(x, margin, use.names = FALSE) {
  
  if(!.lst_is.treelist(x)) {
    stop("`x` must be a tree-like nested list")
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
    dimcumprod <- cumprod(dim(out))
    
    if(use.names) {
      
      names(out) <- character(length(out))
      
      for(i in seq_along(input)) {
        mypointer <- input[[i]]
        indx <- .rcpp_sub2ind_2d(i, seq_along(mypointer), dimcumprod)
        out[indx] <- mypointer
        if(!is.null(names(mypointer))) {
          names(out)[indx] <- names(mypointer)
        }
      }
      rownames(out) <- names(input)
    } else {
      for(i in seq_along(input)) {
        mypointer <- input[[i]]
        out[i, seq_along(mypointer)] <- mypointer
      }
    }
    
    return(out)
  }
  
  if(margin == 2) {
    
    dim(out) <- c(maxlen, length(input))
    dimcumprod <- cumprod(dim(out))
    
    if(use.names) {
      names(out) <- character(length(out))
      
      for(i in seq_along(input)) {
        mypointer <- input[[i]]
        indx <- .rcpp_sub2ind_2d(seq_along(mypointer), i, dimcumprod)
        out[indx] <- mypointer
        if(!is.null(names(mypointer))) {
          names(out)[indx] <- names(mypointer)
        }
      }
      colnames(out) <- names(input)
    } else {
      for(i in seq_along(input)) {
        mypointer <- input[[i]]
        out[seq_along(mypointer), i] <- mypointer
      }
    }
    
    return(out)
  }
  else {
    stop(simpleError("`margin` must be 0, 1, or 2", call = abortcall))
  }
}
