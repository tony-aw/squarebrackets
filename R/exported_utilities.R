#' Exported Utilities
#'
#' @description
#' Exported utilities. \cr
#' Usually the user won't need these functions. \cr \cr
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param i See \link{squarebrackets_indx_args}.
#' @param xnames names or dimension names
#' @param xsize length or dimension size
#'
#' @returns
#' The subsetted object.
#' 
#' 
#' @examples
#' x <- 1:10
#' names(x) <- letters[1:10]
#' indx_x(1:5, x, names(x), length(x))
#' indx_wo(1:5, x, names(x), length(x))
#'
#'

#' @rdname exported_uilities
#' @export
indx_x <- function(i, x, xnames, xsize) {
  if(is.null(i) || .is.0(i)) return(base::quote(expr = ))
  if(is.function(i)) return(which(lapply(x, i) |> unname() |> unlist()))
  if(length(i)==0) return(numeric(0)) 
  if(is.character(i)) {
    out <- lapply(
      i, \(i) which(xnames == i)
    ) |> unlist()
    return(out)
  }
  if(is.logical(i)) return(which(i))
  if(is.complex(i)) {
    i <- Im(i)
    return(data.table::fifelse(
      i < 0, xsize + i + 1L,
      i
    ))
  }
  if(is.numeric(i)) return(i)
}


#' @rdname exported_uilities
#' @export
indx_wo <- function(i, x, xnames, xsize) {
  if(is.null(i) || .is.0(i)) return(base::quote(expr = ))
  if(is.function(i)) return(which(!(lapply(x, i) |> unname() |> unlist())))
  if(length(i)==0) return(seq_len(xsize)) 
  if(is.character(i)) {
    return(which(!(xnames %in% i)))
  }
  if(is.logical(i)) return(which(!i))
  
  if(is.complex(i)) {
    i <- Im(i)
    i <- data.table::fifelse(
      i < 0, xsize + i + 1L,
      i
    )
  }
  if(is.numeric(i)) return(seq_len(xsize)[-i])
}


#' @rdname exported_uilities
#' @export
.is.0 <- function(x) {
  length(x) == 1L && is.numeric(x) && x == 0L
}
