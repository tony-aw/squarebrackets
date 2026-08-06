#' Array Shadow
#'
#' @description
#' The Array Shadow object is a "shadow" of an vector or array `x`: \cr
#' It has the same (dim)names as `x`, and pretends to have the same length and dimensions. \cr
#' But it holds no actual data. \cr
#' 
#' @param type the type of the vector/array (see \link[base]{atomic} and \link[base]{list}).
#' @param length the length of the vector/array.
#' @param names the names of the vector/array.
#' @param dim the dimensions of the vector/array.
#' @param dimnames the `dimnames` of the vector/array.
#' @param oldClass the class attribute of the vector/array.
#' @param x_expr the substituted name of the vector/array.
#' @param env the environment where the vector/array name (`x_expr`) actually lives. \cr
#' 
#' 
#' 
#' @returns
#' The "shadow" of the vector/array.
#'
#'
#' @example inst/examples/arrayshadow.R
#' 


#' @name ArrayShadow
NULL



#' @rdname ArrayShadow
#' @export
cast_ArrayShadow <- function(type, length, dim, names, dimnames, oldClass) {
  structure(vector(type, 1L), 
            shadow_len = length,
            shadow_dim = dim,
            shadow_nms = names,
            shadow_dimnames = dimnames, 
            class = c("ArrayShadow", oldClass))
}


#' @rdname ArrayShadow
#' @export
cast_ArrayShadow2 <- function(x_expr, env) {
  
  meta_expr <- substitute(
    list(
      type = base::typeof(x_expr),
      length = base::length(x_expr),
      dim = base::dim(x_expr),
      names = base::names(x_expr),
      dimnames = base::dimnames(x_expr),
      oldClass = base::oldClass(x_expr)
    ), 
    list(x_expr = x_expr)
  )
  x_meta <- eval(meta_expr, envir = env)
  x_type  <- x_meta$type
  x_len <- x_meta$len
  x_dim <- x_meta$dim
  x_names <- x_meta$names
  x_dimnames <- x_meta$dimnames
  x_oldClass <- x_meta$oldClass
  
  x_shadow <- cast_ArrayShadow(
    x_type, x_len, x_dim, x_names, x_dimnames, x_oldClass
  )
  return(x_shadow)
}




