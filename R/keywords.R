#' Formula with Keywords
#'
#' @description
#' Instead of the usual indexing types,
#' formulas can be used to specify indices in arrays that can evaluate keywords. \cr
#' \cr
#' The following keywords are available:
#' 
#'  - `.M`: the given margin/dimension; `0` if not relevant.
#'  - `.Nms`: the (dim)names at the given margin.
#'  - `.N`: the size of a given dimension (if `.M` is not `0`) or else the length of `x`.
#'  - `.I`: equal to `1:.N`.
#'  - `.bi(...)`: a \bold{function} to specify bilateral indices.
#'  - `.x`: the input variable `x` itself.
#'  Here one can specify both positive and negative numbers. \cr
#'  Positive numbers specify indices work like normally. \cr
#'  Negative numbers specify indices from the end. \cr
#' 
#' These keywords can be used inside formulas to specify more advanced indices. \cr
#' 
#' Here are a few examples of advanced indexing in arrays:
#' 
#' ```{r, echo = TRUE, eval = TRUE}
#' x <- matrix(1:20, 5, 4)
#' dimnames(x) <- list(month.abb[1:5], month.abb[1:4])
#' print(x)
#' 
#' 
#' ss_x(x, ~ 1:round(.N/2)) # select first half of rows and columns
#' 
#' ss_x(x, ~ grep("a", .Nms)) # select subset whose dimnames contains an "a"
#' 
#' ss_x(x, ~ .bi(1, -1), -Inf) # remove first and last index of every dimension 
#' 
#' ```
#' 
#' 
#'
#' @returns
#' The `is_formula()` function returns `TRUE` if the input is a formula,
#' and `FALSE` otherwise. \cr \cr
#'
#'
#'
#' @examples
#' x <- matrix(1:20, 5, 4)
#' dimnames(x) <- list(month.abb[1:5], month.abb[1:4])
#' print(x)
#' 
#' 
#' ss_x(x, ~ 1:round(.N/2)) # select first half of rows and columns
#' 
#' ss_x(x, ~ grep("a", .Nms)) # select subset whose dimnames contains an "a"
#' 
#' ss_x(x, ~ .bi(c(1, -1)), -Inf) # remove first and last index of every dimension 
#' 
#' 
#' 


#' @name keywords
NULL


#' @rdname keywords
#' @export
is.formula <- function(form) {
  check <- inherits(form, "formula") && is.call(form) && isTRUE(form[[1]] == "~")
  return(check)
}
