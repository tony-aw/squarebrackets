#' Formula with Keywords
#'
#' @description
#' Instead of the usual indexing types,
#' formulas can be used to specify indices in vectors, arrays, or data.frames that can evaluate keywords. \cr
#' \cr
#' The following keywords are available:
#' 
#'  - `.M`: the given margin/dimension; `0` if not relevant.
#'  - `.Nms`: the (dim)names at the given margin.
#'  - `.N`: the size of a given dimension (if `.M` is not `0`) or else the length of `x`.
#'  - `.I`: equal to `1:.N`.
#'  - `.bi(...)`: a \bold{function} to specify bilateral indices. See the `Details` section below.
#'  - `.x`: the input variable `x` itself.
#'  
#' 
#' These keywords can be used inside formulas to specify more advanced indices. \cr
#' 
#' Here are a few examples of advanced indexing in arrays:
#' 
#' ```{r, echo = TRUE, eval = TRUE}
#' 
#' x <- matrix(1:20, 5, 4)
#' dimnames(x) <- list(month.abb[1:5], month.abb[1:4])
#' print(x)
#' 
#' 
#' # select first half of rows, select first & last column:
#' ss_x(x, n(~ 1:round(.N/2), ~ .bi(1, -1)))
#' 
#' 
#' # extract where rownames contain "r" & where colnames DO NOT contain "r":
#' ss_x(x, ~ grep("r", .Nms), use = c(1, -2)) 
#' 
#' 
#' # reverse order of all dimensions:
#' ss_x(x, ~ .bi(-.I))
#' 
#' ```
#' 
#' @details
#' \bold{Bilateral Indices} \cr
#' The `.bi(...)` function is used to specify bilateral indices. \cr
#' One can specify both positive and negative numbers. \cr
#' Positive numbers (for example `~ .bi(1:10)`) specify indices work like normally. \cr
#' Negative numbers specify indices from the end; \cr
#' i.e. `~ .bi(-2)` would be equivalent to `length(x) - 1` if `x` is a vector,
#' or `dim(x)[.M] - 1` if `x` is an array. \cr
#' \cr
#' The usage of `.bi(...)` is similar to the `c(...)` function,
#' in that multiple vectors can be concatenated to one. \cr
#' And one can specify both negative and positive numbers together. \cr
#' For example: `~ .bi(-10:-1, 10:1)`. \cr
#' \cr
#' One can also use the other keywords inside `.bi()`. \cr
#' For example, `~ .bi(-.I)` can be used to reverse a vector,
#' or reverse a dimension of an array or data.frame. \cr
#' \cr
#' 
#' 
#' @rdname aaa11_squarebrackets_keywords
#' @name aaa11_squarebrackets_keywords
#' @aliases squarebrackets_keywords
#' @aliases keywords
NULL


