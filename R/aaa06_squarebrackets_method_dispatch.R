#' Method Dispatch of 'squarebrackets'
#'
#' @description
#' 
#' This help page gives some additional details regarding the S3 method dispatch
#' used in 'squarebrackets'. \cr \cr
#' 
#' 
#' @section Atomic vs Recursive:
#' Atomic and recursive objects are quite different from each other in some ways:
#' 
#'  - **homo- or heterogeneous**: an atomic object can only have values of one data type. \cr
#'  recursive objects can hold values of any combination of data types. \cr
#'  - **nesting**: Recursive objects can be nested, while atomic objects cannot be nested.
#'  - **copy and coercion effect**: One can coerce or copy a subset of a recursive object,
#'  without copying the rest of the object. \cr
#'  For atomic objects, however, a coercion or copy operation coerces or copies the entire vector
#'  (ignoring attributes). \cr
#'  - **vectorization**: most vectorized operations generally work on atomic objects,
#'  whereas recursive objects often require loops or apply-like functions. \cr
#'  - **recursive subsets**: Recursive objects distinguish between "regular" subset operations
#'  (in base R using `[`, `[<-`),
#'  and recursive subset operations (in base R using `[[`, `[[<-`). \cr
#'  See for example the \link{sb2_rec} method,
#'  or the `red = TRUE` argument in the \link{sb2_x} and \link{sb2_wo} methods. \cr
#'  For atomic objects, these 2 have no meaningful difference
#'  (safe for perhaps some minor attribute handling). \cr
#'  - **views**: For recursive objects,
#'  one can create a \link[=mutatomic_coercion]{view} of a recursive subset. \cr
#'  Subset views do not exist for atomic objects. \cr \cr
#' 
#' Despite these non-trivial differences,
#' the S3 method dispatch does not distinguish between atomic and recursive objects. \cr
#' I.e. S3 methods check if an object is, for example, an array,
#' but not if it is an  atomic array or a recursive array. \cr
#' (S3 method dispatch actually does distinguish between basic atomic and recursive vectors,
#' but not for dimensional objects like arrays,
#' which is problematic for this specific package). \cr
#' Therefore, the methods in 'squarebrackets'
#' that perform subset operations on an object,
#' come in the atomic (`sb_`) and recursive (`sb2_`) form. \cr
#' The \link{idx} method operates on the indices of an object,
#' but does not operate on the object itself,
#' and so has no distinction between the atomic and recursive form. \cr
#' \cr
#'  
#'  
#' @section Manual Dispatch:
#' The 'squarebrackets' package intentionally exports each function in its S3 method dispatch system. \cr
#' This is handy for programming purposes. \cr
#' For example: one can explicitly alias a specific dispatch of a method,
#' if one so desires. \cr
#' For example like so: \cr
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' array_x <- function(x, ...) {
#' 
#'    if(is.atomic(x)) {
#'      sb_x.array(x, ...)
#'    }
#'    else if(is.recursive(x)) {
#'      sb2_x.array(x, ...)
#'    }
#'    
#' }
#' 
#' ```
#' 
#' Under certain circumstances, this might help your code to be more clear. \cr
#' \cr
#' 
#' 
#' @section Ellipsis:
#' Due to how the S3 method dispatch system works in 'R',
#' all generic methods have the ellipsis argument (`...`). \cr
#' For the user's safety,
#' 'squarebrackets' does check that the user doesn't accidentally
#' add arguments that make no sense for that method
#' (like specifying the `inv` argument when calling \link{sb_x}). \cr
#' \cr
#' \cr
#' 
#' 
#' 
#' 
#' 

#' @rdname aaa06_squarebrackets_method_dispatch
#' @name aaa06_squarebrackets_method_dispatch
#' @aliases squarebrackets_method_dispatch
NULL
