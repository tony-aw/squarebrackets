#' Method Dispatch of 'squarebrackets'
#'
#' @description
#' 
#' This help page gives some additional details regarding the S3 method dispatch
#' used in 'squarebrackets'. \cr \cr
#' 
#' 
#' @section Atomic vs Recursive:
#' With the exception of the \link{idx} method,
#' the main generic methods are available in 2 forms: \cr
#' The Atomic form (`sb_`), and the Recursive Form (`sb2_`). \cr
#' This because some S3 classes are available in both atomic and recursive forms. \cr
#' \cr
#' For example,
#' the \link{array} S3 class and the \link{matrix} S3 class
#' (which inherits from the "array" S3 class)
#' have both an atomic form, and a recursive form. \cr
#' \cr
#' Atomic and recursive objects are quite different from each other in some ways:
#' 
#'  - **homo- or heterogeneous**: an atomic object can only have values of one data type. \cr
#'  recursive objects can hold values of any combination of data types.
#'  - **copy semantics**: One can copy a subset of a recursive object without copying the rest of the object
#'  (thus saving memory when copying).
#'  For atomic objects, however, a copy operation copies the entire vector (ignoring attributes). \cr
#'  - **vectorization**: most vectorized operations generally work on atomic objects,
#'  whereas recursive objects often require loops or apply-like functions.
#'  - **recursive subsets**: Recursive objects distinguish between "regular" subset operations
#'  (in base R using `[`, `[<-`),
#'  and recursive subset operations (in base R using `[[`, `[[<-`). \cr
#'  For atomic objects, these 2 have no meaningful difference
#'  (safe for perhaps attribute handling).
#'  - **views**: For recursive objects,
#'  one can create a subset \link[=squarebrackets_PassByReference]{view} of a recursive subset. \cr
#'  Recursive subset views do not exist for atomic objects. \cr \cr
#' 
#' Despite these differences, 
#' R'S S3 method dispatch system does not have a built-in method to have separate dispatches for recursive and atomic objects. \cr
#' Hence, given all the above,
#' the 'squarebrackets' package
#' has separate methods for recursive and atomic objects. \cr \cr
#' 
#' 
#' @section Manual Dispatch:
#' The 'squarebrackets' package intentionally exports each function in its S3 method dispatch system. \cr
#' This is handy for programming purposes. \cr
#' For example: atomic matrices and atomic arrays each have their own dispatch. \cr
#' Thus, when looping though matrices and arrays to extract some elements,
#' it may be easier to treat them all as arrays (remember that matrices inherit from arrays). \cr
#' Thus one can use `sb_x.array()` to ensure the "array" method is used,
#' instead of the "matrix" method. \cr
#' \cr
#' Another advantage is that one can alias a specific dispatch of a method,
#' if one so desires. \cr
#' I.e.: `array2_x <- sb2_x.array`. \cr
#' Under certain circumstances, this may help your code to be more clear. \cr \cr
#' 
#' 
#' @section Ellipsis:
#' Due to how the S3 method dispatch system works in 'R',
#' all generic methods have the ellipsis argument (`...`). \cr
#' For the user's safety,
#' 'squarebrackets' does check that the user doesn't accidentally
#' add arguments that make no sense for that method
#' (like specifying the `inv` argument when calling \link{sb_x}). \cr
#' 
#' 

#' @rdname aaa4_squarebrackets_method_dispatch
#' @name aaa4_squarebrackets_method_dispatch
#' @aliases squarebrackets_method_dispatch
NULL
