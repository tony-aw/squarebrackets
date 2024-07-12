#' Method Dispatch of 'squarebrackets'
#'
#' @description
#' 
#' This help page gives some additional details regarding the S3 method dispatch
#' used in 'squarebrackets'. \cr \cr
#' 
#' 
#' @section Non-Recursive vs Recursive:
#' With the exception of the \link{idx} method,
#' the main generic methods are available in 2 forms: \cr
#' The Non-Recursive form (`sb_`), and the Recursive Form (`sb2_`). \cr
#' This because some S3 classes are available in both atomic and recursive forms. \cr
#' \cr
#' For example,
#' the \link{array} S3 class and the \link{matrix} S3 class
#' (which inherits from the "array" S3 class)
#' have both an atomic form, and a recursive form. \cr
#' The recursive form of arrays and matrices is sometimes referred to as a "dimensional list". \cr
#' \cr
#' Recursive and non-recursive objects are quite different from each other in some ways:
#' 
#'  - **homo- or heterogeneous**: atomic object are homogeneous,
#'  in that they can only contain one data-type
#'  (logical, integer, double, character, complex, raw). \cr
#'  In contract, recursive objects are heterogeneous,
#'  as they can have any combination of data-types.
#'  - **vectorization**: vectorized operations generally work on atomic objects,
#'  whereas recursive objects generally require loops or apply-like functions. \cr
#'  This is especially relevant for transforming subsets.
#'  - **recursive subsets**: Recursive objects distinguish between "regular" subset operations
#'  (in base R using `[`, `[<-`),
#'  and recursive subset operations (in base R using `[[`, `[[<-`). \cr
#'  For both forms, atomic objects give atomic objects. \cr
#'  But for recursive objects,
#'  these 2 subset operations are significantly different.
#'  - **views**: Recursive objects are weird in that they are *pointers* to other objects. \cr
#'  As such they allow their extracted subsets to be *views* of these pointers
#'  (see \link{squarebrackets_mutable_classes}
#'  for more information on how to use "views" of recursive objects). \cr
#'  Atomic objects do now allow for subset views. \cr
#' 
#' The S3 method dispatch system does not have a built-in method to have separate dispatches for recursive and atomic objects. \cr
#' Hence, given all the above,
#' the 'squarebrackets' package
#' gives separate methods for recursive and non-recursive objects. \cr \cr
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

#' @rdname aaa6_squarebrackets_method_dispatch
#' @name aaa6_squarebrackets_method_dispatch
#' @aliases squarebrackets_method_dispatch
NULL
