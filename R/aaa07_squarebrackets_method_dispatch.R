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
#'  See for example the \link{lst_rec} method,
#'  or the `red = TRUE` argument in the \eqn{\ast}`_x` and \eqn{\ast}`_wo` methods. \cr
#'  For atomic objects, these 2 have no meaningful difference
#'  (safe for perhaps some minor attribute handling). \cr
#'  - **views**: For recursive objects,
#'  one can create a View of a recursive subset
#'  (see the documentation in the 'mutatomic' package for details). \cr
#'  Subset views do not exist for atomic objects. \cr \cr
#'  
#'
#' 
#' @section Ellipsis:
#' Due to how the S3 method dispatch system works in 'R',
#' all generic methods have the ellipsis argument (`...`). \cr
#' For the user's safety,
#' 'squarebrackets' does check that the user doesn't accidentally
#' add arguments that make no sense for that method
#' (like specifying the `inv` argument when calling \link{fi_x}). \cr
#' \cr
#' \cr
#' 
#' 
#' 
#' 
#' 

#' @rdname aaa07_squarebrackets_method_dispatch
#' @name aaa07_squarebrackets_method_dispatch
#' @aliases squarebrackets_method_dispatch
NULL
