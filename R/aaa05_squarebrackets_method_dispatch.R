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
#'  recursive objects can hold values of any combination of data types.
#'  - **nesting**: Recursive objects can be nested, while atomic objects cannot be nested.
#'  - **copy semantics**: One can copy a subset of a recursive object without copying the rest of the object
#'  (thus saving memory when copying).
#'  For atomic objects, however, a copy operation copies the entire vector (ignoring attributes). \cr
#'  - **vectorization**: most vectorized operations generally work on atomic objects,
#'  whereas recursive objects often require loops or apply-like functions.
#'  - **recursive subsets**: Recursive objects distinguish between "regular" subset operations
#'  (in base R using `[`, `[<-`),
#'  and recursive subset operations (in base R using `[[`, `[[<-`). \cr
#'  For atomic objects, these 2 have no meaningful difference
#'  (safe for perhaps attribute handling). \cr
#'  See also \link{squarebrackets_indx_fundamentals}.
#'  - **views**: For recursive objects,
#'  one can create a subset \link[=squarebrackets_PassByReference]{view} of a recursive subset. \cr
#'  Recursive subset views do not exist for atomic objects. \cr \cr
#' 
#' Most of these differences primarily come down to the following: \cr
#' An atomic object holds its own value,
#' while the elements of a recursive object references to other objects. \cr
#' \cr
#' All methods and binding implementations
#' that perform subset/bind operation on an object,
#' come in the atomic (`sb_`/`bind_`) and recursive (`sb2_`, `bind2_`) form. \cr
#' The \link{idx} method operates on the indices of an object,
#' but does not operate on the object itself,
#' and so has no distinction between the atomic and recursive form. \cr
#' \cr
#' The split between the atomic and recursive forms of the method dispatches
#' is done for several reasons:
#' 
#'  - Some S3 classes, like the `array` and `matrix` classes,
#'  are available in both atomic and recursive forms. \cr
#'  But the S3 method dispatch does not distinguish between atomic and recursive objects,
#'  despite the aforementioned differences between the 2. \cr
#'  So 'squarebrackets' uses a separate method dispatch for the atomic and recursive form.
#'  - The differences between atomic and recursive objects
#'  result in differences in the behaviour of sub-set operations. \cr
#'  Using a separate dispatch for atomic and recursive objects
#'  makes this disticntion more syntactically clear,
#'  and also forces some more careful thought from the user on handling objects.
#'  - Recursive matrices and array can be extremely powerful in tackling
#'  difficult problems that deal with nesting. \cr
#'  Yet there sometimes seem to be (in my experience) some confusion,
#'  especially among beginners, regarding recursive vs atomic objects,
#'  and (atomic and recursive) matrices vs data.frames. \cr
#'  By splitting the method dispatches so strictly,
#'  I hope to make sub-setting more beginner friendly, at least in the long run. \cr \cr
#' 
#' 
#' @section Manual Dispatch:
#' The 'squarebrackets' package intentionally exports each function in its S3 method dispatch system. \cr
#' This is handy for programming purposes. \cr
#' For example: atomic matrices and atomic arrays each have their own dispatch. \cr
#' Thus, when looping though matrices and arrays to extract some elements,
#' it may be easier to treat them all as arrays (remember that matrices inherit from arrays). \cr
#' Thus one can use `sb_x.array()`/ `sb2_x.array()` to ensure the "array" method is used,
#' instead of the "matrix" method. \cr
#' \cr
#' Another advantage is that one can explicitly alias a specific dispatch of a method,
#' if one so desires. \cr
#' For example like so: \cr
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' array_x <- function(x, ...) {
#'    if(is.atomic(x)) {
#'      sb_x.array(x, ...)
#'    }
#'    else if(is.recursive(x)) {
#'      sb2_x.array(x, ...)
#'    }
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

#' @rdname aaa05_squarebrackets_method_dispatch
#' @name aaa05_squarebrackets_method_dispatch
#' @aliases squarebrackets_method_dispatch
NULL
