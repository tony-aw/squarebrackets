#' Methods
#'
#' @description
#' The main methods of 'squarebrackets' use the naming convention `A_B`: \cr
#' `A` tells you on what kind of object and what kind of indices the method operates on; \cr
#' `B` tells you \bold{what operation} is performed. \cr
#' \cr
#' For the `A` part, the following is available:
#' 
#'  - `i_`: operates on subsets of \bold{atomic} objects by (flat/linear) indices.
#'  - `i2_`: operates on subsets of \bold{recursive} objects by (flat/linear) indices.
#'  - `ss_`: operates on subsets of \bold{atomic} objects by (dimensional) subscripts.
#'  - `ss2_`: operates on subsets of \bold{recursive} objects by (dimensional) subscripts.
#'  - `slice_`: uses \bold{index-less}, \bold{sequence-based}, and efficient operations on `mutatomic` objects.
#'  - `slicev_`: uses \bold{index-less}, \bold{value-based} and efficient operations on `mutatomic` objects. \cr \cr
#' 
#' 
#' For the `B` part, the following is available:
#' 
#'  - `_x`: extract, exchange, or duplicate (if applicable) subsets.
#'  - `_wo`: returns the original object \bold{without} the provided subsets.
#'  - `_mod`: modify subsets and return copy.
#'  - `_set`: modify subsets using \link[=mutatomic_PassByReference]{pass-by-reference semantics}.
#'  - `_rec`: access recursive subsets of lists - ONLY available with the `i2_` prefix.
#'  - `_recin`: replace, transform, delete, or extend recursive subsets of lists - ONLY available with the `i2_` prefix. \cr \cr
#' 
#' There is also the somewhat separate \link{idx} method,
#' which works on both recursive and non-recursive objects. \cr
#' 
#' Thus, for example,
#' the \link{i_x} method extracts/exchanges/duplicates subsets of atomic objects by flat/linear indices. \cr \cr
#' 
#' 
#' @section Illustration: 
#' 
#' To illustrate, let's take the methods used for extracting subsets
#' (\eqn{\ast}`_x`): \cr
#' When `y` is atomic, the following holds (roughly speaking):
#' 
#'  - `i_x(y, i)` corresponds to `y[i]`
#'  - `ss_x(y, n(i, k), c(1, 3))` corresponds to `y[i, , k]` \cr \cr
#'  
#' When `y` is a list (i.e. recursive), the following holds (roughly speaking):
#' 
#'  - `i2_x(y, i)` corresponds to `y[i]` or `y[[i]]` (depending on the arguments given in `i2_x()`)
#'  - `ss2_x(y, n(i, k), c(1, 3))` corresponds to `y[i, , k]` or `y[[i, , k]]` (depending on the arguments given in `ss2_x()`) \cr \cr
#' 
#' 
#' @section Finding the Appropriate Help Pages:
#' 
#' Now that it is clear what the naming convention is of the main methods,
#' one can easily find out information about a particular method by usign the `?` operator. \cr
#' So to find out about modifying recursive objects by reference using dimensional indices,
#' type in: \cr
#' `?ss2_set` \cr
#' \cr
#' Note that some combinations of `A` and `B` are not available. \cr
#' For example, as the `_rec` `_recin` suffices are only applicable for recursive objects using linear indices,
#' only `i2_rec` and `i2_recin` exist; \cr
#' combinations like `i_rec`, `ss_recin`, etc. don't exist. \cr
#' Another example: there is no `_mod` suffix available for the `slice_` and `slicev_` methods. \cr \cr
#' 
#' 
#' 
#'  
#' 
#' 

#' @rdname aaa02_squarebrackets_methods
#' @name aaa02_squarebrackets_methods
#' @aliases squarebrackets_methods
NULL
