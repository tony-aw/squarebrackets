#' Methods Available in 'squarebrackets'
#'
#' @description
#' This help page gives an overview of the methods available in 'squarebrackets'. \cr \cr
#' 
#' @section Main Methods:
#' The main methods of 'squarebrackets' use the naming convention `<indexform>_<operation>`: \cr
#' `<indexform>` tells you what form of indices the method uses; \cr
#' `<operation>` tells you \bold{what operation} is performed. \cr
#' \cr
#' For the `<indexform>` part, the following is available:
#' 
#'  - `ii_`: operates on subsets of atomic/recursive vectors/arrays
#'  by \link[=squarebrackets_indx_fundamentals]{interior indices}.
#'  - `ss_`: operates on subsets of atomic/recursive arrays of any dimensionality
#'  by \link[=squarebrackets_indx_fundamentals]{subscripts}.
#'  - `sbt_`: operates on subsets of data.frames and atomic/recursive matrices
#'  by \link[=squarebrackets_indx_fundamentals]{tabular indices}.
#'  - `slice_`: uses \bold{index-less}, \bold{sequence-based}, and efficient operations on (\link{mutatomic}) long vectors.
#'  - `slicev_`: uses \link[=squarebrackets_slicev]{index-less & value-based} and efficient operations on (\link{mutatomic}) long vectors. \cr \cr
#' 
#' 
#' For the `<operation>` part, the following is available:
#' 
#'  - `_x`: extract, exchange, exclude, or duplicate (if applicable) subsets.
#'  - `_mod`: modify subsets and return copy.
#'  - `_set`: modify subsets using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#' 
#' 
#' To illustrate, let's take the methods used for extracting subsets
#' (`_x`): 
#' 
#'  - If `y` is a vector or array (of any dimension), \cr
#'  `ii_x(y, i)` corresponds to `y[i]`.
#'  - If `y` is a 3d array, \cr
#'  `ss_x(y, n(i, k), c(1, 3))` corresponds to `y[i, , k, drop = FALSE]`.
#'  - If `y` is a matrix or data.frame-like object, \cr
#'  `sbt_x(y, i, j)` corresponds to `y[i, j, drop = FALSE]`. \cr \cr
#'
#' 
#' @section Other Methods:
#' Besides the main methods, 'squarebrackets' provides some additional methods that do not neatly fit into the above methods. \cr
#' \cr
#' First, there is the \link[=lst_rec]{lst_} set of methods,
#' which deal with sub-set operations that are only relevant for (nested) lists,
#' but not for the other types of supported objects. \cr
#' \cr
#' Second, there is \link{idx} method,
#' which works on both recursive and non-recursive objects,
#' and transforms/translates indices to be used in R's default copy-on-modify semantics. \cr
#' \cr
#' Finally, there are the `sb_` sets of methods,
#' which cover miscellaneous operations for atomic/recursive objects. \cr \cr
#' 
#' 
#' @section Finding the Appropriate Help Pages:
#' 
#' With knowledge of the naming convention of the main methods,
#' one can easily find out information about a particular method by usign the `?` operator. \cr
#' So to find out about modifying objects by subscripts using Pass-by-Reference semantics,
#' type in: \cr
#' `?ss_set` \cr
#' \cr
#'  
#' 
#' 
#'  
#' 
#' 

#' @rdname aaa01_squarebrackets_methods
#' @name aaa01_squarebrackets_methods
#' @aliases squarebrackets_methods
NULL
