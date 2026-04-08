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
#'  - `tt_`: operates on subsets of data.frames and atomic/recursive matrices
#'  by \link[=squarebrackets_indx_fundamentals]{tabular indices} (also known as tabulat tiles).
#' 
#' For the `<operation>` part, the following is available:
#' 
#'  - `_x`: extract, exchange, exclude, or duplicate (if applicable) subsets.
#'  - `_mod`: modify subsets and return copy.
#'  - `_set`: modify subsets using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  - `icom`: create indices to be used in `[<-` to modify objects using the default Copy-On-Modify semantics. \cr \cr
#' 
#' To illustrate, let's take the methods used for extracting subsets
#' (`_x`): 
#' 
#'  - If `y` is a vector or array (of any dimension), \cr
#'  `ii_x(y, i)` corresponds to `y[i]`.
#'  - If `y` is a 3d array, \cr
#'  `ss_x(y, n(i, k), c(1, 3))` corresponds to `y[i, , k, drop = FALSE]`.
#'  - If `y` is a matrix or data.frame-like object, \cr
#'  `tt_x(y, i, j)` corresponds to `y[i, j, drop = FALSE]`. \cr \cr
#'
#' 
#' @section Specialized Methods:
#' The main methods of 'squarebrackets' are applicable for all supported types and classes
#' (provided the correct method for the correct dimensionality is used). \cr
#' 'squarebrackets' also provides specialized methods specific to certain structures:
#' 
#'  - the \link[=lst_rec]{lst_} set of methods,
#' which deal with sub-set operations that are only relevant for (nested) lists,
#' but not for the other types of supported objects. \cr
#' - the \link[=long]{long_} set of methods,
#' which deal with index-less sub-set operations,
#' that are only relevant for long atomic vectors. \cr \cr
#' 
#' @section Other Methods:
#' Besides the previously mentioned methods,
#' 'squarebrackets' provides some additional methods that do not neatly fit into the above methods. \cr
#' These are the set of `sb_` methods,
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
#' @section 'squarebrackets' Methods that do not require explicit Dispatches: 
#' The `ii_`/`ss_`/`tt_` - `_x` methods
#' are essentially wrappers around
#' the `[` operator. \cr
#' Similarly,
#' the `ii_`/`ss_`/`tt_` - `_mod` methods
#' are essentially wrappers around
#' the `[<-` operator. \cr
#' And the \link{lst_rec} and \link{lst_recin} methods
#' are essentially wrappers around
#' the `[[` and `[[<-` operators. \cr
#' Therefore, any custom class that has method dispatches defined for
#' the `[`, `[<-`, `[[`, and `[[<-` methods,
#' have their dispatches automatically handled by the above named methods. \cr
#' \cr
#' 
#' @section 'squarebrackets' Methods that DO require explicit Dispatches:
#' Unlike the `ii_`/`ss_`/`tt_` - `_x` methods,
#' the \link{long_x} method is \bold{not} a wrapper around the `[` operator,
#' and thus does not detect custom method dispatches defined for `[`. \cr
#' \link{long_x} can support classes that have static attributes
#' (attributes that do not depend on the type, length, or data of the vector),
#' by adding the class names to 
#' the \link[=squarebrackets_options]{squarebrackets.sticky} option. \cr
#' Classes with non-static attributes will explicitly require a `long_x` method. \cr
#' \cr
#' 
#' @section Class support for the Pass-By-Reference Methods:
#' The `_set` methods only support the `data.table` and `mutatomic` classes. \cr
#' Other mutable classes are not supported by the 'squarebrackets' package. \cr
#' However,
#' other 'R' package authors are welcome to add additional method dispatches
#' for the `_set` methods. \cr
#' \cr
#' 
#' 
#'  
#' 
#' 

#' @rdname aaa01_squarebrackets_methods
#' @name aaa01_squarebrackets_methods
#' @aliases squarebrackets_methods
NULL
