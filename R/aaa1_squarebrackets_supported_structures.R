#' Supported Structures
#'
#' @description
#' 'squarebrackets' only supports the most common S3 objects,
#' and only those that primarily use square brackets for sub-set operations
#' (hence the name of the package). \cr
#' \cr
#' One can generally divide the structures supported by 'squarebrackets'
#' along 3 key properties:
#' 
#'  * atomic vs recursive: \cr
#'  Types `logical`, `integer`, `double`, `complex`, `character`, and `raw` are \code{\link[base:is.atomic]{atomic}}. \cr
#'  Lists and data.frames are \code{\link[base:is.atomic]{recursive}}.
#'  * dimensionality: \cr
#'  Whether an object is a vector, matrix, array, or data.frame.
#'  * mutability: \cr
#'  Base R's S3 classes (except Environments) are generally immutable: \cr
#'  Modifying the object will create a copy (called 'copy-on-modify'). \cr
#'  'squabrackets also supports `data.tables` and \link{mutable_atomic} objects,
#'  which are mutable: \cr
#'  If desired, one can modify them without copy using
#'  \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#' 
#' \bold{Supported Structures} \cr
#' 
#' 'squarebrackets' supports the following immutable structures:
#' 
#'  * basic `atomic` classes \cr
#'  (atomic vectors, matrices, and arrays).
#'  * derived atomic vectors \cr
#'  (factors, dates, date-time, etc.).
#'  * basic list classes \cr
#'  (recursive vectors, matrices, and arrays). \cr
#'  * \link[base]{data.frame} \cr
#'  (including the classes `tibble`, `sf-data.frame` and `sf-tibble`). \cr \cr
#' 
#' 'squarebrackets' supports the following mutable structures:
#' 
#'  * \link{mutable_atomic} \cr
#'  (`mutable_atomic` vectors, matrices, and arrays);
#'  * \link[data.table]{data.table} \cr
#'  (including the classes `tidytable`, `sf-data.table`, and `sf-tidytable`). \cr \cr
#'  
#'  
#' 
#' @details
#' 
#' \bold{Atomic vs Recursive} \cr
#' The `sb_` methods and `bind_` implementations provided by 'squarebrackets'
#' work on \bold{atomic} (see \link[base]{is.atomic}) objects. \cr
#' The `sb2_` methods and `bind2_` implementations provided by 'squarebrackets'
#' work on \bold{recursive} (see \link[base]{is.recursive}) objects. \cr
#' The distinction is made between atomic and recursive methods,
#' because R's S3 method dispatch only checks the class of an object. \cr
#' So the S3 method dispatch makes no distinction, for example,
#' between atomic matrices,
#' and recursive matrices,
#' despite them being completely different kinds of objects
#' (except that they're both 2 dimensional objects). \cr
#' See \link{squarebrackets_method_dispatch} for more details. \cr
#' \cr
#' 
#' 
#' \bold{Dimensionality} \cr
#' 'squarebrackets' supports dimensionless or vector objects (i.e. \link{ndims}\code{ == 0L}). \cr
#' squarebrackets' supports arrays (see \link[base]{is.array} and \link[base]{is.matrix});
#' note that a matrix is simply an array with 2 dimensions. \cr
#' squarebrackets' also supports data.frame-like objects (see \link{is.data.frame}). \cr
#' Specifically, squarebrackets' supports a wide variety of data.frame classes: \cr
#' `data.frame`, `data.table`, `tibble`, `tidytable`; \cr
#' 'squarebrackets' also supports their 'sf'-package compatible counter-parts: \cr
#' `sf-data.frame`, `sf-data.table`, `sf-tibble`, `sf-tidytable`. \cr
#' \cr
#' Dimensionless vectors and dimensional arrays are supported in both their atomic and recursive forms. \cr
#' Data.frame-like objects, in contrast, \bold{only} exist in the \bold{recursive} form
#' (and, as stated, are supported by 'squarebrackets'). \cr
#' Recursive vectors, recursive matrices, and recursive arrays,
#' are collectively referred to as "lists" in the 'squarebrackets' documentation. \cr
#' \cr
#' Note that the dimensionality of data.frame-like objects is not the same as the dimensionality of
#' (recursive) arrays/matrices. \cr
#' For any array/matrix `x`, it holds that `length(x) == prod(dim(x))`. \cr
#' But for any data.frame `x`, it is the case that `length(x) == ncol(x)`. \cr
#' \cr
#' 
#' 
#' \bold{Mutable vs Immutable} \cr
#' Most S3 objects in base 'R' are immutable: \cr
#' They have no explicit \link[=squarebrackets_PassByReference]{pass-by-reference} semantics. \cr
#' Environments do have \link[=squarebrackets_PassByReference]{pass-by-reference} semantics,
#' but they are not supported by 'squarebrackets'. \cr
#' \cr
#' 'squarebrackets' supports the mutable `data.table` class
#' (and thus also `tidytable`, which inherits from `data.table`). \cr
#' \cr
#' 'squarbrackets' also includes a new class of mutable objects:
#' \link{mutable_atomic} objects. \cr
#' `mutable_atomic` objects are the same as atomic objects, except they are mutable (hence the name). \cr
#' \cr
#' The supported immutable structures are: \cr
#' Atomic and recursive vectors/matrices/arrays, data.frames, and tibbles. \cr
#' \cr
#' All the functions in the 'squarebrackets' package with the word "set" in their name
#' perform \link[=squarebrackets_PassByReference]{pass-by-reference} modification,
#' and thus only work on mutable structures. \cr
#' All other functions work the same way for both mutable and immutable structures. \cr
#' \cr
#' 
#' 
#' \bold{Derived Atomic Vector} \cr
#' A special class of objects are the Derived Atomic Vector structures: \cr
#' structures that are derived from atomic objects, but behave differently. \cr
#' For example: \cr
#' Factors, datetime, POSIXct and so on are derived from atomic vectors. \cr
#' But they have attributes and special methods that make them behave differently. \cr
#' \cr
#' 'squarebrackets' treats derived atomic classes as regular atomic vectors. \cr
#' There are highly specialized packages to handle objects derived from atomic objects. \cr
#' For example the 'forcats' package for handling factors, and the 'anytime' package
#' to handle date-time objects. \cr
#' \cr
#' 
#' 
#' \bold{Not Supported S3 structures} \cr
#' Key-Values storage S3 structures,
#' such as environments,
#' are not supported by 'squarebrackets'. \cr
#' \cr
#' 
#' 
#' 

#' @rdname aaa1_squarebrackets_supported_structures
#' @name aaa1_squarebrackets_supported_structures
#' @aliases squarebrackets_supported_structures
NULL
