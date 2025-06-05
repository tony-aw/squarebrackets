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
#'  Whether an object is a vector, array, or data.frame. \cr
#'  Note that a matrix is simply an array with 2 dimensions.
#'  * mutability: \cr
#'  Base R's S3 classes (except Environments) are generally immutable: \cr
#'  Modifying the object will create a copy (called 'copy-on-modify'). \cr
#'  'squarebrackets also supports `data.tables` and \link[mutatomic]{mutatomic} objects,
#'  which are mutable: \cr
#'  If desired, one can modify them without copy using
#'  pass-by-reference semantics. \cr \cr
#' 
#' \bold{Supported Structures} \cr
#' 
#' 'squarebrackets' supports the following immutable structures:
#' 
#'  * basic `atomic` classes \cr
#'  (atomic vectors and arrays).
#'  * \link{factor}. \cr
#'  * basic list classes \cr
#'  (recursive vectors and arrays). \cr
#'  * \link[base]{data.frame} \cr
#'  (including the classes `tibble`, `sf-data.frame` and `sf-tibble`). \cr \cr
#' 
#' 'squarebrackets' supports the following mutable structures:
#' 
#'  * \link[mutatomic]{mutatomic} \cr
#'  (`mutatomic` vectors arrays);
#'  * \link[data.table]{data.table} \cr
#'  (including the classes `tidytable`, `sf-data.table`, and `sf-tidytable`). \cr \cr
#'  
#' The methods provided by 'squarebrackets', like any method, can be extended
#' (by other 'R' package authors)
#' to support additional classes that are not already supported natively by 'squarebrackets'. \cr \cr
#' 
#' 
#' @details
#' 
#' \bold{Atomic vs Recursive} \cr
#' The `i_`/`ss_` methods provided by 'squarebrackets'
#' work on \bold{atomic} (see \link[base]{is.atomic}) objects. \cr
#' The `i2_`/`ss2_` methods provided by 'squarebrackets'
#' work on \bold{recursive} (see \link[base]{is.recursive}) objects. \cr
#' See \link{squarebrackets_methods} for more details. \cr
#' \cr
#' 
#' 
#' \bold{Dimensionality} \cr
#' 'squarebrackets' supports dimensionless or vector objects (i.e. \link{ndim}\code{ == 0L}). \cr
#' squarebrackets' supports arrays (see \link[base]{is.array} and \link[base]{is.matrix});
#' note that a matrix is simply an array with 2 dimensions. \cr
#' 'squarebrackets' also supports data.frame-like objects (see \link{is.data.frame}). \cr
#' Specifically, squarebrackets' supports a wide variety of data.frame classes: \cr
#' `data.frame`, `data.table`, `tibble`, `tidytable`; \cr
#' 'squarebrackets' also supports their 'sf'-package compatible counter-parts: \cr
#' `sf-data.frame`, `sf-data.table`, `sf-tibble`, `sf-tidytable`. \cr
#' \cr
#' Dimensionless vectors and dimensional arrays are supported in both their atomic and recursive forms. \cr
#' Data.frame-like objects, in contrast, only exist in the recursive form
#' (and, as stated, are supported by 'squarebrackets'). \cr
#' Recursive vectors, recursive matrices, and recursive arrays,
#' are collectively referred to as "lists" in the 'squarebrackets' documentation. \cr
#' \cr
#' Note that the dimensionality of data.frame-like objects is not the same as the dimensionality of
#' (recursive) arrays/matrices. \cr
#' For example: \cr
#' For any array/matrix `x`, it holds that `length(x) == prod(dim(x))`. \cr
#' But for any data.frame `x`, it is the case that `length(x) == ncol(x)`. \cr
#' \cr
#' 
#' 
#' \bold{Mutable vs Immutable} \cr
#' Most of base R's S3 classes (except Environments) are generally immutable: \cr
#' Modifying the object will create a copy (called 'copy-on-modify'). \cr
#' They have no explicit \link[=mutatomic_PassByReference]{pass-by-reference} semantics. \cr
#' Most S3 objects in base 'R' are immutable: \cr
#' Environments do have \link[=mutatomic_PassByReference]{pass-by-reference} semantics,
#' but they are not supported by 'squarebrackets'. \cr
#' \cr
#' Supported mutable structures:
#' 
#'  - 'squarebrackets' supports the mutable `data.table` class \cr
#'  (and thus also `tidytable`, which inherits from `data.table`).
#'  - 'squarebrackets' supports the \link[mutatomic]{mutatomic} class. \cr
#' `mutatomic` objects are the same as atomic objects,
#' except they are mutable (hence the name). \cr \cr
#' 
#' Supported immutable structures: \cr
#' Atomic and recursive vectors/matrices/arrays, data.frames, and tibbles. \cr
#' \cr
#' All the functions in the 'squarebrackets' package with the word "set" in their name
#' perform pass-by-reference modification,
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
#' For example, the 'anytime' package to handle date-time objects. \cr
#' \cr
#' 'squarebrackets  does provide some more explicit support for factors. \cr
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

#' @rdname aaa01_squarebrackets_supported_structures
#' @name aaa01_squarebrackets_supported_structures
#' @aliases squarebrackets_supported_structures
NULL
