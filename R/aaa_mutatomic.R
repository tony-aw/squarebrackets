#' A Class of Mutable Atomic Array-Like Objects
#'
#' @description
#' The `mutatomic` class is a class of mutable atomic array-like objects. \cr
#' It works exactly the same in all aspects as regular atomic vectors and arrays. \cr
#' There is only one real difference: \cr
#' Pass-by-reference functions in 'squarebrackets'
#' only accept atomic objects when they are of class `mutatomic`,
#' for greater safety. \cr
#' In all other aspects, `mutatomic` objects are the same as R's regular atomic objects,
#' including the behaviour of the `[<-` operator . \cr
#' \cr
#' Exposed functions (beside the S3 methods):
#' 
#'  * `mutatomic()`: create a `mutatomic` object from given data.
#'  * `couldb.mutatomic()`: checks if an object could become `mutatomic`. \cr
#'
#'
#' @param x an atomic object.
#' @param data atomic vector giving data to fill the `mutatomic` object.
#' @param names,dim,dimnames,comment see their respective help pages. 
#' @param ... method dependent arguments.
#' 
#' 
#' @details
#' 
#' \bold{`couldb.mutatomic()`}: \cr
#' The `couldb.mutatomic()` function checks if an object can be converted to a `mutatomic` object. \cr
#' Only objects with the following properties can be converted to `mutatomic`: \cr
#' 
#'  - \bold{`atomic` data type}: \cr
#'  The data type of the object is `atomic`. \cr
#'  I.e. \link{raw}, \link{logical}, \link{integer}, \link{double}, \link{complex}, or \link{character}. \cr
#'  Thus recursive objects, like lists, and S4 objects, and so on cannot become `mutatomic`. \cr
#'  `ALTREP` objects will be materialized when coerced to `mutatomic`.
#'  
#'  - \bold{length equals number of elements}: \cr
#'  The length of the object equals the number of elements. \cr
#'  This is the case for the vast majority of objects. \cr
#'  But some objects, like the various classes of the 'bit' package,
#'  break this principle and are thus not compatible with `mutatomic`.
#'  
#'  - \bold{product of dimensions equals number of elements}: \cr
#'  If the object has dimensions,
#'  the product of the dimension sizes must equal the number of elements.
#'  
#'  - \bold{array-like}: \cr
#'  The object is "array-like",
#'  meaning that the object is a vector where one can \emph{meaningfully} set, remove, or modify the dimensions,
#'  without breaking its functionality. \cr
#'  Thus a factor
#'  (which already represents a dummy matrix),
#'  and most data-time objects (which generally do not support dimensions)
#'  cannot be `mutatomic`.
#'  
#'  - \bold{missing values consistent with base 'R'}: \cr
#'  The meaning of `NA` in the vector is consistent with that of base 'R'. \cr
#'  This is the case for the vast majority of objects. \cr
#'  But some classes, like `integer64` has a different definition of `NA` than base 'R',
#'  which is not supported by the `mutatomic` class.
#'  
#'  - \bold{not an S4 class}: \cr
#'  The object is not an S4 class. \cr \cr
#' 
#' 
#' @section Footnotes: 
#' - Always use
#' the exported functions given by 'squarebrackets'
#' to create a `mutatomic` object,
#' as they make necessary safety checks.
#' - Mutable objects that can only exist as vectors can be modified via functions from the 'data.table' or 'collapse' packages.
#' - Mutable date-time objects already exist in the form of S4 classes, and need no coverage from the 'mutatomic' class.
#' - While one can force dimension attributes upon factors and base R's date-time objects,
#' this may break or corrupt it's interactions with methods
#' (like the `[` and `print()` methods),
#' coercions (like `as.data.frame()`), and array manipulators (like `aperm()`). \cr
#' 'squarebrackets' does not allow such sloppiness,
#' and thus strictly consider factors and date-time objects to be vectors but not 'array-like'. \cr
#' - It would not make much sense to make factors dimensional,
#' since they are primarily used to represent a dummy matrix for statistical modelling. \cr
#' A dimensional array of categorical values that's not used for modelling
#' is better off being represented as either character array or integer array. \cr \cr
#' 
#' 
#' @returns
#' For `mutatomic()`, `as.mutatomic()`: \cr
#' Returns a `mutatomic` object. \cr
#' \cr
#' For `is.mutatomic()`: \cr
#' Returns `TRUE` if the object is `mutatomic`,
#' and returns `FALSE` otherwise. \cr
#' \cr
#' For `couldb.mutatomic()`: \cr
#' Returns `TRUE` if the object can be converted to `mutatomic`. \cr
#' Returns `FALSE` otherwise. \cr \cr
#'
#'
#' @example inst/examples/class_mutatomic.R
#' 


#' @name mutatomic_class
NULL
