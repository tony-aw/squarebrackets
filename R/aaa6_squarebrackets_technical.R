#' Additional Technical Details
#'
#' @description
#' 
#' \bold{Atomic} \cr 
#' `r .mybadge_require_unique_names("NO")` \cr
#' The atomic vector is the most basic class type. \cr
#' Matrices and Arrays are just atomic vectors with dimension attributes. \cr
#' All elements in an atomic vector class
#' must have the same atomic type
#' ("logical", "integer", "numeric", "complex", "character" and "raw"). \cr
#' Therefore, the \link{sb_coe} method will coerce the entirety of an atomic vector,
#' NOT just a subset of it. \cr
#' \cr
#' \bold{Factor} \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' Factors have the property that they can only store values that are defined in
#' their "levels" attribute; other values are not allowed, and thus result in `NA`s. \cr
#' Thus And \link{sb_mod} does NOT coerce replacement values for factors!
#' This is quite different from the atomic vector classes,
#' which can store an infinite variety of values
#' (provided they are of the same atomic type). \cr
#' Due to these properties, there is NO `sb_set()` method for factors. \cr
#' \cr
#' \bold{List} \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' Lists are recursive objects (i.e. they can be nested),
#' and they do not actually store values but rather store reference to other objects. \cr
#' Therefore \link{sb_rec} method can be used to access recursive subsets of a list,
#' no matter how deep/low in hierarchy it is in the list. \cr
#' \cr
#' \bold{Data.frame-like} \cr
#' `r .mybadge_require_unique_names("YES")` \cr
#' The data.frame-like objects quite different from the previously named classes. \cr
#' And the different data.frame-like classes also differ from each other quite a bit -
#' especially in terms of sub-setting. \cr
#' The 'squarebrackets' R-package attempts to keep the data.frame methods as class agnostic as possible,
#' through the class agnostic functionality of the 'collapse' and 'data.table' R-packages. \cr
#' These 3 things cause some
#' \bold{important}
#' oddities in how data.frame-like classes are treated differently from the other classes: 
#' 
#'  * Whole-columns will be auto-coerced when replaced/transformed by `sb_mod()`,
#'  but partial columns will NOT be auto-coerced.
#'  * The \link{sb_x} and \link{sb_rm} methods always automatically conserve all attributes
#'  (though names are adjusted accordingly, of course),
#'  they are never stripped, unlike the other classes.
#'  * Giving a data.frame-like object with non-unique column names to the `sb_`-methods
#'  returns an error.
#'  Also, duplicating columns with \link{sb_x}
#'  will automatically adjust the column names to make them unique. \cr \cr
#' 
#' 

#' @rdname aaa6_squarebrackets_technical
#' @name aaa6_squarebrackets_technical
#' @aliases squarebrackets_technical
NULL
