#' squarebrackets: Methods as an Alternative to the Square Brackets Operators
#' 
#' @description
#' squarebrackets: Methods as an Alternative to the Square Brackets Operators \cr \cr
#' 
#' 
#' @section Goal & Properties:
#' 
#' Among programming languages,
#' 'R' has perhaps one of the most
#' flexible and comprehensive sub-setting functionality,
#' provided by the square brackets operators (`[`, `[<-`). \cr
#' But in some situations the square brackets operators
#' are occasionally less than optimally convenient \cr
#' (see \link{squarebrackets_inconveniences}). \cr
#' \cr
#' The Goal of the 'squarebrackets' package
#' is not to replace the square-brackets operators,
#' but to provide \bold{alternative} sub-setting methods and functions,
#' to be used in situations where the square bracket operators are inconvenient. \cr
#' \cr
#' These alternative sub-setting methods and functions have the following properties:
#' 
#'  * \bold{Programmatically friendly}:
#'    * Name-based arguments instead of position-based arguments.
#'    * Unlike base `[`,
#'    it's not required to know the number of dimensions of an array a-priori,
#'    to perform subset-operations on an array.
#'    * Missing arguments can be filled with `NULL`,
#'    instead of using dark magic like `base::quote(expr =    )`.
#'    * No Non-standard evaluation.
#'    * Functions are pipe-friendly.
#'    * No (silent) vector recycling.
#'    * Extracting and removing subsets uses the same syntax.
#'  * \bold{Class consistent}: 
#'    * sub-setting of multi-dimensional objects by specifying dimensions
#'    (i.e. rows, columns, ...)
#'    use `drop = FALSE`. So matrix in, matrix out.
#'    * The functions deliver the same results for
#'    data.frames, data.tables, tibbles, and tidytables.
#'    No longer does one have to re-learn the different brackets-based sub-setting rules
#'    for different types of data.frame-like objects.
#'    Powered by the subclass agnostic 'C'-code from 'collapse' and 'data.table'.
#'  * \bold{Explicit copy semantics}:
#'    * Sub-set operations that change its memory allocations,
#'    always return a modified copy of the object.
#'    * For sub-set operations that just change values in-place
#'    (similar to the `[<-` and `[[<-` methods)
#'    the user can choose a method that modifies the object by \bold{reference},
#'    or choose a method that returns a \bold{deep copy}.
#'  * \bold{Careful handling of names and other attributes}:
#'    * Sub-setting an object by index names returns ALL indices with that name,
#'    not just the first.
#'    * Data.frame-like objects (see supported classes below)
#'    are forced to have unique column names.
#'    * Attributes of data.frame-like objects (see supported classes below) are always preserved when sub-setting.
#'    * For other object types, the user can specify whether to preserve Attributes,
#'    or use R's `[` attribute behaviour (i.e. drop most attributes).
#'    This is to ensure compatibility with R-packages that create their own attribute behaviour for sub-setting.
#'  * \bold{Concise function and argument names}.
#'  * \bold{Performance aware}: \cr
#'  Despite the many checks performed, the functions are kept reasonably speedy,
#'  through the use of the 'Rcpp', 'collapse', and 'data.table' R-packages.
#'  Most of the heavy lifting in this package is done by the 'collapse' package. \cr \cr
#'
#'
#' @section Supported  Classes:
#' 'squarebrackets' only supports S3 classes,
#' and only those that primarily use square brackets for sub-setting
#' (hence the name of the package). \cr
#' \cr
#' Supported \link[=squarebrackets_immutable_classes]{immutable classes}: \cr
#' `atomic`, `factor`, `list`, `data.frame`
#' (including `tibble` and `sf-data.frame`). \cr
#' \cr
#' Supported  \link[=squarebrackets_mutable_classes]{mutable classes}: \cr
#' \link{mutable_atomic}, `data.table`
#' (including `tidytable` and `sf-data.table`). \cr
#' \cr
#' There are, of course, a lot of classes which are not supported by 'squarebrackets'. \cr
#' Most notably, certain types of list(-like) classes are not supported
#' (note that regular immutable lists are fully supported):
#' 
#'  * Environments are not supported.
#'  * Mutable list-like classes,
#'  such as the various 'collections' classes from the 'collections' package,
#'  are not supported
#'  (their sub-setting method is not based on square-brackets).
#'  * Locked list-like classes,
#'  such as the deferred list from the 'deflist' package, are not supported. \cr \cr
#' 
#'  
#' @section Methods and Functions:
#' 
#' `r .mybadge_intro_section("GENERIC METHODS", "darkgreen")` \cr
#' The main focus is on the generic methods. \cr
#' There are 2 types of generic methods:
#' 
#'  - generic methods for non-recursive objects (atomic, factor, etc.);
#'  these all start with `sb_`.
#'  - generic methods for recursive objects (list, data.frame, etc.);
#'  these all start with `sb2_`. \cr
#' 
#' The available generic methods are the following:
#' 
#'  * \link{sb_x}, \link{sb2_x}: extract, exchange, or duplicate subsets.
#'  * \link{sb_rm},  \link{sb2_rm}: un-select/remove subsets.
#'  * \link{sb_set}, \link{sb2_set}: modify (transform or replace)
#'  subsets of a \link[=squarebrackets_mutable_classes]{mutable object}
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{sb_mod}, \link{sb2_mod}: return a \bold{copy}
#'  of an object with modified
#'  (transformed or replaced) subsets.
#'  * \link{sb2_coe}: Coercively transform subsets of recursive objects.
#'  * \link{sb_before}, \link{sb_after}, \link{sb2_before}, \link{sb2_after}: insert new values before or after an index
#'  along a dimension of an object.
#'  * \link{sb2_rec}: accesses recursive subsets of lists.
#'  * \link{sb_setRename}, \link{sb2_setRename}: change the names of
#'  a \link[=squarebrackets_mutable_classes]{mutable object}
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{sb_currentBindings}, \link{sb2_currentBindings}: list or lock all currently existing bindings
#'  that share the share the same address as the input variable.
#'  * \link{idx}: translate given indices/subscripts,
#'  for the purpose of copy-on-modify substitution. \cr \cr
#'  
#' So for example,
#' use `sb_rm()` to remove subsets from atomic arrays,
#' and use `sb2_rm()` to remove subsets from recursive arrays. \cr \cr
#' 
#' `r .mybadge_intro_section("SPECIALIZED FUNCTIONS", "darkred")` \cr
#' Additional specialized sub-setting functions are provided:
#'  
#'  * \link{setapply}: apply functions over mutable matrix margins
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{ma_setv}: Find & Replace values in \link{mutable_atomic} objects
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#'  This is considerably faster and more memory efficient than using \link{sb_set} for this.
#'  * The \link[=dt_setcoe]{dt_}-functions for `data.table`-specific `[`-operations.
#'  * \link{sb_str}: extract or replace a subset of characters of a single string
#'  (each single character is treated as a single element).
#'  * \link{sb_a}: extract multiple attributes from an object. \cr \cr
#' 
#' `r .mybadge_intro_section("HELPER FUNCTIONS", "lightblue")` \cr
#' And finally,
#' a couple of helper functions for creating ranges, sequences, and indices
#' (often needed in sub-setting)
#' are provided:
#' 
#'  * \link{n}: Nested version of \link[base]{c},
#'  and short-hand for \link[base]{list}.
#'  * \link{sub2coord}, \link{coord2ind}: Convert subscripts
#'  (array indices) to coordinates,
#'  coordinates to flat indices,
#'  and vice-versa.
#'  * \link{match_all}: Find all matches, of one vector in another,
#'  taking into account the order and any duplicate values of both vectors.
#'  * Computing indices: \cr
#'  \link{idx_by} to compute grouped indices. \cr
#'  \link[=idx_ord_v]{idx_ord_}-functions to compute ordered indices.
#'  * Computing sequences: \cr
#'  \link{seq_rec2} for the recursive sequence generator
#'  (for example to generate a Fibonacci sequence). \cr
#'  \link{seq_names} to create a range of indices from a specified starting and ending name. \cr
#' 
#' 
#' @author \strong{Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @references The badges shown in the documentation of this R-package were made using the services of: \url{https://shields.io/}
#' 
#' @name aaa0_squarebrackets
#' @rdname aaa0_squarebrackets
#' @aliases squarebrackets-package
#' @aliases squarebrackets
#' @aliases squarebrackets_help
#' @useDynLib squarebrackets, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @exportPattern "^sb_x"
#' @exportPattern "^sb_rm"
#' @exportPattern "^sb_set"
#' @exportPattern "^sb_mod"
#' @exportPattern "^sb_coe"
#' @exportPattern "^sb_before"
#' @exportPattern "^sb_after"
#' @exportPattern "^sb_setRename"
#' @exportPattern "^sb_currentBindings"
#' @exportPattern "^sb2_x"
#' @exportPattern "^sb2_rm"
#' @exportPattern "^sb2_set"
#' @exportPattern "^sb2_mod"
#' @exportPattern "^sb2_coe"
#' @exportPattern "^sb2_before"
#' @exportPattern "^sb2_after"
#' @exportPattern "^sb2_rec"
#' @exportPattern "^sb2_setRename"
#' @exportPattern "^sb2_currentBindings"
#' @method `[` mutable_atomic
#' @method `[<-` mutable_atomic
#' 
NULL
#> NULL
