#' squarebrackets: Subset Methods as Alternatives to the Square Brackets Operators for Programming
#' 
#' @description
#' squarebrackets: \cr
#' Subset Methods as Alternatives to the Square Brackets Operators for Programming. \cr
#' \cr
#' 'squarebrackets' provides subset methods
#' (supporting both atomic and recursive S3 classes)
#' that may be more convenient alternatives to the `[` and `[<-` operators,
#' whilst maintaining similar performance. \cr
#' Some nice properties of these methods include, but are not limited to, the following.
#'  
#'  1) The `[` and `[<-` operators use different rule-sets for different data.frame-like types
#'  (data.frames, data.tables, tibbles, tidytables, etc.). \cr
#'  The 'squarebrackets' methods use the same rule-sets for the different data.frame-like types.
#'  2) Performing dimensional subset operations on an array using `[` and `[<-`,
#'  requires a-priori knowledge on the number of dimensions the array has. \cr
#'  The 'squarebrackets' methods work on any arbitrary dimensions without requiring such prior knowledge.
#'  3) When selecting names with the `[` and `[<-` operators,
#'  only the first occurrence of the names are selected in case of duplicate names. \cr
#'  The 'squarebrackets' methods always perform on all names in case of duplicates,
#'  not just the first.
#'  4) The `[<-` operator only supports copy-on-modify semantics for most classes. \cr
#'  The 'squarebrackets' methods provides explicit pass-by-reference and pass-by-value semantics,
#'  whilst still respecting things like binding-locks and mutability rules.
#'  5) 'squarebrackets' supports index-less sub-set operations,
#'  which is more memory efficient
#'  (and better for the environment)
#'  for `long vectors` than sub-set operations using the `[` and `[<-` operators. \cr \cr
#' 
#' 
#' @section Goal:
#' 
#' Among programming languages,
#' 'R' has perhaps one of the most
#' flexible and comprehensive sub-setting functionality,
#' provided by the square brackets operators (`[`, `[<-`). \cr
#' But in some situations the square brackets operators
#' are occasionally less than optimally convenient \cr
#' \cr
#' The Goal of the 'squarebrackets' package
#' is not to replace the square-brackets operators,
#' but to provide \bold{alternative} sub-setting methods and functions,
#' to be used in situations where the square bracket operators are inconvenient. \cr
#' \cr
#' 
#' 
#' @section Overview Help Pages:
#' 
#' The essential documentation is split into the following help pages:
#' 
#'  - \link{squarebrackets_supported_structures}: \cr
#'  Lists the structures that are supported by 'squarebrackets',
#'  and explains some related terminology.
#'  - \link{squarebrackets_methods}: \cr
#'  Lists the main methods provided by 'squarebrackets'.
#'  - \link{squarebrackets_indx_fundamentals}: \cr
#'  Explains the essential fundamentals of the indexing forms in 'squarebrackets'.
#'  - \link{squarebrackets_indx_args}: \cr
#'  Explains the common indexing arguments used in the main S3 methods.
#'  - \link{squarebrackets_modify}: \cr
#'  Explains the essentials of modification in 'squarebrackets'
#'  - \link{squarebrackets_options}: \cr
#'  Lists and explains the options the user can specify in 'squarebrackets'. \cr \cr
#'  
#'  
#' The following help pages explain the pass-by-reference semantics provided by 'squarebrackets',
#' and only need to be read when planning to use those semantics:
#' 
#'  - \link{squarebrackets_PassByReference}: \cr
#'  Explains Pass-by-Reference semantics, and its important consequences.
#'  - \link{squarebrackets_coercion}: \cr
#'  Explains the difference in coercion rules between
#'  modification through Pass-by-Reference semantics and
#'  modification through copy (i.e. pass-by-value). \cr \cr
#'  
#'
#' And finally,
#' there is the \link{squarebrackets_method_dispatch} help page,
#' which gives some small additional details regarding
#' the S3 method dispatch used in 'squarebrackets'. \cr \cr
#' 
#' 
#' 
#' @section Helper Functions: 
#' 
#' A couple of convenience functions, and helper functions for creating ranges, sequences, and indices
#' (often needed in sub-setting)
#' are provided:
#' 
#'  * \link{n}: Nested version of \link[base]{c},
#'  and short-hand for \link[base]{list}.
#'  * \link{ndim}: Get the number of dimensions of an object.
#'  * \link{sub2coord}, \link{coord2ind}: Convert subscripts
#'  (array indices) to coordinates,
#'  coordinates to flat indices,
#'  and vice-versa.
#'  * \link{match_all}: Find all matches, of one vector in another,
#'  taking into account the order and any duplicate values of both vectors.
#'  * Computing indices: \cr
#'  \link{idx_r} to compute an integer index range. \cr
#'  \link{idx_by} to compute grouped indices. \cr
#'  \link[=idx_ord_v]{idx_ord_}-functions to compute ordered indices. \cr \cr
#' 
#' 
#' @section Properties Details:
#' The alternative sub-setting methods and functions provided by 'squarebrackets'
#' have the following properties:
#' 
#'  * \bold{Programmatically friendly}:
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
#'    use `drop = FALSE`. \cr
#'    So matrix in, matrix out.
#'    * The methods deliver the same results for
#'    data.frames, data.tables, tibbles, and tidytables. \cr
#'    No longer does one have to re-learn the different brackets-based sub-setting rules
#'    for different types of data.frame-like objects. \cr
#'    Powered by the subclass agnostic 'C'-code from 'collapse' and 'data.table'.
#'  * \bold{Explicit copy semantics}:
#'    * Sub-set operations that change its memory allocations,
#'    always return a modified (partial) copy of the object. \cr
#'    * For sub-set operations that just change values in-place
#'    (similar to the `[<-` and `[[<-` methods)
#'    the user can choose a method that modifies the object by \bold{reference},
#'    or choose a method that returns a \bold{(partial) copy}.
#'  * \bold{Careful handling of names}:
#'    * Sub-setting an object by index names returns ALL matches with the given names,
#'    not just the first.
#'    * Data.frame-like objects (see supported classes below)
#'    are forced to have unique column names.
#'    * Sub-setting arrays using `x[indx1, indx2, etc.]` will drop `names(x)`. \cr
#'    The methods from 'squarebrackets' will not drop `names(x)`.
#'  * \bold{Concise function and argument names}.
#'  * \bold{Performance & Energy aware}: \cr
#'  Despite the many checks performed, the functions are kept reasonably speedy,
#'  through the use of the 'Rcpp', 'collapse', and 'data.table' R-packages. \cr
#'  The functions were also made to be as memory efficient as reasonably possible,
#'  to lower the carbon footprint of this package. \cr \cr
#'
#' 
#' 
#' @author \strong{Author, Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @references The badges shown in the documentation of this R-package were made using the services of: \url{https://shields.io/}
#' 
#' @name aaa00_squarebrackets_help
#' @rdname aaa00_squarebrackets_help
#' @aliases squarebrackets-package
#' @aliases squarebrackets
#' @aliases squarebrackets_help
#' @useDynLib squarebrackets, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' 
#' @exportPattern "^sb_setFlatnames"
#' @exportPattern "^sb_setDimnames"
#' @exportPattern "^sb2_setVarnames"
#' @exportPattern "^idx"
#' 
#' @exportPattern "^i_x"
#' @exportPattern "^i_wo"
#' @exportPattern "^i_set"
#' @exportPattern "^i_mod"
#' @exportPattern "^i2_x"
#' @exportPattern "^i2_wo"
#' @exportPattern "^i2_mod"
#' @exportPattern "^i2_rec"
#' 
#' @exportPattern "^ss_x"
#' @exportPattern "^ss_wo"
#' @exportPattern "^ss_set"
#' @exportPattern "^ss_mod"
#' @exportPattern "^ss2_x"
#' @exportPattern "^ss2_wo"
#' @exportPattern "^ss2_set"
#' @exportPattern "^ss2_mod"
#' 
#' 
NULL
#> NULL
