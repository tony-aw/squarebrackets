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
#'  4) The `[[` and `[[<-` operators
#'  allow operating on a recursive subset of a nested list. \cr
#'  But these only operate on a single recursive subset,
#'  and are not vectorized for multiple recursive subsets of a nested list at once. \cr
#'  'squarebrackets' provides a way to reshape a nested list
#'  into a recursive matrix,
#'  thereby allowing vectorized operations on recursive subsets of such a nested list.
#'  5) The `[<-` operator only supports copy-on-modify semantics for most classes. \cr
#'  The 'squarebrackets' methods provides explicit pass-by-reference and pass-by-value semantics,
#'  whilst still respecting things like binding-locks and mutability rules.
#'  6) 'squarebrackets' supports index-less sub-set operations,
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
#' \cr
#' 
#'
#' @section Supported Structures:
#' 'squarebrackets' only supports the most common S3 classes,
#' and only those that primarily use square brackets for sub-setting
#' (hence the name of the package). \cr
#' \cr
#' 'squarebrackets' supports the following structures:
#' 
#'  * basic `atomic` classes \cr
#'  (atomic vectors, matrices, and arrays).
#'  * \link{mutable_atomic} classes \cr
#'  (\link{mutable_atomic} vectors, matrices, and arrays).
#'  * \link{factor}. \cr
#'  * basic list classes \cr
#'  (recursive vectors, matrices, and arrays). \cr
#'  * \link[base]{data.frame} \cr
#'  (including the classes `tibble`, `sf-data.frame` and `sf-tibble`).
#'  * \link[data.table]{data.table} \cr
#'  (including the classes `tidytable`, `sf-data.table`, and `sf-tidytable`). \cr \cr
#' 
#' See \link{squarebrackets_supported_structures} for more details. \cr \cr
#
#'  
#' @section Sub-set Operation Methods & Binding Implementations:
#' 
#' The main focus of this package is on its generic methods
#' and dimensional binding implementations. \cr
#' \cr
#' Generic methods for atomic objects
#' start with `sb_`. \cr
#' Generic methods for recursive objects (list, data.frame, etc.)
#' start with `sb2_`. \cr
#' There is also the somewhat separate \link{idx} method,
#' which works on both recursive and non-recursive objects. \cr
#' The binding implementations for dimensional objects
#' start with `bind_`. \cr
#' And finally there are the `slice_` methods,
#' which (currently) only work on (mutable) atomic vectors. \cr
#' \cr
#' 
#' 
#' `r .mybadge_intro_section("ACCESS SUBSETS", "darkgreen")` \cr
#' 
#' Methods to access subsets (i.e. extract selection, or extract all except selection):
#' 
#'  * \link{sb_x}, \link{sb2_x}: extract, exchange, or duplicate subsets.
#'  * \link{sb_wo},  \link{sb2_wo}: return an object without the specified subset.
#'  * \link{sb2_rec}: access recursive subsets of lists.
#'  * \link{slice_x}: index-less and efficient,
#'  sequence-based extraction of a subset from a long vector.
#'  * \link{slice_wo}: index-less and efficient,
#'  sequence-based returning a long vector without the specified subset.
#'  * \link{slicev_x}: index-less and efficient,
#'  value-based extraction of a subset from a long vector. \cr \cr
#'  
#'  
#' `r .mybadge_intro_section("MODIFY SUBSETS", "red")` \cr
#'  
#' Methods to modify subsets:
#'  
#'  * \link{idx}: translate given indices/subscripts,
#'  for the purpose of copy-on-modify substitution.
#'  * \link{sb2_recin}: replace, transform, remove, or add recursive subsets to a list,
#'  through R's default Copy-On-Modify semantics.
#'  * \link{sb_mod}, \link{sb2_mod}: return the object with modified
#'  (transformed or replaced) subsets.
#'  * Methods to \link[=sb_setRename]{rename a mutable object} using
#'  \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{sb_set}, \link{sb2_set}: modify (transform or replace)
#'  subsets of a \link[=squarebrackets_supported_structures]{mutable object}
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{slice_set}: index-less and efficient,
#'  sequence-based modification of a (long) vector subset using
#'  \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{slicev_set}: index-less and efficient,
#'  value-based modification of a (long) vector subset using
#'  \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#'  
#' 
#' `r .mybadge_intro_section("EXTEND BEYOND", "purple")` \cr
#'  
#' Methods and binding implementations,
#' to extend or re-arrange an object beyond its current size:
#' 
#'  * \link[=bind]{bind_}:  implementations for binding dimensional objects.
#'  * \link{sb_x}, \link{sb2_x}: extract, exchange, or duplicate subsets.
#'  * \link{sb2_recin}: replace, transform, remove, or add recursive subsets to a list,
#'  through R's default Copy-On-Modify semantics.
#'  
#' 
#' See \link{squarebrackets_method_dispatch} for more information on how 'squarebrackets'
#' uses its S3 Method dispatch. \cr \cr
#' 
#' 
#' @section Functions: 
#' 
#' `r .mybadge_intro_section("SPECIALIZED FUNCTIONS", "darkred")` \cr
#' Additional specialized sub-setting functions are provided:
#' 
#'  * \link{lst_untree}: unnest tree-like nested list into a recursive matrix,
#'  to speed-up vectorized sub-setting on recursive subsets of the list.
#'  * The \link[=dt_setcoe]{dt_}-functions
#'  to programmatically perform `data.table`-specific `[`-operations,
#'  with the security measures provided by the 'squarebrackets' package.
#'  * \link{setapply}: apply functions over mutable matrix margins
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr \cr
#'  
#' 
#' `r .mybadge_intro_section("HELPER FUNCTIONS", "lightblue")` \cr
#' A couple of convenience functions, and helper functions for creating ranges, sequences, and indices
#' (often needed in sub-setting)
#' are provided:
#' 
#'  * \link{currentBindings}: list or lock all currently existing bindings
#'  that share the share the same address as the input variable.
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
#' @section Overview Help Pages:
#' 
#' Besides the website,
#' 'squarebrackets' comes with several help pages
#' that can be accessed from within 'R'. \cr
#' 
#' MAIN DOCUMENTATION:
#' 
#'  - \link{squarebrackets_supported_structures}: \cr
#'  Lists the structures that are supported by 'squarebrackets',
#'  and explains some related terminology.
#'  - \link{squarebrackets_indx_fundamentals}: \cr
#'  Explains the essential fundamentals of the indexing forms in 'squarebrackets'.
#'  - \link{squarebrackets_indx_args}: \cr
#'  Explains the common indexing arguments used in the main S3 methods.
#'  - \link{squarebrackets_modify}: \cr
#'  Explains the essentials of modification in 'squarebrackets'
#'  - \link{squarebrackets_options}: \cr
#'  Lists and explains the options the user can specify in 'squarebrackets'.
#'  - \link{squarebrackets_method_dispatch}: \cr
#'  Gives details regarding the S3 method dispatch in 'squarebrackets'. \cr \cr
#'  
#' 
#' ADDITIONAL DOCUMENTATION:
#' 
#' - \link{squarebrackets_PassByReference}: \cr
#' Explains Pass-by-Reference semantics, and its important consequences. \cr
#' If you are not planning on using the pass-by-reference functionality
#' in 'squarebrackets',
#' you do not need to read this help page.
#' - \link{squarebrackets_coercion}: \cr
#' Explains the difference in coercion rules between
#' modification through Pass-by-Reference semantics and
#' modification through copy (i.e. pass-by-value)
#' for the supported mutable structures. \cr
#' If you are not planning on using the pass-by-reference functionality
#' in 'squarebrackets',
#' you do not need to read this help page.
#' - \link{squarebrackets_slicev}: \cr
#' Explains the arguments for the \link{slicev} set of methods. \cr
#' If you are not planning to use the \link{slicev} methods,
#' you do not need to read this help page. \cr \cr
#' 
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
#' @exportPattern "^sb_x"
#' @exportPattern "^sb_wo"
#' @exportPattern "^sb_set"
#' @exportPattern "^sb_mod"
#' @exportPattern "^sb_coe"
#' @exportPattern "^sb_setFlatnames"
#' @exportPattern "^sb_setDimnames"
#' @exportPattern "^currentBindings"
#' @exportPattern "^sb2_x"
#' @exportPattern "^sb2_wo"
#' @exportPattern "^sb2_set"
#' @exportPattern "^sb2_mod"
#' @exportPattern "^sb2_rec"
#' @exportPattern "^sb2_setVarnames"
#' @exportPattern "^idx"
#' @method `[` mutable_atomic
#' @method `[<-` mutable_atomic
#' 
NULL
#> NULL
