#' squarebrackets: Subset Methods as Alternatives to the Square Brackets Operators for Programming
#' 
#' @description
#' squarebrackets: Subset Methods as Alternatives to the Square Brackets Operators for Programming \cr \cr
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
#'  * \bold{Performance aware}: \cr
#'  Despite the many checks performed, the functions are kept reasonably speedy,
#'  through the use of the 'Rcpp', 'collapse', and 'data.table' R-packages. \cr \cr
#'
#'
#' @section Supported Classes:
#' 'squarebrackets' only supports S3 classes,
#' and only those that primarily use square brackets for sub-setting
#' (hence the name of the package). \cr
#' \cr
#' Supported \link[=squarebrackets_immutable_classes]{immutable classes}: \cr
#' `atomic`, `list`, `data.frame`
#' (including `tibble`, `sf-data.frame`, and `sf-tibble`). \cr
#' \cr
#' Supported  \link[=squarebrackets_mutable_classes]{mutable classes}: \cr
#' \link{mutable_atomic}, `data.table`
#' (including `tidytable`, `sf-data.table`, and `sf-tidytable`). \cr
#' \cr
#' There are, of course, a lot of classes which are not supported by 'squarebrackets'. \cr
#' Most notably,
#' key-value stores - such as environments,
#' and the various 'collections' classes
#' from the 'collections' package - are not supported. \cr \cr
#
#'  
#' @section Methods and Functions:
#' 
#' `r .mybadge_intro_section("GENERIC METHODS", "darkgreen")` \cr
#' The main focus of this package is on its generic methods
#' and dimensional binding implementations. \cr
#' \cr
#' Generic methods for atomic objects
#' start with `sb_`. \cr
#' Generic methods for recursive objects (list, data.frame, etc.)
#' start with `sb2_`. \cr
#' The binding implementations for atomic dimensional objects (atomic arrays)
#' start with `bind_`. \cr
#' The binding implementations for recursive dimensional objects (recursive arrays, data.frames)
#' start with `bind2_`. \cr
#' There is also the somewhat separate \link{idx} method,
#' which works on both recursive and non-recursive objects. \cr
#' \cr
#' The available generic methods are the following:
#' 
#'  * \link{sb_x}, \link{sb2_x}: extract, exchange, or duplicate subsets.
#'  * \link{sb_rm},  \link{sb2_rm}: un-select/remove subsets.
#'  * \link{sb_set}, \link{sb2_set}: modify (transform or replace)
#'  subsets of a \link[=squarebrackets_mutable_classes]{mutable object}
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{sb_mod}, \link{sb2_mod}: return a \bold{(partial) copy}
#'  of an object with modified
#'  (transformed or replaced) subsets.
#'  * \link{sb2_rec}: access recursive subsets of lists.
#'  * \link{sb2_reccom}: replace, transform, remove, or add recursive subsets to a list,
#'  through R's default Copy-On-Modify semantics.
#'  * \link{sb_setFlatnames}, \link{sb_setDimnames}, \link{sb2_setVarnames}: change the names of
#'  a \link[=squarebrackets_mutable_classes]{mutable object}
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link[=bind]{bind_}, \link[=bind]{bind2_}:  implementations for binding dimensional objects.
#'  * \link{idx}: translate given indices/subscripts,
#'  for the purpose of copy-on-modify substitution.
#'  * \link{cp_seq}: construct parameters for margin-based sequences. \cr
#'  
#' So for example,
#' use `sb_rm()` to remove subsets from atomic arrays,
#' and use `sb2_rm()` to remove subsets from recursive arrays. \cr
#' See \link{squarebrackets_method_dispatch} for more information on how 'squarebrackets'
#' uses its S3 Method dispatch. \cr \cr
#' 
#' `r .mybadge_intro_section("SPECIALIZED FUNCTIONS", "darkred")` \cr
#' Additional specialized sub-setting functions are provided:
#' 
#'  * \link{lst_untree}: unnest tree-like nested list,
#'  to make vectorized sub-setting on recursive subsets of the list easier.
#'  * The \link[=dt_setcoe]{dt_}-functions
#'  to programmatically perform `data.table`-specific `[`-operations,
#'  with the security measures provided by the 'squarebrackets' package.
#'  * \link{setapply}: apply functions over mutable matrix margins
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}.
#'  * \link{ma_setv}: Find & Replace values in \link{mutable_atomic} objects
#'  using \link[=squarebrackets_PassByReference]{pass-by-reference semantics}. \cr
#'  This is considerably faster and more memory efficient than using \link{sb_set} for this.
#'  
#' 
#' `r .mybadge_intro_section("HELPER FUNCTIONS", "lightblue")` \cr
#' A couple of helper functions for creating ranges, sequences, and indices
#' (often needed in sub-setting)
#' are provided:
#' 
#'  * \link{currentBindings}: list or lock all currently existing bindings
#'  that share the share the same address as the input variable.
#'  * \link{n}: Nested version of \link[base]{c},
#'  and short-hand for \link[base]{list}.
#'  * \link{sub2coord}, \link{coord2ind}: Convert subscripts
#'  (array indices) to coordinates,
#'  coordinates to flat indices,
#'  and vice-versa.
#'  * \link{match_all}: Find all matches, of one vector in another,
#'  taking into account the order and any duplicate values of both vectors.
#'  * Computing indices: \cr
#'  \link{idx_r} to compute an integer index range. \cr
#'  \link{idx_by} to compute grouped indices. \cr
#'  \link[=idx_ord_v]{idx_ord_}-functions to compute ordered indices. \cr
#'  * Computing sequences: \cr
#'  \link{seq_rec2} for the recursive sequence generator
#'  (for example to generate a Fibonacci sequence). \cr  \cr
#' 
#' 
#' `r .mybadge_intro_section("DEVELOPER FUNCTIONS", "pink")` \cr
#' 
#' And finally some developer functions for constructing indices. \cr
#' These are also used internally by 'squarebrackets',
#' and package authors can use these to create additional sb_/sb2_ S3 methods,
#' or even entirely new subset-related functions.
#' 
#'  * \link[=tci_bool]{tci_} functions, for type-casting indices.
#'  * \link[=ci_flat]{ci_} functions, for constructing indices.
#'  * \link{indx_x} and \link{indx_rm}, for testing methods.
#' 
#' 
#' @author \strong{Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @references The badges shown in the documentation of this R-package were made using the services of: \url{https://shields.io/}
#' 
#' @name aaa0_squarebrackets_help
#' @rdname aaa0_squarebrackets_help
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
#' @exportPattern "^sb_setFlatnames"
#' @exportPattern "^sb_setDimnames"
#' @exportPattern "^currentBindings"
#' @exportPattern "^sb2_x"
#' @exportPattern "^sb2_rm"
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
