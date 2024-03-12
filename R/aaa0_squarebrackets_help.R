#' squarebrackets: Methods as an Alternative to the Square Brackets Operators
#' 
#' @description
#' squarebrackets: Methods as an Alternative to the Square Brackets Operators \cr \cr
#' 
#' 
#' @section Motivation:
#' 
#' In order to perform subset operations on some array `x`
#' with the square brackets operator (`[`, `[<-`),
#' one needs to know how many dimensions it has. \cr
#' I.e. if `x` has 3 dimensions, one would use \cr
#' `x[i, j, k, drop = FALSE]`  or `x[i, j, k] <- value` \cr
#' But how would one the use the square brackets operators,
#' when number of dimensions of `x` is not known a-priori? \cr
#' It’s not impossible, but still rather convoluted. \cr
#' \cr
#' The \link{data.frame}, \link[tibble]{tibble}, \link[data.table]{data.table},
#' and \link[tidytable]{tidytable} classes
#' all inherit from class “data.frame”. \cr
#' Yet they use different rules regarding the usage of the square bracket operators. \cr
#' Constantly switching between these rules is annoying,
#' and makes one's code inconsistent. \cr
#' \cr
#' When selecting names for sub-setting,
#' only the first occurrences of the names are selected for the sub-set; \cr
#' and when un-selecting/removing names for sub-setting,
#' the syntax is very different from selecting names. \cr
#' \cr
#' ‘R’ adheres to copy-on-modification semantics when replacing values using \code{[<-}. \cr
#' But sometimes one would like explicit control when to create a copy,
#' and when to modify using pass-by-reference semantics. \cr
#' \cr
#' \cr
#' Among programming languages,
#' 'R' has perhaps one of the most
#' flexible and comprehensive sub-setting functionality,
#' provided by the square brackets operators (`[`, `[<-`). \cr
#' But as shown in the above cases,
#' the square brackets operators
#' are occasionally less than optimally convenient. \cr
#' \cr
#' There are some packages that solve some of these issues,
#' but I have not found an R-package that provides a holistic approach
#' to providing alternative methods to the square brackets operators. \cr
#' \cr
#' Thus, this R package was born. \cr \cr
#' 
#' 
#' @section Goal & Properties:
#' 
#' The Goal of the 'squarebrackets' package is not to replace the square-brackets operators per-sé
#' (see \link[base]{Extract}),
#' but to provide \bold{alternative} sub-setting methods and functions,
#' to be used in situations where the square-brackets operators are inconvenient. \cr
#' These are (hopefully) easier sub-setting methods and functions,
#' with the following properties:
#' 
#'  * \bold{Programmatically friendly}:
#'    * Name-based arguments instead of position-based arguments.
#'    * Unlike base `[`,
#'    it's not required to know the number of dimensions of an array a-priori,
#'    to perform subset-operations on the object.
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
#' No support for mutable list-like classes,
#' such as
#' the various \link[collections]{collections} classes
#' from the 'collections' package,
#' and the \link[fastmap]{fastmap} class
#' from the 'fastmap' package,
#' as their sub-setting method is not primarily based on square-brackets. \cr \cr
#'   
#'  
#' @section Methods and Functions:
#' 
#' `r .mybadge_intro_section("GENERIC METHODS", "darkgreen")` \cr
#' The main focus is on the following generic methods:
#' 
#'  * \link{sb_x}: extract, exchange, or duplicate subsets.
#'  * \link{sb_rm}: un-select/remove subsets.
#'  * \link{sb_set}: modify (transform or replace)
#'  subsets of a \link[=squarebrackets_mutable_classes]{mutable object}
#'  using \link[=squarebrackets_PassByReference]{Pass By Reference} semantics.
#'  * \link{sb_mod}: return a \bold{copy}
#'  of an object with modified
#'  (transformed or replaced) subsets.
#'  * \link{sb_coe}: coerce and transform a whole object,
#'   or a recursive subset of an object.
#'  * \link{sb_before}, \link{sb_after}: insert new values before or after an index
#'  along a dimension of an object.
#'  * \link{sb_rec}: accesses recursive subsets of lists.
#'  * \link{sb_currentBindings}: list all currently existing bindings
#'  that share the share the same address as the input variable. \cr \cr
#' 
#' `r .mybadge_intro_section("SPECIALIZED FUNCTIONS", "darkred")` \cr
#' Additional specialized sub-setting functions are provided:
#'  
#'  * \link{setapply}: apply functions over mutable matrix margins
#'  using \link[=squarebrackets_PassByReference]{Pass By Reference} semantics.
#'  * \link{sb_str}: extract or replace a subset of characters of a single string
#'  (each single character is treated as a single element).
#'  * \link{sb_a}: extract multiple attributes from an object.
#'  * The \link[=dt_setcoe]{dt_}-functions for `data.table`-specific `[`-operations. \cr \cr
#' 
#' `r .mybadge_intro_section("HELPER FUNCTIONS", "lightblue")` \cr
#' And finally,
#' a couple of helper functions for creating ranges, sequences, and indices
#' (often needed in sub-setting)
#' are provided:
#' 
#'  * \link{n}: Nested version of \link[base]{c},
#'  and short-hand for \link[base]{list}.
#'  * Computing indices: \cr
#'  \link{idx_by} to compute grouped indices. \cr
#'  \link[=idx_ord_v]{idx_ord_}-functions to compute ordered indices.
#'  * \link{match_all}: Find all matches, of one vector in another,
#'  taking into account the order and any duplicate values of both vectors.
#'  * Computing sequences: \cr
#'  \link{seq_rec} for the recursive sequence generator
#'  (for example to generate a Fibonacci sequence). \cr
#'  \link{seq_names} to create a range of indices from a specified starting and ending name. \cr
#'  \link{seq_mlen} to create multiple sequences of the given lengths at once.
#'  * \link{sub2coord}, \link{coord2ind}: Convert subscripts
#'  (array indices) to coordinates,
#'  coordinates to flat indices,
#'  and vice-versa. \cr \cr
#' 
#' 
#' @author \strong{Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @seealso
#' 
#' 'squarebrackets' relies on the 'collapse', 'Rcpp', and 'data.table' R-packages
#' to ensure an acceptable performance of its functions despite the many checks that these functions perform. \cr
#' I also recommend using these packages for other sub-setting and data wrangling functionalities. \cr
#' \cr
#' 'squarebrackets' uses a modified version of the \link[abind]{abind} function from the 'abind' R-package;
#' the 'abind' package is recommended for binding and sub-filling arrays of arbitrary dimensions. \cr
#' \cr
#' Besides these package,
#' the following R packages work very nicely together with 'squarebrackets':
#' 
#'  * 'stringi': \cr
#'  The primary R package for fast and concise string manipulation - an essential part of any programming language.
#'  * 'tinycodet': \cr
#'  Helps the user with their coding etiquette.
#'  Focuses on 4 aspects: (1) safe functionalities;
#'  (2) an import system that combines benefits of using a package without attaching, and attaching a package;
#'  (3) extending the capabilities of the 'stringi' package;
#'  (4) functions to reduce repetitive code.
#' 
#' @references The badges shown in the documentation of this R-package were made using the services of: \url{https://shields.io/}
#' 
#' @docType package
#' @name aaa0_squarebrackets
#' @rdname aaa0_squarebrackets
#' @aliases squarebrackets-package
#' @aliases squarebrackets
#' @aliases squarebrackets_help
#' @useDynLib squarebrackets, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @exportPattern "sb_x"
#' @exportPattern "sb_rm"
#' @exportPattern "sb_set"
#' @exportPattern "sb_mod"
#' @exportPattern "sb_coe"
#' @exportPattern "sb_before"
#' @exportPattern "sb_after"
#' @exportPattern "sb_rec"
#' @method `[` mutable_atomic
#' @method `[<-` mutable_atomic
#' 
NULL
#> NULL
