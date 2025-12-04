#' Indexing Fundamentals of 'squarebrackets'
#'
#' @description
#' This help page explains the fundamentals regarding how 'squarebrackets' treats indexing. \cr
#' Some familiarity with base R's `[` and `[<-` operators is required to follow this help page. \cr
#' \cr
#' 
#' @section Indexing Forms:
#' 
#' Consider the following representation of array indices for a (in this case) 4 by 5 matrix:
#' 
#' ```{r echo = FALSE, eval = TRUE, comment = NA}
#' matrix(sprintf("[%d]", 1:20), 4, 5) |> noquote()
#' ```
#' 
#' The numbers `1` to `20` on the interior of this representation,
#' are referred to in this documentation as "interior indices"
#' (abbreviated as "ii"),
#' also known as "flat indices". \cr
#' The numbers on the edges of this representations,
#' `1` to `4` for the rows and `1` to `5` for the columns,
#' are referred to in this documentation as "subscripts"
#' (abbreviated as "ss"),
#' also known as "dimensional indices". \cr
#' Indexing by rows and columns, referred to as tabular indices,
#' is a commonly used special subset of using subscripts,
#' available only for data.frames and matrices. \cr
#' \cr
#' Thus 'squarebracets' supports these 3 forms of indexing: \cr
#' Indexing by interior indices, indexing by subscripts, and tabular indices. \cr
#' \cr
#' Regarding which kind of object supports which kind of indexing form:
#' 
#'  - Matrices, which are simply 2-dimensional arrays, support all 3 of the above given indexing forms.
#'  - Arrays in general can always support both interior indices and subscripts.
#'  - Dimensionless vectors (i.e. objects for which `dim()` returns `NULL`) only support interior indices.
#'  - Data.frames only support tabular indices. \cr \cr
#'  
#' Regarding which set of \link[=squarebrackets_methods]{methods} support which kind of indexing form:
#' 
#'  - One can operate on flat/interior indices (often simply referred to as "indices")
#'  using the \link[=squarebrackets_methods]{ii_} methods; \cr
#'  These primarily use the \link[=squarebrackets_indx_args]{i, use} argument pair.
#'  - One can operate on general subscripts (= dimensional indices)
#'  using the \link[=squarebrackets_methods]{ss_} methods; \cr
#'  These primarily use the the \link[=squarebrackets_indx_args]{s, use} argument pair.
#'  - One can operate on tabular indices
#'  using the  \link[=squarebrackets_methods]{sbt_} methods; \cr
#'  These primarily use the the
#'  \link[=squarebrackets_indx_args]{obs, vars} or \link[=squarebrackets_indx_args]{row, col}
#'  argument pair. \cr \cr
#' 
#' For the relationship between flat/interior indices and subscripts for arrays,
#' see the \link{ss2ii} help page. \cr
#' \cr
#' \cr
#' 
#' @section Indexing Types:
#' Base 'R' supports indexing through `logical`, `integer`, and `character` vectors. \cr
#' 'squarebrackets' supports these also (albeit with some improvements),
#' but also supports some additional methods of indexing. \cr
#' \cr
#' \cr
#' \bold{Whole numbers} \cr
#' Whole numbers are the most basic form on index selection. \cr
#' All forms of indexing in 'squarebrackets' are internally translated to integer
#' (or double if` > (2^31 - 1)`) indexing first,
#' ensuring consistency. \cr
#' Indexing through integer/numeric indices in 'squarebrackets' works the same as in base 'R',
#' except that negative values are not allowed. \cr
#' So indexing starts at `1` and is inclusive. \cr
#' \cr
#' \cr
#' \bold{Logical} \cr
#' Selecting indices with a logical vector in 'squarebrackets' works the same as in base 'R',
#' except that recycling is not allowed. \cr
#' \cr
#' \cr
#' \bold{Characters} \cr
#' When selecting indices using a character vector,
#' base 'R' only selects the first matches in the names. \cr
#' 'squarebrackets', however, selects all matches:
#' 
#' 
#' ```{r}
#' 
#' nms <- c("a", letters[4:1], letters[1:5])
#' x <- 1:10
#' names(x) <- nms
#' print(x) #' `x` has multiple elements with the name "a"
#' 
#' ii_x(x, "a") # extracts all indices with the name "a"
#' 
#' ii_x(x, c("a", "a")) # repeats all indices with the name "a"
#' 
#' ```
#' 
#' Character indices are internally translated to integer indices using
#' \link{match_all}. \cr
#' \cr
#' 
#' 
#' 
#' 
#' @section Inverting:
#' Inverting indices means to specify all elements \bold{except} the given indices. \cr
#' Consider for example the atomic vector `month.abb` (abbreviate month names). \cr
#' Given this vector, indices `1:5` gives `c("Jan" "Feb" "Mar" "Apr", "May")`. \cr
#' Inverting those same indices will give `c("Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")`. \cr
#' \cr
#' In base 'R', inverting an index is done in different ways. \cr
#' (negative numbers for numeric indexing, negation for logical indexing,
#' manually un-matching for character vectors). \cr
#' \cr
#' In 'squarebrackets', inverting is consistently done through the `use` argument: \cr
#' A positive sign for `use` means to select the specified indices,
#' a negative sign for `use` means to select all indices **except** the specified indices. \cr
#' \cr
#' 
#' \bold{EXAMPLES}
#' 
#' 
#' ```{r eval = TRUE, echo = TRUE}
#' 
#' x <- month.abb
#' print(x)
#' 
#' 
#' ii_x(x, 1:5) # extract first 5 elements
#' 
#' ii_x(x, 1:5, -1) # return WITHOUT first 5 elements
#' 
#' 
#' ii_mod(x, 1:5, rp = "XXX") # copy, replace first 5 elements, return result
#' 
#' ii_mod(x, 1:5, -1, rp = "XXX") # same, but for all except first 5 elements
#' 
#' ```
#' 
#' \bold{ABOUT ORDERING} \cr
#' The order in which the user gives indices when inverting indices generally does not matter. \cr
#' The order of the indices as they appear in the original object `x` is maintained,
#' just like in base 'R'. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Out-of-Bounds Integers, Non-Existing Names, and NAs:
#' 
#'  - Integer indices that are out of bounds (including `NaN` and `NA_integer_`) always give an error.
#'  - Character indices that specify non-existing names
#'  is considered a form of zero-length indexing. \cr
#'  Specifying `NA` names returns an error. \cr
#'  - Logical indices are translated internally to integers using \link[base]{which},
#'  and so `NA`s are ignored. \cr \cr
#'  
#'
#'
#' @section Index-less Sub-set Operations:
#' Until now this help page focussed on performing sub-set operations with an indexing vector. \cr
#' \cr
#' Performing sub-set operations on a long vector using a index vector
#' (which may itself also be a long vector)
#' is not very memory-efficient. \cr
#' 'squarebrackets' therefore introduces index-less sub-set operations,
#' through the \link{slice}\code{_} and \link{slicev}\code{_} methods. \cr
#' These methods are much more memory and computationally efficient than index-based sub-set methods
#' (and so also a bit better for the environment!). \cr
#' \cr
#' The \link{slice}\code{_} methods perform sequence based sub-set operations. \cr
#' \cr
#' The \link{slicev}\code{_} methods
#' (notice the "v" at the end)
#' perform value-based sub-set operations. \cr
#' Though this method is intentionally kept relatively simple,
#' it is still involved enough to warrant its own help page; \cr
#' for the details on value-based index-less sub-set operations,
#' please see \link{squarebrackets_slicev}. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Regarding Performance:
#' Integer vectors created through the `:` operator are "compact ALTREP" integer vectors,
#' and provide the fastest way to specify indices. \cr
#' Indexing through names (i.e. character vectors) is the slowest. \cr
#' \cr
#' Index-less sub-set operations are usually faster and more memory efficient
#' than any index-based sub-set operation. \cr
#' So if performance is important, use index-less sub-set operations,
#' or use compact ALTREP integer indices. \cr
#' \cr
#' \cr
#' 
#' 
#' 
#' @section Indexing in Recursive Subsets:
#' Until now this help page focussed on indexing for regular (or "shallow") subsets. \cr
#' This section will discuss indexing in recursive subsets. \cr
#' \cr
#' One of the differences between atomic and recursive objects,
#' is that recursive objects support recursive subsets, while atomic objects do not. \cr
#' \cr
#' Bear in mind that every element in a recursive object is a reference to another object. \cr
#' Consider the following list `x`:
#' 
#' ```{r}
#' 
#' x <- list(
#'    A = 1:10,
#'    B = letters,
#'    C = list(A = 11:20, B = month.abb)
#' )
#' 
#' ```
#' 
#' Regular subsets, AKA surface-level subset operations (`[`, `[<-` in base 'R'),
#' operate on the recursive object itself. \cr
#' I.e. \link{ii_x}`(x, 1)`, or equivalently `x[1]`,
#' returns the \bold{list} `list(A = 1:10)`:
#' 
#' ```{r}
#' 
#' ii_x(x, 1) # equivalent to x[1]; returns list(A = 1:10)
#' 
#' ```
#' 
#' Recursive subset operations (`[[`, `[[<-`, and `$` in base 'R'), on the other hand,
#' operate on an object a subset of the recursive object references to. \cr
#' I.e. \link{lst_rec}`(x, 1)`, or equivalently `x[[1]]`,
#' returns the \bold{integer vector} `1:10`:
#' 
#' ```{r}
#' 
#' lst_rec(x, 1) # equivalent to x[[1]]; returns 1:10
#' 
#' ```
#' 
#' Recursive objects can refer to other recursive objects,
#' which can themselves refer to recursive objects, and so on. \cr
#' Recursive subsets can go however deep you want. \cr
#' So, for example,
#' to extract the character vector `month.abb` from the aforementioned list `x`,
#' one would need to do: \cr
#' \link{lst_rec}`(x, c("C","B"))`, (in base R: `x$C$B`):
#' 
#' ```{r}
#' 
#' lst_rec(x, c("C","B")) # equivalent to x$C$B
#' 
#' # or:
#' 
#' lst_rec(x, c(3, 2)) # equivalent to x[[3]][[2]]
#' 
#' ```
#' 
#' 
#' \bold{LIMITATIONS} \cr
#' Indexing in recursive subsets is significantly more limited than in regular
#' (or "shallow") subsets: 
#' 
#'  - Recursive subset operations
#'  using \link{lst_rec}/\link{lst_recin}
#'  only support positive integer vectors and character vectors.
#'  - Imaginary numbers (using complex vectors) and logical vectors are not supported.
#'  - Since a recursive subset operation only operates on a single element,
#'  specifying the index with a character vector only selects the first matching element
#' (just like base 'R'), not all matches.
#'  - Inverting indices is also \bold{not} available for recursive indexing.
#'  - Unlike regular sub-setting, out-of-bounds specification for indices is acceptable,
#'  as it can be used to add new values to lists. \cr \cr
#' 
#' 
#' @section Non-Standard Evaluation:
#' 'squarebrackets' is designed primarily for programming,
#' and seeks to be fully programmatically friendly. \cr
#' As part of this endeavour,
#' 'squarebrackets' never uses Non-Standard Evaluation. \cr
#' All input for all methods and functions in 'squarebrackets'
#' are objects that can be stored in a variable. \cr
#' Like atomic vectors, lists, formulas, etc. \cr
#' \cr
#' 
#' 
#' @rdname aaa02_squarebrackets_indx_fundamentals
#' @name aaa02_squarebrackets_indx_fundamentals
#' @aliases squarebrackets_indx_fundamentals
NULL

