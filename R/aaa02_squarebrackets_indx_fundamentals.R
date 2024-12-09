#' Indexing Fundamentals
#'
#' @description
#' This help page explains the fundamentals regarding how 'squarebrackets' treats indexing. \cr
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
#' So indexing starts at `1`, and is inclusive (all matching indices are selected). \cr
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
#' sb_x(x, "a") # extracts all indices with the name "a"
#' 
#' sb_x(x, c("a", "a")) # repeats all indices with the name "a"
#' 
#' ```
#' 
#' 
#' Character indices are internally translated to integer indices using
#' \link{match_all}. \cr
#' \cr
#' \cr
#' \bold{Imaginary Numbers} \cr
#' A \link[base]{complex} vector `y` is structured as \cr
#' `y = a + b * i` \cr
#' where `Re(y)` returns `a`, and `Im(y)` returns `b`. \cr
#' squarebrackets' includes support for indexing through imaginary numbers (`Im(y)`) of \link[base]{complex} vectors. \cr
#' Indexing with imaginary numbers is a generalization of indexing with regular integers. \cr
#' \cr
#' It works as follows: \cr
#' Imaginary numbers that are positive integers,
#' like `1:10 * 1i`, work the same as regular integers. \cr
#' Imaginary numbers that are negative integers,
#' like `1:10 * -1i`,
#' index by counting backwards (i.e. from the end). \cr
#' Note that \bold{only} the `Imaginary` part of a complex vector is used (`Im(y)`); \cr
#' the `Real` part (`Re(y)`) is \bold{ignored}. \cr
#' \cr
#' See the results of the following code as an example:
#' 
#' ```{r eval = TRUE, echo = TRUE}
#' 
#' x <- 1:30 # vector of 30 elements
#' 
#' sb_x(x, 1:10 * 1i) # extract first 10 elements
#' 
#' sb_x(x, 1:10 * -1i) # extract last 10 elements
#' 
#' sb_x(x, 10:1 * -1i) # last 10 elements, in tail()-like order
#' 
#' ```
#' Thus complex vectors allow the user to choose between counting from the beginning,
#' like regular integers,
#' or backwards counting from the end. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Basic Indexing in Dimensionless Vectors:
#' Indexing in a vector is straight-forward, and very similar to base 'R'. \cr
#' An expression like \link{sb_x}`(x, i)`, for example, is equivalent to `x[i]`. \cr
#' \cr
#' \cr
#' 
#' @section Basic Indexing in Arrays:
#' Arrays and matrices (matrices are simply arrays with 2 dimensions)
#' distinguish between flat indices and subscripts. \cr
#' \cr
#' Flat indices, also called linear indices,
#' specifies the indices of an array as-if it is vector,
#' thus ignoring dimensions. \cr
#' So in an expression like \link{sb_x}`(x, i)` (equivalent to `x[i]`),
#' where `i` is a vector, `i` specifies flat indices. \cr
#' \cr
#' Matrices and arrays also have subscripts. \cr
#' Array subscripts work by specifying multiple indexing vectors,
#' which can be of different sizes,
#' where each vector specifies positions in a specific dimension. \cr
#' Given, for example, a 3-dimensional array,
#' the subscript `(1:10, 2:5, 3:9)`,
#' refers to rows 1 to 10, columns 2 to 5, and layers 3 to 9. \cr
#' \cr
#' The base S3 vector classes in 'R' use the standard Linear Algebraic convention,
#' as in academic fields like Mathematics and Statistics,
#' in the following sense: \cr
#'  * vectors are \bold{column} vectors (i.e. vertically aligned vectors);
#'  * index counting starts at `1`;
#'  * rows are the first dimension/subscript, columns are the second dimension/subscript, etc.
#' 
#' Thus, the orientation of flat indices in, for example,
#' a 4-rows-by-5-columns matrix, is as follows:
#' 
#' ```{r echo = FALSE, eval = TRUE, comment = NA}
#' matrix(1:20, ncol = 5)
#' ```
#' 
#' In a 4 by 4 matrix, subscript `(1, 2)` corresponds to flat index `5`. \cr
#' All array subscripts in 'squarebrackets' also follow this convention. \cr
#' \cr
#' The S3 methods in 'squarebrackets' implement subscripts through
#' the \link[=squarebrackets_indx_args]{sub, dims} argument pair. \cr
#' This argument pair allows specifying indices of an array,
#' without requiring a-priori knowledge on the number of dimensions the array has. \cr
#' \cr
#' \cr
#' 
#' @section Basic Indexing in data.frame-like Objects:
#' Data.frames distinguish between row- and column indices. \cr
#' These are very equivalent to base 'R'. \cr
#' \cr
#' Technically speaking, the column indices of a data.frame-like object is equal to its flat indices. \cr
#' I.e. for a data.frame, `x[i]` is essentially the same as `x[, i]`, safe for some attribute handling. \cr
#' To avoid confusion,
#' 'squarebrackets' does not have an argument for flat indices in its data.frame methods. \cr
#' \cr
#' \cr
#' 
#' @section Inverting:
#' Inverting indices entails to specify all elements \bold{except} the given indices. \cr
#' Consider for example the atomic vector `month.abb` (abbreviate month names). \cr
#' Given this vector, indices `1:5` gives `c("Jan" "Feb" "Mar" "Apr", "May")`. \cr
#' Inverting those same indices will give `c("Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")`. \cr
#' \cr
#' In base 'R', inverting an index is done in different ways. \cr
#' (negative numbers for numeric indexing, negation for logical indexing,
#' manually un-matching for character vectors). \cr
#' \cr
#' 'squarebrackets' provides a (somewhat) consistent syntax to invert indices: \cr
#' 
#'  - The methods that end with `_x` perform extraction; \cr
#'  to invert extraction,
#'  i.e. return the object \bold{without} the specified subset,
#'  use the methods that end with `_wo`. \cr
#'  - In the modification methods (`_mod_`/`_set_`) one can set the argument
#'  \link[=squarebrackets_indx_args]{inv}\code{ = TRUE} to invert indices. \cr \cr
#' 
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
#' sb_x(x, 1:5) # extract first 5 elements
#' 
#' sb_wo(x, 1:5) # return WITHOUT first 5 elements
#' 
#' 
#' sb_mod(x, 1:5, rp = "XXX") # copy, replace first 5 elements, return result
#' 
#' sb_mod(x, 1:5, inv = T, rp = "XXX") # same, but for all except first 5 elements
#' 
#' ```
#' 
#' \bold{ABOUT ORDERING} \cr
#' The order in which the user gives indices when inverting indices generally does not matter. \cr
#' The order of the indices as they appear in the original object `x` is maintained,
#' just like in base 'R'. \cr
#' Therefore, when replacing multiple values where the order of the replacement matters,
#' it is better to keep `inv = FALSE`, which is the default. \cr
#' For replacement with a single value or with a transformation function,
#' `inv = TRUE` can be used without considering the ordering. \cr
#' \cr
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
#' @section Regarding Performance:
#' Integer vectors created through the `:` operator are "ALTREP" integer vectors,
#' and provide the fastest way to specify indices. \cr
#' Indexing through names (i.e. character vectors) is the slowest. \cr
#' Complex vectors of imaginary numbers are somewhat in the middle
#' in terms of speed.
#' So if performance is important, use ALTREP integer indices. \cr
#' \cr
#' \cr
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
#' Regular subsets, AKA surface-level subset operations (`[`, `[<-` in base 'R'),
#' operate on the recursive object itself. \cr
#' I.e. \link{sb2_x}`(x, 1)`, or equivalently `x[1]`,
#' returns the \bold{list} `list(A = 1:10)`:
#' 
#' ```{r}
#' 
#' sb2_x(x, 1) # equivalent to x[1]; returns list(A = 1:10)
#' 
#' ```
#' 
#' 
#' 
#' Recursive subset operations (`[[`, `[[<-`, and `$` in base 'R'), on the other hand,
#' operate on an object a subset of the recursive object references to. \cr
#' I.e. \link{sb2_rec}`(x, 1)`, or equivalently `x[[1]]`,
#' returns the \bold{integer vector} `1:10`:
#' 
#' ```{r}
#' 
#' sb2_rec(x, 1) # equivalent to x[[1]]; returns 1:10
#' 
#' ```
#' 
#' Recursive objects can refer to other recursive objects,
#' which can themselves refer to recursive objects, and so on. \cr
#' Recursive subsets can go however deep you want. \cr
#' So, for example,
#' to extract the character vector `month.abb` from the aforementioned list `x`,
#' one would need to do: \cr
#' \link{sb2_rec}`(x, c("C","B"))`, (in base R: `x$C$B`):
#' 
#' ```{r}
#' 
#' sb2_rec(x, c("C","B")) # equivalent to x$C$B
#' 
#' # or:
#' 
#' sb2_rec(x, c(3, 2)) # equivalent to x[[3]][[2]]
#' 
#' ```
#' 
#' 
#' \bold{LIMITATIONS} \cr
#' Indexing in recursive subsets is significantly more limited than in regular
#' (or "shallow") subsets: 
#' 
#'  - Recursive subset operations
#'  using \link{sb2_rec}/\link{sb2_recin}
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
#' 
#' 
#' @rdname aaa02_squarebrackets_indx_fundamentals
#' @name aaa02_squarebrackets_indx_fundamentals
#' @aliases squarebrackets_indx_fundamentals
NULL

