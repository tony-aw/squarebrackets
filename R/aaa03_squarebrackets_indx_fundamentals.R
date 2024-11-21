#' Indexing Fundamentals
#'
#' @description
#' This help page explains the fundamentals regarding how 'squarebrackets' treats indexing. \cr
#' \cr
#' 
#' @section Indexing Types:
#' Base 'R' supports indexing through `logical`, `integer`, and `character` vectors. \cr
#' 'squarebrackets' suppports these also (albeit with some improvements),
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
#' \cr
#' \cr
#' \bold{Logical} \cr
#' Selecting indices with a logical vector in 'squarebrackets' works the same as in base 'R',
#' except that recycling is not allowed. \cr
#' Thus the logical vector must be of the correct length
#' (i.e. `length(x)` or `dim(x)[L]`, depending on the situation). \cr
#' \cr
#' \cr
#' \bold{Characters} \cr
#' When selecting indices using a character vector,
#' base 'R' only selects the first matches in the names. \cr
#' 'squarebrackets', however, selects ALL matches. \cr
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
#' index by counting backwards (i.e. from the end),
#' where the integer indices are computed as `n + Im(y) + 1L`. \cr
#' Here `n` is the maximum possible integer
#' (i.e. `length(x)`, or `dim(x)[L]`, depending on the situation),
#' and `Im(y)` is negative. \cr
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
#' or counting from the end. \cr
#' \cr
#' \cr
#' 
#' @section Subscripts: 
#' One can distinguish between flat indices, array subscripts, and data.frame subscripts. \cr
#' \cr
#' Flat indices, also called linear indices,
#' specifies the index of a vector, ignoring dimensions (if any are present). \cr
#' So in an expression like `x[i]`, where `i` is a vector, `i` specifies flat indices. \cr
#' \cr
#' Matrices and arrays also have array subscripts. \cr
#' Array subscripts works by specifying multiple indexing vectors,
#' which can be of different sizes,
#' where each vector specifies positions in a specific dimension. \cr
#' Given, for example, a 3-dimensional array,
#' the subscript `[1:10, 2:5, 3:9]`
#' refers to rows 1 to 10, columns 2 to 5, and layers 3 to 9. \cr
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
#' In a 4 by 4 matrix, subscript `[1,2]` corresponds to flat index `5`. \cr
#' All array subscripts in 'squarebrackets' also follow this convention. \cr
#' \cr
#' Data.frame-like objects use data.frame subscripts. \cr
#' At first glance this may seem the same as the array subscripts of matrices,
#' but they are not: \cr
#' The column indices of a data.frame-like object is equal to its flat indices. \cr
#' I.e. for a data.frame, `x[i]` is essentially the same as `x[, i]`, safe for some attribute handling. \cr
#' To avoid confusion,
#' 'squarebrackets' does not have an argument for flat indices in its data.frame methods. \cr
#' \cr
#' Flat indices (or just "indices" for non-dimensional objects)
#' exist for all objects
#' (in data.frame-like objects, flat indices are actually equal to column indices). \cr
#' Thus flat indices are the "default" indices, and are usually just referred to as simply "indices". \cr
#' \cr
#' \cr
#' 
#' 
#' @section Indexing in Recursive Subsets:
#' One of the differences between atomic and recursive objects,
#' is that recursive objects support recursive subsets, while atomic objects do not. \cr
#' \cr
#' Bear in mind that every element in a recursive object is a reference to another object. \cr
#' Consider the following list `x`:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x <- list(A = letters, B = 1:10, C = list(A = 11:20, B = LETTERS))
#' 
#' ```
#' Regular subsets, AKA surface-level subset operations (`[`, `[<-` in base 'R'),
#' operate on the recursive object itself. \cr
#' I.e. \link{sb2_x}`(x, 1)`, or equivalently `x[1]`,
#' returns the list `list(A = letters)`. \cr
#' Recursive subset operations (`[[`, `[[<-` in base 'R'), on the other hand,
#' operate on an object a subset of the recursive object references to. \cr
#' I.e. \link{sb2_rec}`(x, 1)`, or equivalently `x[[1]]`,
#' returns the \bold{character vector} `letters`. \cr
#' \cr
#' Recursive objects can refer to other recursive objects,
#' which can themselves refer to recursive objects, and so on. \cr
#' Recursive subsets can go however deep you want. \cr
#' So, for example,
#' to extract the character vector `LETTERS` from the aforementioned list `x`,
#' one would need to do: \cr
#' \link{sb2_rec}`(x, c("C","B"))`, or equivalently, `x[["C"]][["B"]]`. \cr
#' You can also do this using integers of course: \link{sb2_rec}`(x, c(3, 2))`. \cr
#' \cr
#' Note that recursive subset operations
#' using \link{sb2_rec}/\link{sb2_recin}
#' only support positive integer vectors and character vectors; \cr
#' imaginary numbers (using complex vectors) and logical vectors are not supported. \cr
#' Moreover, since a recursive subset operation only operates on a single element,
#' specifying the index with a character vector only selects the first matching element
#' (just like base 'R'), not all matches. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Regarding Performance:
#' Integer indices and logical indices are the fastest. \cr
#' Indexing through names (i.e. character vectors) is the slowest. \cr
#' Thus if performance is important, use integer or logical indices. \cr
#' \cr
#' \cr
#' 
#' 
#' 
#' @rdname aaa03_squarebrackets_indx_fundamentals
#' @name aaa03_squarebrackets_indx_fundamentals
#' @aliases squarebrackets_indx_fundamentals
NULL

