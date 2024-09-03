#' Index Arguments in the Generic Sub-setting Methods
#'
#' @description
#' There are 6 types of arguments that can be used
#' in the generic methods of 'squarebrackets' to specify the indices to perform operations on:
#' 
#'  * `i`: to specify flat (i.e. dimensionless) indices.
#'  * `row, col`: to specify rows and/or columns in tabular objects.
#'  * `sub, dims`: to specify indices of arbitrary dimensions in arrays.
#'  * `filter, vars`: to specify rows and/or columns specifically in data.frame-like objects.
#'  *  `margin, slice`: to specify indices of one particular dimension. \cr \cr
#' 
#' In this help page `x` refers to the object on which subset operations are performed. \cr
#' \cr
#' \cr
#' 
#' @section Fundamentals:
#' 
#' Base 'R' supports indexing through `logical`, `integer`, and `character` vectors. \cr
#' 'squarebrackets' suppports these also (albeit with some improvements),
#' but also supports some additional methods of indexing. \cr
#' \cr
#' \cr
#' \bold{Integers} \cr
#' Integers are the most basic form on index selection. \cr
#' All forms of indexing in 'squarebrackets' are internally translated to integer indexing first,
#' ensuring consistency. \cr
#' Indexing through integers in 'squarebrackets' works the same as in base 'R',
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
#' \bold{Complex Numbers} \cr
#' Unlike base 'R',
#' squarebrackets' includes support for indexing through `complex` vectors. \cr
#' Indexing with complex numbers is a generalization of indexing with regular integers. \cr
#' \cr
#' It works as follows: \cr
#' A complex vector is essentially structured as `Real + Imaginary`. \cr
#' The Real part must be \bold{strictly positive integers}. \cr
#' The Imaginary part must be \bold{constant}.
#' 
#'  * If `Imaginary` is positive or zero, it works the same as integer values.
#'  * If `Imaginary` is negative, indexing counts backwards,
#'  where the integer indices are computed as `n - Real + 1L`.
#' 
#' Where `n` is the maximum possible integer
#' (i.e. `length(x)`, or `dim(x)[L]`, depending on the situation). \cr
#' \cr
#' See the results of the following code as an example:
#' 
#' ```{r eval = TRUE, echo = TRUE}
#' 
#' x <- 1:30 # vector of 30 elements
#' 
#' sb_x(x, 1:10 + 1i) # extract first 10 elements
#' 
#' sb_x(x, 1:10 - 1i) # extract last 10 elements
#' 
#' sb_x(x, 10:1 - 1i) # last 10 elements, in tail()-like order
#' 
#' ```
#' Thus complex vectors allow the user to choose between counting from the beginning,
#' like regular integers,
#' or counting from the end. \cr
#' \cr
#' \cr
#' What follows are detailed descriptions of the common arguments
#' in 'squarebrackets' used to select indices. \cr
#' \cr
#' 
#' 
#' 
#' @section Argument i:
#' `r .mybadge_class("atomic vector")` \cr
#' `r .mybadge_class("factor")` \cr
#' `r .mybadge_class("recursive vector")` \cr
#' 
#' 
#' Any of the following can be specified for argument `i`:
#' 
#'  * `NULL`, corresponds to missing argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation
#'  (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with indices.
#'  * a \bold{complex} vector, as explained at the start of this help page.
#'  * a \bold{logical vector},
#'  of the same length as `x`,
#'  giving the indices to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If an object has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#'  * a \bold{function} that takes as input `x`,
#'  and returns a logical vector,
#'  giving the element indices to select for the operation. \cr
#'  For atomic objects, `i` is interpreted as `i(x)`. \cr
#'  For recursive objects, `i` is interpreted as `lapply(x, i)`. \cr
#'  \cr
#'
#' 
#' Using the `i` arguments corresponds to doing something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb_x(x, i = i) # ==> x[i]
#'  
#' ```
#' 
#' 
#' 
#' @section Arguments row, col:
#' `r .mybadge_class("atomic matrix")` \cr
#' `r .mybadge_class("recursive matrix")` \cr
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' Any of the following can be specified for the arguments `row` / `col`:
#' 
#'  * `NULL` (default), corresponds to a missing argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with dimension indices to select for the operation.
#'  * a \bold{complex} vector, as explained at the start of this help page.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#'  
#' NOTE: The arguments `row` and `col` will be ignored if `i` is specified.
#' 
#' Using the `row, col` arguments corresponds to doing something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb_x(x, row = row, col = col) # ==> x[row, col, drop = FALSE]
#'  
#' ```
#' 
#' @section Argument Pair sub, dims:
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#'  The `sub, dims` argument pair is inspired by the
#'  \code{abind::}\link[abind]{asub} function from the 'abind' package (see reference below). \cr
#' `dims` must be an integer vector of the same length as `sub`,
#' giving the dimensions over which to select indices
#' (i.e. `dims` specifies the "non-missing" index margins). \cr
#' `sub` must be a list of subscripts, of the same length as `dims`.
#' Each element of `sub` can be any of the following:
#' 
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with dimension indices to select for the operation.
#'  * a \bold{complex} vector, as explained at the start of this help page.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation. \cr
#'  
#' Note also the following:
#'  * If `dims` is a single integer,
#'  one can specify `sub` as an atomic vector of any of the above specified types,
#'  instead of as a list of length `1`.
#'  * As stated, `dims` specifies which index margins are non-missing. \cr
#'  If `dims` - and thus also `sub` - is of length `0`,
#'  it is taken as "all index margins are missing". \cr
#'  
#' To keep the syntax short,
#' the user can use the \link{n} function instead of `list()` to specify `sub`. \cr
#' \cr
#' Using the `sub, dims` arguments,
#' corresponds to doing something like the following,
#' here using an example of extracting subsets from a 4-dimensional array:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' sb_x(x, n(1:10, 1:5), c(1, 3)) # ==> x[1:10, , 1:5, , drop = FALSE]
#' 
#' ```
#' 
#' For a brief explanation of the relationship between flat indices (`i`),
#' and dimensional subscripts (`sub`, `dims`),
#' see the `Details` section in \link{sub2ind}. \cr \cr
#' 
#' 
#' 
#' 
#' @section Arguments filter, vars:
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' `filter` must be a one-sided formula
#' with a single logical expression using the column names of the data.frame,
#' giving the condition which observation/row indices should be selected for the operation. \cr
#' For example,
#' to perform an operation on the rows for which column `height > 2` and for which column `sex != "female"`,
#' specify the following formula:
#' 
#' ```{r, eval = FALSE}
#' ~ (height > 2) & (sex != "female")
#' ```
#' 
#' If the formula is linked to an environment,
#' any variables not found in the data set will be searched from the environment. \cr
#' \cr
#' `vars` must be a function that returns a logical vector,
#' giving the column indices to select for the operation. \cr
#' For example, to select all numeric columns, specify `vars = is.numeric`. \cr
#' \cr
#' 
#' @section Argument Pair margin, slice:
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' Relevant only for the \link{idx} method. \cr
#' The `margin` argument specifies the dimension on which argument `slice` is used. \cr
#' I.e. when `margin = 1`, `slice` selects rows; \cr
#' when `margin = 2`, `slice` selects columns; \cr
#' etc. \cr
#' \cr
#' The `slice` argument can be any of the following:
#' 
#'  * a \bold{strictly positive integer} vector with dimension indices to select for the operation.
#'  * a \bold{complex} vector, as explained at the start of this help page.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the dimension indices to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation. \cr
#'  
#' One could also give a vector of length `0` for `slice`; \cr
#' Argument `slice` is only used in the \link{idx} method ,
#' and the result of \link{idx} are meant to be used inside the regular `[` and `[<-` operators. \cr
#' Thus the result of a zero-length index specification depends on the rule-set of
#' `[.class(x)` and `[<-.class(x)`. \cr \cr
#' 
#' 
#' @section Argument inv:
#' `r .mybadge_all_classes()` \cr
#' 
#' Relevant for the \link{sb_mod}/\link{sb2_mod}, \link{sb_set}/\link{sb2_set},
#' and \link{idx} methods. \cr
#' By default, `inv = FALSE`, which translates the indices like normally. \cr
#' When `inv = TRUE`, the inverse of the indices is taken. \cr
#' Consider, for example, an atomic matrix `x`; \cr
#' using `sb_mod(x, col = 1:2, tf = tf)`
#' corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[, 1:2] <- tf(x[, 1:2])
#' x
#' 
#' ```
#' 
#' and using `sb_mod(x, col = 1:2, inv = TRUE, tf = tf)`
#' corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[, -1:-2] <- tf(x[, -1:-2])
#' x
#' 
#' ```
#' 
#' \bold{NOTE} \cr
#' The order in which the user gives indices when `inv = TRUE` generally does not matter. \cr
#' The order of the indices as they appear in the original object `x` is maintained,
#' just like in base 'R'. \cr
#' Therefore, when replacing multiple values where the order of the replacement matters,
#' it is better to keep `inv = FALSE`, which is the default. \cr
#' For replacement with a single value or with a transformation function,
#' `inv = TRUE` can be used without considering the ordering. \cr \cr
#' 
#' 
#' @section All NULL indices:
#' `NULL` in the indexing arguments corresponds to a missing argument. \cr
#' Thus, for \bold{both} \link{sb_x} and \link{sb_rm},
#' using `NULL` for all indexing arguments corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[]
#' 
#' ```
#' 
#' Similarly, for \link{sb_mod} and \link{sb_set},
#' using `NULL` corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[] <- rp # for replacement
#' x[] <- tf(x) # for transformation
#' 
#' ```
#' 
#' The above is true \bold{even if} `inv = TRUE` and/or `drop = TRUE`. \cr \cr
#' 
#' 
#' @section Out-of-Bounds Integers, Non-Existing Names/Levels, and NAs:
#' 
#'  - Integer indices that are out of bounds (including `NaN` and `NA_integer_`) always give an error.
#'  - Specifying non-existing names/levels (including `NA_character_`) as indices
#'  is considered a form of zero-length indexing.
#'  - Logical indices are translated internally to integers using \link[base]{which},
#'  and so `NA`s are ignored. \cr \cr
#' 
#' @section Disallowed Combinations of Index Arguments:
#' 
#' One cannot specify `i` and  the other indexing arguments simultaneously;
#' it's either `i`, or the other arguments. \cr
#' \cr
#' One cannot specify `row` and `filter` simultaneously;
#' it's either one or the other. \cr
#' One cannot specify `col` and `vars` simultaneously;
#' it's either one or the other. \cr
#' One cannot specify the `sub, dims` pair and `slice, margin` pair simultaneously;
#' it's either one pair or the other pair. \cr
#' In the above cases it holds that if one set is specified, the other is set is ignored. \cr
#' \cr
#' 
#' @section Drop:
#' Sub-setting with the generic methods from the 'squarebrackets' R-package using dimensional arguments
#' (`row, col, lyr, sub, dims, filter, vars`)
#' always use `drop = FALSE`. \cr
#' To drop potentially redundant (i.e. single level) dimensions,
#' use the \link[base]{drop} function, like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb_x(x, row = row, col = col) |> drop() # ==> x[row, col, drop = TRUE]
#'  
#' ```
#' 
#' 
#' @section Regarding Performance:
#' Integer indices and logical indices are the fastest. \cr
#' Indexing through names or levels (i.e. character vectors) is the slowest. \cr
#' Thus if performance is important, use integer or logical indices. \cr \cr
#' 
#' 
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}. R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#' 


#' @rdname aaa3_squarebrackets_indx_args
#' @name aaa3_squarebrackets_indx_args
#' @aliases squarebrackets_indx_args
NULL

