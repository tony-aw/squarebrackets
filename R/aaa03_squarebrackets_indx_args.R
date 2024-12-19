#' Index Arguments in the Generic Sub-setting Methods
#'
#' @description
#' There are several types of arguments that can be used
#' in the generic methods of 'squarebrackets' to specify the indices to perform operations on:
#' 
#'  * `i`: to specify flat (i.e. dimensionless) indices.
#'  * `s, d`: to specify indices of arbitrary dimensions in arrays
#'  (including matrices, which inherit from arrays).
#'  *  `margin, slice`: to specify indices of one particular dimension (for arrays and data.frame-like objects).
#'  * `row, col`: to specify rows and/or columns in specifically in data.frame-like objects.
#'  * `filter, vars`: to specify rows and/or columns specifically in data.frame-like objects. \cr \cr
#'  
#' For the fundamentals of indexing in 'squarebrackets',
#' see \link{squarebrackets_indx_fundamentals}. \cr
#' In this help page `x` refers to the object on which subset operations are performed. \cr
#' \cr
#' \cr
#' 
#' 
#' 
#' @section Argument i:
#' `r .mybadge_class("atomic vector")` \cr
#' `r .mybadge_class("derived atomic vector")` \cr
#' `r .mybadge_class("recursive vector")` \cr
#' 
#' 
#' Any of the following can be specified for argument `i`:
#' 
#'  * `NULL`, corresponds to missing argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation
#'  (i.e. empty selection).
#'  * a numeric vector of \bold{strictly positive whole numbers} with indices.
#'  * a \bold{complex} vector, as explained in \link{squarebrackets_indx_fundamentals}.
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
#'  sb_x(x, i = i) # ==> x[i]   # if `x` is atomic
#'  sb2_x(x, i = i) # ==> x[i]  # if `x` is recursive
#'  
#' ```
#' 
#' If `i` is a function, it corresponds to the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb_x(x, i = i) # ==> x[i(x)] # if `x` is atomic
#'  sb2_x(x, i = i) # ==> x[lapply(x, i)] # if `x` is recursive
#'  
#' ```
#' 
#' 
#' @section Argument Pair s, d:
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#' The `s, d` argument pair is inspired by the
#' \code{abind::}\link[abind]{asub} function from the 'abind' package
#' (see reference below). \cr
#' \cr
#' The `s` argument specifies the
#' \link[=squarebrackets_indx_fundamentals]{subscripts}
#' (i.e. dimensional indices). \cr
#' The `d` argument gives the dimensions for which the 
#' \link[=squarebrackets_indx_fundamentals]{subscripts} `s` holds
#' (i.e. `d` specifies the "non-missing" margins). \cr
#' \cr
#' The `d` argument must be an integer vector. \cr
#' \cr
#' `s` must be a list of length 1, or a list of the same length as `d`. \cr
#' If `s` is a list of length 1,
#' it is internally recycled to become the same length as `d`. \cr
#' \cr
#' Each element of `s` can be any of the following:
#' 
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a numeric vector of \bold{strictly positive whole numbers}
#'  with indices of the specified dimension to select for the operation.
#'  * a \bold{complex} vector, as explained in \link{squarebrackets_indx_fundamentals}.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the indices of the specified dimension to select for the operation.
#'  * a \bold{character} vector giving the `dimnames` to select. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation. \cr \cr
#'  
#' Note the following:
#'  * As stated, `d` specifies which index margins are non-missing. \cr
#'  If `d` is of length `0`,
#'  it is taken as "all index margins are missing".
#'  * The default value for `d` is \code{1:}\link{ndims}\code{(x)}. \cr
#'  
#' To keep the syntax short,
#' the user can use the \link{n} function instead of `list()` to specify `s`. \cr
#' \cr
#' \bold{EXAMPLES} \cr
#' Here are some examples for clarity,
#' using an atomic array `x` of 3 dimensions:
#' 
#'  * `sb_x(x, n(1:10, 1:5), c(1, 3))` \cr
#'  extracts the first 10 rows, all columns, and the first 5 layers,
#'  of array `x`. \cr
#'  * `sb_x(x, n(1:10), 2)` \cr
#'  extracts the first 10 columns of array `x`. \cr
#'  * `sb_x(x, n(1:10))`, \cr
#'  extracts the first 10 rows, columns, and layers of array `x`. \cr
#'  * `sb_x(x, n(1:10), c(1, 3))`, \cr
#'  extracts the first 10 rows, all columns, and the first 10 layers,
#'  of array `x`. \cr
#' 
#' I.e.:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' sb_x(x, n(1:10, 1:5), c(1, 3)) # ==> x[1:10, , 1:5, drop = FALSE]
#' 
#' sb_x(x, n(1:10), 2)               # ==> x[ , 1:10, , drop = FALSE]
#' 
#' sb_x(x, n(1:10))                  # ==> x[1:10, 1:10, 1:10, drop = FALSE]
#' 
#' sb_x(x, n(1:10), c(1, 3))         # ==> x[1:10, , 1:10, drop = FALSE]
#' 
#' ```
#' 
#' \bold{NOTE} \cr
#' If `length(d)` is 1,
#' `s` can also be given as an atomic vector (of any length),
#' instead of a list of length 1. \cr
#' Although it is allowed for `s` and `d` to both be atomic vectors of length 1,
#' for the readability of your code it is \bold{highly recommended}
#' that `s` and `d` be explicitly \bold{named} in your method call,
#' in such a case. \cr
#' I.e.: \cr
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' sb_x(x, 1, 1) # BAD: this is not very readable
#' 
#' sb_x(x, s = 1, d = 1) # This is GOOD
#' 
#' ```
#' 
#' 
#' For a brief explanation of the relationship between flat indices (`i`)
#' and subscripts (`s`, `d`) in arrays,
#' see \link{sub2ind}. \cr \cr
#' 
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
#'  * a numeric vector of \bold{strictly positive whole numbers}
#'  with dimension indices to select for the operation.
#'  * a \bold{complex} vector, as explained in \link{squarebrackets_indx_fundamentals}.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the dimension indices to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation. \cr
#'  
#' One could also give a vector of length `0` for `slice`; \cr
#' Argument `slice` is only used in the \link{idx} method ,
#' and the result of \link{idx} are meant to be used inside the regular `[` and `[<-` operators. \cr
#' Thus the effect of a zero-length index specification depends on the rule-set of
#' `[.class(x)` and `[<-.class(x)`. \cr \cr
#' 
#' 
#' 
#' 
#' @section Arguments filter, vars:
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' The `filter` and  `vars` arguments are inspired by the `subset` and `select`
#' arguments of base R's \link[base]{subset}\code{.data.frame} method.
#' However, the `filter` and  `vars` arguments do \bold{not} use
#' non-standard evaluation,
#' as to keep 'squarebrackets' fully programmatically friendly. \cr
#' \cr
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
#' `vars` can be one of the following:
#' 
#'  - a \bold{function} that returns a logical vector,
#'  giving the column indices to select for the operation. \cr
#'  For example, to select all numeric columns,
#'  specify `vars = is.numeric`.
#'  - a \bold{character} vector of column names / variable names. \cr \cr
#' 
#' 
#' @section Arguments row, col:
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' Any of the following can be specified for the arguments `row` / `col`:
#' 
#'  * `NULL` (default), corresponds to a missing argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a numeric vector of \bold{strictly positive whole numbers}
#'  with indices of the specified dimension to select for the operation.
#'  * a \bold{complex} vector, as explained in \link{squarebrackets_indx_fundamentals}.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the indices of the specified dimension to select for the operation.
#'  * a \bold{character} vector giving the `dimnames` to select. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation. \cr
#'  
#' 
#' Using the `row, col` arguments corresponds to doing something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb2_x(x, row, col) # ==> x[row, col, drop = FALSE]
#'  
#' ```
#' 
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
#' using `sb_mod(x, 1:2, 2L, tf = tf)`
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
#' @section All Missing Indices:
#' `NULL` in the indexing arguments corresponds to a missing argument. \cr
#' For `s, d`, specifying `d` of length 0 also corresponds to all subscripts being missing. \cr
#' Thus, for \bold{both} \link{sb_x}/\link{sb2_x} and \link{sb_wo}/\link{sb2_wo},
#' using missing or `NULL` indexing arguments for all indexing arguments corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[]
#' 
#' ```
#' 
#' Similarly, for \link{sb_mod}/\link{sb2_mod} and \link{sb_set}/\link{sb2_set},
#' using missing or `NULL` indexing arguments corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[] <- rp # for replacement
#' x[] <- tf(x) # for transformation
#' 
#' ```
#' 
#' The above is true \bold{even if} `inv = TRUE` and/or `red = TRUE`. \cr \cr
#' 
#' 
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
#' One cannot specify the `s, d` pair and `slice, margin` pair simultaneously;
#' it's either one pair or the other pair. \cr
#' In the above cases it holds that if one set is specified, the other is set is ignored. \cr
#' \cr
#' 
#' @section Drop:
#' Sub-setting with the generic methods from the 'squarebrackets' R-package using dimensional arguments
#' (`s, d, row, col filter, vars`)
#' always use `drop = FALSE`. \cr
#' To drop potentially redundant (i.e. single level) dimensions,
#' use the \link[base]{drop} function, like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb_x(x, s, d) |> drop() # ==> x[..., drop = TRUE]
#'  
#' ```
#' 
#' 
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}.
#' R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#' 


#' @rdname aaa03_squarebrackets_indx_args
#' @name aaa03_squarebrackets_indx_args
#' @aliases squarebrackets_indx_args
NULL

