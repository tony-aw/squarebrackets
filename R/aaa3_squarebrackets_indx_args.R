#' Index Arguments in the Generic Sub-setting Methods
#'
#' @description
#' There are 6 types of arguments that can be used
#' in the generic methods of 'squarebrackets' to specify the indices to perform operations on:
#' 
#'  * `i`: to specify flat (i.e. dimensionless) indices.
#'  * `row, col`: to specify rows and/or columns in tabular objects.
#'  * `idx, dims`: to specify indices of arbitrary dimensions in arrays.
#'  * `rcl`: to specify rows (first dimension),
#'  columns (second dimension),
#'  and layers (third dimension),
#'  in arrays that have exactly 3 dimensions.
#'  * `lvl`: specify levels, for factors only.
#'  * `filter, vars`: to specify rows and/or columns specifically in data.frame-like objects. \cr \cr
#' 
#' In this help page `x` refers to the object on which subset operations are performed. \cr
#' \cr
#' 
#' @section Argument i:
#' `r .mybadge_class("atomic vector")` \cr
#' `r .mybadge_class("factor")` \cr
#' `r .mybadge_class("recursive vector")` \cr
#' 
#' 
#' Any of the following can be specified for argument `i`:
#' 
#'  * `NULL`, only for multi-dimensional objects or factors,
#'  when specifying the other arguments
#'  (i.e. dimensional indices or factor levels.)
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation
#'  (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with indices.
#'  * a \bold{logical vector} (without `NA`s!),
#'  of the same length as `x`,
#'  giving the indices to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If an object has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#'  * a \bold{function} that takes as input `x`,
#'  and returns a logical vector,
#'  giving the element indices to select for the operation. \cr
#'  For atomic objects, `i` is interpreted as `i(x)`. \cr
#'  For lists, `i` is interpreted as `lapply(x, i)`. \cr
#'
#' 
#' Using the `i` arguments corresponds to doing something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  sb_x(x, i = i) # ==> x[i]
#'  
#' ```
#' 
#' For a brief explanation of the relationship between flat indices (`i`),
#' and the dimension indices (`row`, `col`, etc.),
#' see the `Details` section in \link{sub2ind}. \cr
#' 
#' 
#' @section Arguments row, col:
#' `r .mybadge_class("atomic matrix")` \cr
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' Any of the following can be specified for the arguments `row` / `col`:
#' 
#'  * `NULL` (default), corresponds to a missing argument,
#'  which results in ALL of the indices in this dimension being selected for the operation.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with dimension indices to select for the operation.
#'  * a \bold{logical} vector (without `NA`s!) of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If an object has multiple indices with the given name,
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
#' @section Arguments idx, dims:
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#'  
#' `idx` must be a list of indices. \cr
#' `dims` must be an integer vector of the same length as `idx`,
#' giving the dimensions to which the indices given in `idx` correspond to. \cr
#' The elements of `idx` follow the same rules as the rules for `row` and `col`,
#' EXCEPT one should not fill in `NULL`. \cr
#' NOTE: The arguments `idx` and `dims` will be ignored if `i` is specified. \cr
#' \cr
#' To keep the syntax short,
#' the user can use the \link{n} function instead of `list()` to specify `idx`. \cr
#' \cr
#' Using the `idx, dims` arguments,
#' corresponds to doing something like the following,
#' here using an example of a 4-dimensional array:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' sb_x(x, n(1:10, 1:5), c(1, 3)) # ==> x[1:10, , 1:5, , drop = FALSE]
#' 
#' ```
#' 
#' @section Arguments rcl:
#' `r .mybadge_class("atomic array")` \cr
#' 
#' The `rcl` argument is only applicable for atomic arrays with exactly 3 dimensions. \cr
#' If the user knows a-priori that an array has 3 dimensions,
#' using `rcl` is more efficient than using the `idx, dims` arguments. \cr
#' \cr
#' The `rcl` argument must be a list of exactly 3 elements,
#' with the first element giving the indices of the first dimension (rows),
#' the second element giving the indices of the second dimension (columns),
#' and the third element giving the indices of the third and last dimension (layers);
#' thus `rcl` stands for "rows, columns, layers"
#' (i.e. the 3 dimensions of a 3-dimensional array). \cr
#' For each of the aforementioned 3 elements of the list `rcl`,
#' any of the following can be specified:
#' 
#'  * `NULL`, corresponds to a missing argument,
#'  which results in ALL of the indices in this dimension being selected for the operation.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with dimension indices to select for the operation.
#'  * a \bold{logical} vector (without `NA`s!) of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If an object has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#' 
#' By default `rcl` is not a list but simply `NULL`,
#' to be used when specifying the other arguments (either `idx, dims` or `i`). \cr
#' \cr
#' To keep the syntax short,
#' the user can use the \link{n} function instead of `list()` to specify `rcl`. \cr 
#' \cr
#' Using the `rcl` argument corresponds to doing something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' sb_x(x, rcl = n(NULL, 1:10, 1:5)) # ==> x[, 1:10, 1:5, drop = FALSE]
#' 
#' ```
#' 
#' @section Argument lvl:
#' `r .mybadge_class("factor")` \cr
#' 
#' For this argument, the names of the levels of `x` can be given,
#' selecting the corresponding indices for the operation. \cr
#' \cr
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
#' @section Argument inv:
#' `r .mybadge_all_classes()` \cr
#' 
#' Relevant for \link{sb_mod}, \link{sb_set}, and \link{idx}. \cr
#' By default, `inv = FALSE`, which translates the indices like normally. \cr
#' When `inv = TRUE`, the inverse of the indices is taken. \cr
#' Consider, for example, an atomic matrix `x`; \cr
#' using `sb_mod(x, 1:2, 1:2, tf = tf)`
#' corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[1:2, 1:2] <- tf(x[1:2, 1:2])
#' x
#' 
#' ```
#' 
#' and using `sb_mod(x, 1:2, 1:2, inv = TRUE, tf = tf)`
#' corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[-1:-2, -1:-2] <- tf(x[-1:-2, -1:-2])
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
#' The arguments are  evaluated in the following order: 
#' 
#'  1) Argument `i`
#'  2) Argument `lvl` (for factors) or argument `rcl` (for 3-dimensional arrays)
#'  3) The rest of the indexing arguments.
#' 
#' One cannot specify `row` and `filter` simultaneously.
#' It's either one or the other. Similarly,
#' one cannot specify `col` and `vars` simultaneously. \cr
#' In the above cases it holds that if one set is specified, the other is set is ignored. \cr
#' \cr
#' 
#' @section Drop:
#' Sub-setting with the generic methods from the 'squarebrackets' R-package using dimensional arguments
#' (`row, col, lyr, idx, dims, filter, vars`)
#' always use `drop = FALSE`. \cr
#' To drop potentially redundant (i.e. single level) dimensions,
#' use the \link[base]{drop} function, like so:
#' 
#' ```
#'  sb_x(x, row = row, col = col) |> drop() # ==> x[row, col, drop = TRUE]
#'  
#' ```
#' 
#' 
#' @section First, Last, and Shuffle:
#' The indices are counted forward. I.e. `1` is the first element, not the last. \cr
#' One can use the \link[data.table]{last} function to get the last `N` indices. \cr
#' \cr
#' One can use the \link[data.table]{first} function to get the first `N` indices. \cr
#' \cr
#' To shuffle elements of indices, use the \link[base]{sample} function. \cr
#' \cr
#' 
#' 
#' @section Regarding Performance:
#' Integer indices and logical indices are the fastest. \cr
#' Indexing through names or levels (i.e. character vectors) is the slowest. \cr
#' Thus if performance is important, use integer or logical indices. \cr \cr


#' @rdname aaa3_squarebrackets_indx_args
#' @name aaa3_squarebrackets_indx_args
#' @aliases squarebrackets_indx_args
NULL

