#' Index Arguments in the Generic Sub-setting Methods
#'
#' @description
#' There are several types of arguments that can be used
#' in the generic methods of 'squarebrackets' to specify the indices to perform operations on:
#' 
#'  * `i, use`: to specify interior (i.e. dimensionless) indices.
#'  * `s, use`: to specify subscripts of arbitrary dimensions
#'  in arrays.
#'  * `row, col, use`:
#'  to specify rows and columns in tabular objects (matrices and data.frames). \cr
#'  *  `slice, use`: to specify indices of one particular dimension
#'  (for arrays and data.frame-like objects). \cr
#'  Currently only used in the `_icom` methods. \cr \cr
#'  
#' For the fundamentals of indexing in 'squarebrackets',
#' see \link{squarebrackets_indx_fundamentals}. \cr
#' In this help page `x` refers to the object on which subset operations are performed. \cr
#' \cr
#' \cr
#' 
#' 
#' 
#' @section Argument Pair `i, use`:
#' `r .mybadge_class("atomic vector")` \cr
#' `r .mybadge_class("derived atomic vector")` \cr
#' `r .mybadge_class("recursive vector")` \cr
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#' 
#' `i` specifies the interior (or flat) indices. \cr
#' `use` can be `1` or `-1`: \cr
#' `1` means the specified indices in `i` are to be used for the sub-set operation. \cr
#' `-1L` means **all** indices **except** the indices in `i` are to be used for the sub-set operation. \cr \cr
#' 
#' Any of the following can be specified for argument `i`:
#' 
#'  * `NULL` or `0L`, corresponds to missing argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation
#'  (i.e. empty selection).
#'  * a numeric vector of \bold{strictly positive whole numbers} giving indices.
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
#'  For recursive objects, `i` is interpreted as `lapply(x, i)`.
#'  * a formula; see \link{keywords}. \cr \cr
#'
#' 
#' Using the `i` arguments corresponds to doing something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  ii_x(x, i = i) # ==> x[i]   # if `x` is atomic
#'  ii_x(x, i = i) # ==> x[i]  # if `x` is recursive
#'  
#' ```
#' 
#' If `i` is a function, it corresponds to the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  ii_x(x, i = i) # ==> x[i(x)] # if `x` is atomic
#'  ii_x(x, i = i) # ==> x[lapply(x, i)] # if `x` is recursive
#'  
#' ```
#' 
#' 
#' @section Argument Pair `s, use`:
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#' The `s, use` argument pair, inspired by the
#' \code{abind::}\link[abind]{asub} function from the 'abind' package,
#' is the primary indexing argument for sub-set operations on dimensional objects. \cr
#' \cr
#' The `s` argument specifies the
#' \bold{subscripts}
#' (i.e. dimensional indices). \cr
#' The **absolute value** of `use` argument gives the dimensions for which the 
#' `s` holds (i.e. `use` specifies the "non-missing" margins),
#' and the sign (+ or -) of `use` specifies if the indices are to be selected or excluded,
#' similar to the `use` argument used in combination with `i` (see previous section). \cr
#' \cr
#' Specifically, the `use` argument can be any of the following:
#'  
#'  * a vector of length `0`, which will be interpreted as "all subscripts are missing".
#'  * an integer vector;
#'  * the real scalar `Inf`, which will be interpreted as `1:dim(x)`;
#'  * the real scalar `-Inf`, which will be interpreted as `-1:-ndim(x)`. \cr
#' 
#' `use` is not allowed to have any duplicate values,
#' nor is it allowed to include zero (`0`)`.` \cr
#' The default value for `use` is \bold{lazy evaluated} \code{1:}\link{ndim}\code{(x)}. \cr
#' 
#' `s` must be an atomic vector, a list of length 1, or a list of the same length as `use`. \cr
#' If `s` is a list of length 1,
#' it is internally recycled to become the same length as `use`. \cr
#' If `s` is an atomic vector,
#' it is internally treated as `list(s)`,
#' and (as with the previous case) recycled to become the same length as `use`. \cr
#' \cr
#' Each element of `s` when `s` is a list, or `s` as a whole when `s` is atomic,
#' can be any of the following:
#' 
#'  * `NULL` or `0L`, which corresponds to a missing index argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * A formula, with optional keywords, giving an expression to evaluate. \cr
#'  See \link{keywords}. \cr
#'  * a numeric vector of \bold{strictly positive whole numbers}
#'  with indices of the specified dimension to select for the operation.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the indices of the specified dimension to select for the operation.
#'  * a \bold{character} vector giving the `dimnames` to select. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#'  * a formula; see \link{keywords}. \cr \cr
#'  
#'  
#' To keep the syntax short,
#' the user can use the \link{n} function instead of `list()` to specify `s`. \cr
#' \cr
#' \bold{EXAMPLES} \cr
#' Here are some examples for clarity,
#' using an atomic array `x` of 3 dimensions:
#' 
#'  * `ss_x(x, n(1:10, 1:5), c(1, -3))`,
#'  extracts the first 10 rows, extracts all columns, and **removes** the first 5 layers,
#'  of array `x`. \cr
#'  `ss_x(x, n(1:10, 0L, 1:5))` is the same.
#'  * `ss_x(x, n(1:10), 2)` \cr
#'  extracts the first 10 columns of array `x`.
#'  * `ss_x(x, 1:10)`, \cr
#'  extracts the first 10 rows, columns, and layers of array `x`.
#'  * `ss_x(x, 1:10, c(1, 3))`, \cr
#'  extracts the first 10 rows, all columns, and the first 10 layers,
#'  of array `x`. \cr
#' 
#' I.e.:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' ss_x(x, n(1:10, 1:5), c(1, -3)) # ==> x[1:10, , -1:-5, drop = FALSE]
#' 
#' ss_x(x, n(1:10, 0L, 1:5))      # ==> x[1:10, , 1:5, drop = FALSE]
#' 
#' ss_x(x, 1:10, 2)               # ==> x[ , 1:10, , drop = FALSE]
#' 
#' ss_x(x, 1:10)                  # ==> x[1:10, 1:10, 1:10, drop = FALSE]
#' 
#' ss_x(x, 1:10, c(1, 3))         # ==> x[1:10, , 1:10, drop = FALSE]
#' 
#' ```
#' 
#' For a brief explanation on the relationship between flat indices (`i`)
#' and subscripts (`s`, `use`) in arrays,
#' see \link{ss2ii}. \cr
#' \cr
#' 
#' 
#' @section Arguments set `row, col, use`:
#' `r .mybadge_class("atomic matrix")` \cr
#' `r .mybadge_class("recursive matrix")` \cr
#' 
#' Specifies rows and columns in a matrix.
#'  The argument `row` and `col` can each be any of the following:
#' 
#'  * `NULL` or `0L`, which corresponds to a missing index argument.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a numeric vector of \bold{strictly positive whole numbers}
#'  with dimension indices to select for the operation.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the dimension indices to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#'  * a formula; see \link{keywords}. \cr
#'  
#' For data.frames, `col` can also be a function. \cr
#' \cr
#' `use` is set to `1:2` by default. \cr
#' If `use` is `c(-1, 2)` or `-1`, the row indices will be inverted
#' (i.e. select all rows **except** those specified in `row`). \cr
#' If `use` is `c(1, -2)` or `-2`, the column indices will be inverted
#' (i.e. select all columns **except** those specified in `col`). \cr
#' If `use` is `-1:-2`, both the row- and column- indices will be inverted. \cr
#' The order of `use` is irrelevant; i.e. `c(-1, 2)` is the same as `c(2, -1)`. \cr
#' \cr
#' 
#' 
#' @section Argument Pair `slice, use`:
#' `r .mybadge_class("atomic array")` \cr
#' `r .mybadge_class("recursive array")` \cr
#' `r .mybadge_class("data.frame-like")` \cr
#' 
#' Relevant only for the `_icom` methods. \cr
#' The absolute value of `use` specifies the dimension on which argument `slice` is used. \cr
#' The sign of `use`, just like in the previous argument sets, specifies whether to invert the indices or not. \cr
#' I.e. if `use = 1` , `slice` specifies which rows to select; \cr
#' if `use = -2`, `slice` specifies which columns to **not** select; \cr
#' etc. \cr
#' \cr
#' The `slice` argument can be any of the following:
#' 
#'  * `NULL` or `0L`, which corresponds to a missing index argument.
#'  * a numeric vector of \bold{strictly positive whole numbers}
#'  with dimension indices to select for the operation.
#'  * a \bold{logical} vector of the same length as the corresponding dimension size,
#'  giving the dimension indices to select for the operation.
#'  * a \bold{character} vector of index names. \cr
#'  If a dimension has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation. \cr
#'  * a formula; see \link{keywords}.
#'  
#' One could also give a vector of length `0` for `slice`; \cr
#' Argument `slice` is only used in the `_icom` methods,
#' and the results are meant to be used inside the regular `[` and `[<-` operators. \cr
#' Thus the effect of a zero-length index specification depends on the rule-set of
#' `[.class(x)` and `[<-.class(x)`. \cr \cr
#' 
#' 
#' 
#' 
#' @section All Missing Indices:
#' `NULL` and `0L` in the indexing arguments correspond to a missing argument. \cr
#' For `s, use`, specifying `use` of length 0 also corresponds to all subscripts being missing. \cr
#' Thus, for the `_x` methods,
#' using missing indexing arguments for all indexing arguments corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[]
#' 
#' ```
#' 
#' Similarly, for the `_mod` and `_set` methods,
#' using missing or `NULL` indexing arguments corresponds to something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' x[] <- rp # for replacement
#' x[] <- tf(x) # for transformation
#' 
#' ```
#' 
#' The above is true \bold{even if} `sign(use) < 0`. \cr \cr
#' 
#' 
#' 
#' @section Drop:
#' Sub-setting with the generic methods from the 'squarebrackets' R-package using dimensional arguments
#' (`s, row, col, use`)
#' always use `drop = FALSE`. \cr
#' To drop potentially redundant (i.e. single level) dimensions,
#' use the \link[base]{drop} function, like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#'  ss_x(x, s, use) |> drop() # ==> x[..., drop = TRUE]
#'  
#' ```
#' 
#' 
#' @references Plate T, Heiberger R (2016). \emph{abind: Combine Multidimensional Arrays}.
#' R package version 1.4-5, \url{https://CRAN.R-project.org/package=abind}.
#' 


#' @rdname aaa04_squarebrackets_indx_args
#' @name aaa04_squarebrackets_indx_args
#' @aliases squarebrackets_indx_args
NULL

