#' Value-based stride
#'
#' @description
#' `stride_v()` is used in the \link[=long_x]{long_} methods to specify
#' sub-set operations based on values in an atomic vector of properties. \cr
#' \cr
#' `stride_v()` can be used in the
#' \link[=long_x]{long_} methods
#' to perform value-based sub-setting operations. \cr
#' For a very basic understanding, consider the following illustration. \cr
#' In the simplest terms,
#' the sub-set operation `long_x(x, stride_v(y, v = v, na = na))` \cr
#' is conceptually equivalent to the following: \cr
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' 
#' x[ifelse(is.na(y), na, y == v)] # if `na` is `TRUE` or `FALSE`
#' x[is.na(y)] # if `na` is `NA`
#' 
#' ```
#' 
#' 
#' @param y a vector, with the same length as `x`, and ideally related to `x` \cr
#' For example, `y` may be the character vector `names(x)`,
#' the raw vector `broadcast::checkNA(x, "raw")`, the classless (i.e. raw data) values of `x`,
#' or even the raw data values of another long vector with the same length as `x`. \cr
#' Note that, in the default method,
#' `couldb.mutatomic(y)` must be `TRUE`, otherwise an error is returned.
#' @param v a scalar or vector, depending on the type of `y`,
#' indicating what values in `y` to look for. \cr
#' Details are given in the sections below. \cr
#' @param na `TRUE`, `FALSE`, or `NA`, indicating what to do with `NA`s/`NaN`s. \cr
#' If `na = TRUE`, `NA`s/`NaN`s are included in the sub-set operation (i.e. `NA`s/`NaN`s are extracted, removed, replaced, etc.). \cr
#' If `na = FALSE`, `NA`s/`NaN`s are excluded from the sub-set operation (i.e. `NA`s/`NaN`s are not extracted, not removed, not replaced, etc.). \cr
#' If `na = NA`, `v` is ignored, and \bold{only} `NA` values are searched for the sub-set operation. \cr
#' See also the additional sections below.
#' @param use `1` to check for specified condition,
#' `-1` to check for the negated condition (i.e. `!condition`).
#' @param ... not supported: all arguments in `stride_v()` other than `y` must be explicitly named.
#' 
#' @returns
#' An object of class "stride".
#' 
#' @seealso \link{squarebrackets_stride} \cr
#' 
#' @section The Basic Idea: 
#' 
#' The basic idea is as follows. \cr
#' Let `x` and `y` be 2 atomic vectors of the same length
#' (but they don't have to be of the same type). \cr
#' Let `v` be some atomic scalar of the same type as `y`. \cr
#' Given the result of the condition `y == v`,
#' the basic idea is to perform the following sub-set operations: \cr
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' long_x(x, stride_v(y, v = v))            # ==> x[y == v]
#' long_set(x, stride_v(y, v = v), rp = rp) # ==> x[y == v] <- rp
#' long_set(x, stride_v(y, v = v), tf = tf) # ==> x[y == v] <- tf(x[y == v]) 
#' 
#' ```
#' 
#' The above is with the default argument specification \code{use = 1}. \cr
#' Of course one can invert the relationship by specifying argument \code{use = -1},
#' to get something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' long_x(x, stride_v(y, v = v, use = -1))             # ==> x[p != v]                  
#' long_set(x, stride_v(y, v = v, use = -1), rp = rp)  # ==> x[p != v] <- rp
#' long_set(x, stride_v(y, v = v, use = -1), tf = tf)  # ==> x[p != v] <- tf(x[p != v])
#' 
#' ```
#' 
#' And `y` is allowed to be the same vector as `x`, of course. \cr
#' \cr
#' This basic idea, however, can become more complicated,
#' depending on the atomic type of `y`, which is discussed in the next section. \cr
#' \cr
#' \cr
#' 
#' @section Details per Atomic Type: 
#' \bold{Logical, Raw, Complex} \cr
#' For `y` of type `logical`, `raw`, and `complex`,
#' \link{stride_v} works exactly as explained in the previous section. \cr
#' `y` and `v` must be of the same atomic type. \cr
#' \cr
#' \cr
#' 
#' \bold{Numeric} \cr
#' For `y` of type `integer` or `double` (collectively referred to as "numeric"),
#' the basic idea laid-out before still holds: \cr
#' one can use atomic vector `y` and atomic scalar `v` to perform sub-set operations like \cr
#' `x[y == v]`. \cr
#' \cr
#' But one may be more interested in a range of numbers, rather than one specific number
#' (especially considering things like measurement error, and machine precision,
#' and greater-than/larger-than relationships). \cr
#' So for numeric `y`, one can also supply `v` of length \bold{2}. \cr
#' When `length(v) == 2L`, `long_` will check whether `y` is inside
#' (or outside if \code{use = -1}) the bounded range given by `v`. \cr
#' I.e. :
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' y >= v[1] & y <= v[2]
#' 
#' ```
#' 
#' Note that `y` and `v` must both be numeric here,
#' but they don't have to be the same type. \cr
#' I.e. one can have `y` of type `integer` and `v` of type `double`,
#' without problems. \cr
#' \cr
#' \cr
#' 
#' 
#' \bold{Character} \cr
#' For `y` of type `character`,
#' the basic idea is still to do something like `x[y == v]`. \cr
#' \cr
#' When searching for string `v` for sub-setting purposes,
#' one may want to take into consideration things like different spelling,
#' spacing, or even encodings of the same string. \cr
#' Implementing every form of fuzzy matching or encoding matching is computationally intensive,
#' and also quite beyond the scope of this package. \cr
#' Instead, the user may supply a character vector `v` of arbitrary length,
#' containing all the variations
#' (in terms of spelling, spacing, encoding, or whatever)
#' of all the strings to look for. \cr
#' \cr
#' So if a vector is given for `v` (instead of a single string),
#' the following check is performed:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' y %in% v
#' 
#' ```
#' 
#' \bold{NOTE} \cr
#' The order of `v` is \bold{irrelevant}. \cr
#' \cr
#' 
#' 
#' 
#' @section Smaller Than, Greater Than: 
#' For numeric `y`, one can specify a range for `v`, as explained earlier. \cr
#' But note one can also specify something like `v = c(-Inf, 4)`,
#' which essentially corresponds to the condition `y <= 4`. \cr
#' Thus, when `v` specifies a range, "greater-than" and "smaller-than" comparisons are also possible. \cr
#' \cr
#' \cr
#' 
#' @section Handling NAs and NaN:
#' We also have to handle the `NA`s and `NaN`s. \cr
#' The `na` argument can be used to specify what to do when
#' a `y` is `NA`. \cr
#' \cr
#' When `na = FALSE`, all `NA` values of `y` are always ignored. \cr
#' I.e. `long_x(x, stride_v(y, v = v, na = FALSE), use = 1)` will not extract `NA`s/`NaN`s, \cr
#' and `long_x(x, stride_v(y, v = v, na = FALSE, use = -1))` will not remove `NA`s/`NaN`s. \cr
#' \cr
#' When `na = TRUE`, `NA` values of `y` are always included. \cr
#' I.e. `long_x(x, stride_v(y, v = v, na = TRUE), use = 1)` will also extract `NA`s/`NaN`s, \cr
#' and `long_x(x, stride_v(y, v = v, na = TRUE, use = -1))` will also remove `NA`s/`NaN`s. \cr
#' \cr
#' One can also specify `na = NA`, 
#' which will ignore `v` completely,
#' and explicitly look for `NA`s/`NaN`s in `y` instead - like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' long_x(x, stride_v(y, na = NA))                        # ==> x[is.na(y)]                     
#' long_x(x, stride_v(y, na = NA, use = -1))             # ==> x[!is.na(y)]
#' long_set(x, stride_v(y, na = NA), rp = rp)             # ==> x[is.na(y)] <- rp                 
#' long_set(x, stride_v(y, na = NA, use = -1), rp = rp)  # ==> x[!is.na(y)] <- rp               
#' long_set(x, stride_v(y, na = NA), tf = tf)             # ==> x[is.na(y)] <- tf(x[is.na(y)])    
#' long_set(x, stride_v(y, na = NA, use = -1), tf = tf)  # ==> x[!is.na(y)] <- tf(x[!is.na(y)]) 
#' 
#' ```
#' 
#' Handling `NA`s/`NaN`s works the same for all atomic types. \cr
#' For `y` of type `complex`,
#' a value `p[i]` is considered `NA`
#' if `Re(y[i])` is `NA`/`NaN` and/or `Im(y[i])` is `NA`/`NaN`. \cr
#' \cr
#' Argument `v` is never allowed to contain `NA`/`NaN`. \cr
#' \cr
#' \cr
#' 
#' @section All in One:
#' 
#' Combining all of the above,
#' one can allocate indices in base 'R' to be equivalent to
#' the virtual indices produced by `stride_v(y, v = v, na = na, use = use)`,
#' with the following code:
#' 
#' ```{r, echo = TRUE, eval = FALSE}
#' 
#' # if `na = NA`:
#' ind <- which(is.na(y)) * sign(use)
#' 
#' # else if using scalar `v`:
#' ind <- which(ifelse(is.na(y), na, y == v)) * sign(use)
#' 
#' # else if using numeric range for `v`:
#' ind <- which(ifelse(is.na(y), na, y >= v[1] & y <= v[2])) * sign(use)
#' 
#' # else if using character vector for `v`:
#' ind <- which(ifelse(is.na(y), na, y %in% v)) * sign(use)
#' 
#' ```
#' 
#' 
#' @section Technical Details: 
#' On a technical level, the `stride_v()` method does the following:
#' 
#'  1) Determine if we go through vector `y` in one go, or in chunks. \cr
#'  If `lenght(y) >= 2^16`,
#'  vector `y` will be dealt with in
#'  `ceiling(length(x)^0.1)` equal-sized (so far as possible) chunks. \cr
#'  Otherwise, treat `y` in one go; i.e. in one chunk.
#'  2) Make a list equal to number of chunks from step one.
#'  3) For each chunk, count the number of condition matches (`count`),
#'  the location of the first ( `first`) match,
#'  and location of last (`last`) match.
#'  4) For each chunk, do the following. \cr
#'  If `count` is equal to `last - first + 1`, fill in list element for this chunk with `NULL`. \cr
#'  If `count <= 2`, fill in list element for this chunk with `NULL`. \cr
#'  Otherwise, fill list with `last - first + 1` bits (not bytes), specifying bit `1` if there's a match and bit `0` if no match. \cr
#'  The bits will be stored in 32 bit integers. Thus each integer holds 32 elements worth of bits. \cr \cr
#'  
#' In 'R', an expression like `x[y == v]` is internally translated to `x[which(y == v)]`. \cr
#' This means, 'R' will store 32 bits per element for the logical vector `y == v`,
#' and 64 bits per element for the numeric vector from `which(y == v)`. \cr
#' `stride_v()` stores information about the matches as 1 bit per condition (instead of 32 bits per condition),
#' and only for the regions (chunks) where there's a need to store such data. \cr
#' And `long_x()`/ `long_set()` will never call `which()`. \cr
#' As such, `stride_v()` will \bold{guarantee} to be \bold{at least} 32 times more memory efficient than the base 'R' approach. \cr
#' And the whole `long_x(x, stride_v(...))` / `long_set(x, stride_v(...))` operation
#' will in most practical cases use \bold{hundreds of times} less memory than the base 'R' approach! \cr
#' 
#' @example inst/examples/stride_v.R

#' @name stride_v
#' @rdname stride_v
#' @export
NULL
