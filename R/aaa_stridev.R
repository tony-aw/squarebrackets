#' Value-based stride
#'
#' @description
#' `stride_pv()` is used in the \link[=long_x]{long_} methods to specify
#' sub-set operations based on values in an atomic vector of properties. \cr
#' \cr
#' `stride_pv()` can be used in the
#' \link[=long_x]{long_} methods
#' to perform some rather complex sub-setting operations; \cr
#' but for a very basic understanding, consider the following illustration. \cr
#' In the simplest terms,
#' the sub-set operation `long_x(x, stride_pv(p, v, na))` \cr
#' is conceptually equivalent to the following: \cr
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' 
#' x[ifelse(is.na(p), na, p == v)] # if `na` is `TRUE` or `FALSE`
#' x[is.na(p)] # if `na` is `NA`
#' 
#' ```
#' 
#' `countv()` is a helper function, that counts how often `v` appears in `p`.
#' The sections further below give more details. \cr
#' 
#' 
#' @param p class-less atomic vector of properties,
#' with the same length as `x`, and ideally related to `x` \cr
#' For example, `p` may be the character vector `names(x)`,
#' the raw vector `broadcast::checkNA(x, "raw")`, the classless (i.e. raw data) values of `x`,
#' or even the raw data values of another long vector with the same length as `x`. \cr
#' Note that `couldb.mutatomic(p)` must be `TRUE`, otherwise an error is returned.
#' @param v a scalar or vector, depending on the type of `p`,
#' indicating what values in `p` to look for. \cr
#' Details are given in the sections below. \cr
#' @param na `TRUE`, `FALSE`, or `NA`, indicating what to do with `NA`s/`NaN`s. \cr
#' If `na = TRUE`, `NA`s/`NaN`s are included in the sub-set operation (i.e. `NA`s/`NaN`s are extracted, removed, replaced, etc.). \cr
#' If `na = FALSE`, `NA`s/`NaN`s are excluded from the sub-set operation (i.e. `NA`s/`NaN`s are not extracted, not removed, not replaced, etc.). \cr
#' If `na = NA`, `v` is ignored, and \bold{only} `NA` values are searched for the sub-set operation. \cr
#' See also the additional sections below.
#' @param use `1` to check for specified condition, `-1` to check for the negated condition (i.e. `!condition`).
#' 
#' @returns
#' An object of class "stride".
#' 
#' @seealso \link{squarebrackets_stride} \cr
#' 
#' @section The Basic Idea: 
#' 
#' The basic idea is as follows. \cr
#' Let `x` and `p` be 2 atomic vectors of the same length
#' (but they don't have to be of the same type). \cr
#' Let `v` be some atomic scalar of the same type as `p`. \cr
#' Given the result of the condition `y == v`,
#' the basic idea is to perform the following sub-set operations: \cr
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' long_x(x, stride_pv(p, v))            # ==> x[p == v]
#' long_set(x, stride_pv(p, v), rp = rp) # ==> x[p == v] <- rp
#' long_set(x, stride_pv(p, v), tf = tf) # ==> x[p == v] <- tf(x[p == v]) 
#' 
#' ```
#' 
#' The above is with the default argument specification \code{use = 1}. \cr
#' Of course one can invert the relationship by specifying argument \code{use = -1},
#' to get something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' long_x(x, stride_pv(p, v), use = -1)             # ==> x[p != v]                  
#' long_set(x, stride_pv(p, v), use = -1, rp = rp)  # ==> x[p != v] <- rp
#' long_set(x, stride_pv(p, v), use = -1, tf = tf)  # ==> x[p != v] <- tf(x[p != v])
#' 
#' ```
#' 
#' And `p` is allowed to be the same vector as `x`, of course. \cr
#' \cr
#' This basic idea, however, can become more complicated,
#' depending on the atomic type of `p`, which is discussed in the next section. \cr
#' \cr
#' \cr
#' 
#' @section Details per Atomic Type: 
#' \bold{Logical, Raw, Complex} \cr
#' For `p` of type `logical`, `raw`, and `complex`,
#' \link{stride_pv} works exactly as explained in the previous section. \cr
#' `p` and `v` must be of the same atomic type. \cr
#' \cr
#' \cr
#' 
#' \bold{Numeric} \cr
#' For `p` of type `integer` or `double` (collectively referred to as "numeric"),
#' the basic idea laid-out before still holds: \cr
#' one can use atomic vector `p` and atomic scalar `v` to perform sub-set operations like \cr
#' `x[p == v]`. \cr
#' \cr
#' But one may be more interested in a range of numbers, rather than one specific number
#' (especially considering things like measurement error, and machine precision,
#' and greater-than/larger-than relationships). \cr
#' So for numeric `p`, one can also supply `v` of length \bold{2}. \cr
#' When `length(v) == 2L`, `long_` will check whether `p` is inside
#' (or outside if \code{use = -1}) the bounded range given by `v`. \cr
#' I.e. :
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' p >= v[1] & p <= v[2]
#' 
#' ```
#' 
#' Note that `p` and `v` must both be numeric here,
#' but they don't have to be the same type. \cr
#' I.e. one can have `p` of type `integer` and `v` of type `double`,
#' without problems. \cr
#' \cr
#' \cr
#' 
#' 
#' \bold{Character} \cr
#' For `p` of type `character`,
#' the basic idea is still to do something like `x[p == v]`. \cr
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
#' p %in% v
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
#' For numeric `p`, one can specify a range for `v`, as explained earlier. \cr
#' But note one can also specify something like `v = c(-Inf, 4)`,
#' which essentially corresponds to the condition `y <= 4`. \cr
#' Thus, when `v` specifies a range, "greater-than" and "smaller-than" comparisons are also possible. \cr
#' \cr
#' \cr
#' 
#' @section Handling NAs and NaN:
#' We also have to handle the `NA`s and `NaN`s. \cr
#' The `na` argument can be used to specify what to do when
#' a `p` is `NA`. \cr
#' \cr
#' When `na = FALSE`, all `NA` values of `p` are always ignored. \cr
#' I.e. `long_x(x, stride_pv(p, v, na = FALSE), use = 1)` will not extract `NA`s/`NaN`s, \cr
#' and `long_x(x, stride_pv(p, v, na = FALSE), use = -1)` will not remove `NA`s/`NaN`s. \cr
#' \cr
#' When `na = TRUE`, `NA` values of `p` are always included. \cr
#' I.e. `long_x(x, stride_pv(p, v, na = TRUE), use = 1)` will also extract `NA`s/`NaN`s, \cr
#' and `long_x(x, stride_pv(p, v, na = TRUE), use = -1)` will also remove `NA`s/`NaN`s. \cr
#' \cr
#' One can also specify `na = NA`, 
#' which will ignore `v` completely,
#' and explicitly look for `NA`s/`NaN`s in `p` instead - like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' long_x(x, stride_pv(y, na = NA))                        # ==> x[is.na(y)]                     
#' long_x(x, stride_pv(y, na = NA), use = -1)             # ==> x[!is.na(y)]
#' long_set(x, stride_pv(y, na = NA), rp = rp)             # ==> x[is.na(y)] <- rp                 
#' long_set(x, stride_pv(y, na = NA), use = -1, rp = rp)  # ==> x[!is.na(y)] <- rp               
#' long_set(x, stride_pv(y, na = NA), tf = tf)             # ==> x[is.na(y)] <- tf(x[is.na(y)])    
#' long_set(x, stride_pv(y, na = NA), use = -1, tf = tf)  # ==> x[!is.na(y)] <- tf(x[!is.na(y)]) 
#' 
#' ```
#' 
#' Handling `NA`s/`NaN`s works the same for all atomic types. \cr
#' For `p` of type `complex`,
#' a value `p[i]` is considered `NA`
#' if `Re(p[i])` is `NA`/`NaN` and/or `Im(p[i])` is `NA`/`NaN`. \cr
#' \cr
#' Argument `v` is never allowed to contain `NA`/`NaN`. \cr
#' \cr
#' \cr
#' 
#' @section All in One:
#' 
#' Combining all of the above,
#' one can allocate indices in base 'R' to be equivalent to
#' the virtual indices produced by `stride_pv(p, v, na), use)`,
#' with the following code:
#' 
#' ```{r, echo = TRUE, eval = FALSE}
#' 
#' # if `na = NA`:
#' ind <- which(is.na(p)) * sign(use)
#' 
#' # else if using scalar `v`:
#' ind <- which(ifelse(is.na(p), na, p == v)) * sign(use)
#' 
#' # else if using numeric range for `v`:
#' ind <- which(ifelse(is.na(p), na, p >= v[1] & p <= v[2])) * sign(use)
#' 
#' # else if using character vector for `v`:
#' ind <- which(ifelse(is.na(p), na, p %in% v)) * sign(use)
#' 
#' ```
#' 
#' 
#' @example inst/examples/stride_pv.R

#' @name stride_pv
#' @rdname stride_pv
#' @export
NULL
