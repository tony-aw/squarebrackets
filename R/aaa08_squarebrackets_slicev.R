#' On Index-Less Value-Based Sub-Set Operations
#'
#' @description
#' This help page explains the details on the arguments used in the 
#' \link{slicev}\code{_} methods and the \link{countv} function. \cr
#' \cr
#' 
#' 
#' 
#' @section The Basic Idea: 
#' 
#' The basic idea is as follows. \cr
#' Let `x` and `y` be 2 atomic vectors of the same length
#' (but they don't have to be of the same type). \cr
#' Let `v` be some atomic scalar of the same type as `y`. \cr
#' Given the result \code{r} of the condition `y == v`,
#' the basic idea is to perform the following sub-set operations: \cr
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' slicev_x(x, y = y, v = v)            # ==> x[y == v]
#' slicev_set(x, y = y, v = v, rp = rp) # ==> x[y == v] <- rp
#' slicev_set(x, y = y, v = v, tf = tf) # ==> x[y == v] <- tf(x[y == v]) 
#' countv(y,v = v)                      # ==> sum(y == v)
#' 
#' ```
#' 
#' The above is with the default argument specification \code{r = TRUE}. \cr
#' Of course one can invert the relationship by specifying argument \code{r = FALSE},
#' to get something like the following:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' slicev_x(x, y = y, v = v, r = FALSE)             # ==> x[y != v]                  
#' slicev_set(x, y = y, v = v, r = FALSE, rp = rp)  # ==> x[y != v] <- rp
#' slicev_set(x, y = y, v = v, r = FALSE, tf = tf)  # ==> x[y != v] <- tf(x[y != v])
#' countv(y, v = v, r = FALSE)                  # ==> sum(y != v)
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
#' \link{slicev} works exactly as explained in the previous section. \cr
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
#' When `length(v) == 2L`, `slicev_`/ `countv` will check whether `y` is inside
#' (or outside if \code{r = FALSE}) the bounded range given by `v`. \cr
#' I.e. :
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' y >= v[1] & y <= v[2]  # if r = TRUE
#' y < v[1] | y > v[2]    # if r = FALSE
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
#' y %in% v   # if r = TRUE
#' !y %in% v  # if r = FALSE
#' 
#' ```
#' 
#' 
#' 
#' @section Factors: 
#' 
#' Technically, a factor has the type of `integer`,
#' but it has special behaviour to the extend that it is treated differently in 'R'. \cr
#' It is similarly treated by the `slicev_`/ `countv_` methods and functions. \cr
#' \cr
#' When `y` is a factor, `v` can be given as:
#' 
#'  - a single string (matching one of the levels of `y`);
#'  - a single integer (matching one of the unique values of `unclass(y)`);
#'  - a factor of length 1, with the same levels and level-ordering as `y`. \cr
#'  
#' Note that factors with `NA` levels are not supported,
#' and passing such a factor to `y` will result in an error. \cr \cr
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
#' So these are not extracted (\link{slicev_x}),
#' replaced (\link{slicev_set}),
#' or counted (\link{countv}). \cr
#' \cr
#' When `na = TRUE`, `NA` values of `y` are always included. \cr
#' So these will be included in the extractions (\link{slicev_x}),
#' replacements (\link{slicev_set}),
#' and counts (\link{countv}). \cr
#' \cr
#' One can also specify `na = NA`, 
#' which will ignore `v` completely,
#' and explicitly look for `NA`s/`NaN`s in `y` instead - like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' slicev_x(x, y = y, na = NA)                        # ==> x[is.na(y)]                     
#' slicev_x(x, y = y, na = NA, r = FALSE)             # ==> x[!is.na(y)]
#' slicev_set(x, y = y, na = NA, rp = rp)             # ==> x[is.na(y)] <- rp                 
#' slicev_set(x, y = y, na = NA, r = FALSE, rp = rp)  # ==> x[!is.na(y)] <- rp               
#' slicev_set(x, y = y, na = NA, tf = tf)             # ==> x[is.na(y)] <- tf(x[is.na(y)])    
#' slicev_set(x, y = y, na = NA, r = FALSE, tf = tf)  # ==> x[!is.na(y)] <- tf(x[!is.na(y)]) 
#' countv(y, na = NA)                                 # ==> sum(is.na(y))                    
#' countv(y, na = NA, r = FALSE)                      # ==> sum(!is.na(y))                    
#' 
#' ```
#' 
#' Handling `NA`s works the same for all atomic types. \cr
#' For `y` of type `complex`,
#' a value `y[i]` is considered `NA`
#' if `Re(y[i])` is `NA`/`NaN` and/or `Im(y[i])` is `NA`/`NaN`. \cr
#' \cr
#' Argument `v` is never allowed to contain `NA`/`NaN`. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Inverting: 
#' `countv()` and `slicev_set()` do not have an "invert" argument,
#' and likewise there is no `slicev_wo()` function. \cr
#' One can only invert the sub-set condition, by specifying \code{r = FALSE}. \cr
#' \cr
#' \cr
#' 
#' @section Ellipsis: 
#' The ellipsis (`...`) is intentionally placed right after the first argument
#' (`x` in `slicev_` and `y` in `countv`)
#' to force the user to explicitly name all arguments,
#' as doing so will avoid a lot of unnecessary confusion. \cr \cr
#' 
#' 
#' 
#' @example inst/examples/slicev.R

#' @rdname aaa08_squarebrackets_slicev
#' @name aaa08_squarebrackets_slicev
#' @aliases squarebrackets_slicev
NULL
