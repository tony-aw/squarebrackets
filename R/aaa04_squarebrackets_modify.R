#' Regarding Modification
#'
#'
#' @description
#' This help page describes the main modification semantics
#' available in 'squarebrackets'. \cr \cr
#' 
#' 
#' @section Base R's default modification: 
#' For most average users, R's default copy-on-modify semantics are fine. \cr
#' The benefits of the indexing arguments from 'squarebrackets'
#' can be combined the `[<-` operator,
#' through the \link{idx} method. \cr
#' The result of the `idx()` method
#' can be used inside the regular square-brackets operators. \cr
#' For example like so:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' x <- array(...)
#' my_indices <- idx(x, s, d)
#' x[my_indices] <- value
#' 
#' y <- data.frame(...)
#' rows <- idx(y, 1:10, 1, inv = TRUE)
#' cols <- idx(y, c("a", "b"), 2)
#' y[rows, cols] <- value
#' ```
#' 
#' thus allowing the user to benefit from the convenient index translations from 'squarebrackets',
#' whilst still using R's default copy-on-modification semantics
#' (instead of the semantics provided by 'squarebrackets'). \cr
#' \cr
#' \cr
#' @section Explicit Copy:
#' 'squarebrackets' provides
#' the \link{sb_mod}/\link{sb2_mod} method
#' to modify through a (shallow) copy. \cr
#' This method returns the modified object. \cr
#' For recursive objects, \link{sb2_mod} returns the original object,
#' where only the modified subsets are copied,
#' thus preventing unnecessary usage of memory. \cr
#' \cr
#' \cr
#' @section Pass-by-Reference:
#' 'squarebrackets' provides
#' the \link{sb_set}/\link{sb2_set} and \link{slice_set} methods
#' to modify by reference,
#' meaning no copy is made at all. \cr
#' Pass-by-Reference is fastest and the most memory efficient. \cr
#' But it is also more involved than the other modification forms,
#' and requires more thought. \cr
#' See \link{squarebrackets_PassByReference} for more information. \cr
#' \cr
#' \cr
#' 
#' @section Replacement and Transformation in Atomic Objects:
#' The `rp` argument is used to replace the values at the specified indices
#' with the values specified in `rp`.
#' Using the `rp` argument in the modification methods,
#' corresponds to something like the following: \cr
#' 
#' ```{r, echo = TRUE, eval = FALSE}
#' 
#' x[...] <- rp
#' 
#' ```
#' 
#' The `tf` argument is used to transform the values at the specified indices
#' through transformation function `tf`.
#' Using the `tf` argument
#' corresponds to something like the following: \cr
#' 
#' ```{r, echo = TRUE, eval = FALSE}
#' 
#' x[...] <- tf(x[...])
#' 
#' ```
#' where `tf` is a function that \bold{returns} an object of appropriate type and size
#' (so `tf` should not be a pass-by-reference function). \cr
#' \cr
#' 
#' 
#' @section Replacement and Transformation in Lists:
#' The `rp` and `tf` arguments work mostly in the same way for recursive objects. \cr
#' But there are some slight differences. \cr
#' \cr
#' \bold{Argument `rp`} \cr
#' 'squarebrackets' demands that `rp` is always provided as a list
#' in the S3 methods for recursive vectors, matrices, and arrays (i.e. lists). \cr
#' This is to prevent ambiguity
#' with respect to how the replacement is recycled or distributed over the specified indices \cr
#' (See `Footnote 1` below). \cr
#' \cr
#' \bold{Argument `tf`} \cr
#' Most functions in (base) 'R' are vectorized for atomic objects, but not for lists \cr
#' (see `Footnote 2` below). \cr
#' 'squarebrackets' will therefore apply transformation function `tf` via `lapply`,
#' like so: \cr
#' 
#' ```{r, echo = TRUE, eval = FALSE}
#' 
#' x[...] <- lapply(x[...], tf)
#' 
#' ```
#' 
#' In the methods for recursive objects,
#' the `tf` argument is accompanied by the `.lapply` argument. \cr
#' By default, `.lapply = lapply`. \cr
#' The user may supply a custom `lapply()`-like function
#' in this argument to use instead. \cr
#' For example, to perform parallel transformation,
#' the user may supply `future.apply::`\link[future.apply]{future_lapply}. \cr
#' The supplied function must use the exact same argument convention as
#' \link[base]{lapply},
#' otherwise errors or unexpected behaviour may occur. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Replacement and Transformation in data.frame-like Objects:
#' Replacement and transformations
#' in data.frame-like objects are a bit more flexible than in Lists. \cr
#' \cr
#' `rp` is not always demanded to be a list for data.frame-like objects,
#' only when appropriate
#' (for example, when replacing multiple columns, or when the column itself is a list.) \cr
#' When `rp` \bold{is} given as a list,
#' it is unclassed and unnamed before being used to replace values. \cr
#' This is to ensure consistency across all supported data.frame types. \cr
#' \cr
#' Bear in mind that every column in a data.frame is like an element in a list; \cr
#' so `.lapply` is used for transformations across multiple columns. \cr
#' \cr
#' \cr
#' 
#' @section Recycling and Coercion:
#' Recycling is not allowed in the modification methods. \cr
#' So, for example, `length(rp)` must be equal to the length of the selected subset,
#' or equal to `1`. \cr
#' \cr
#' When using \link[=squarebrackets_PassByReference]{Pass-by-Reference semantics},
#' the user should be extra mindful of the auto-coercion rules. \cr
#' See \link{squarebrackets_coercion} for details. \cr
#' \cr
#' 
#' 
#' @section Footnotes:
#' \bold{Footnote 1} \cr
#' Consider the following replacement in base 'R':
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' x <-list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' x[1:2] <- 2:1
#' 
#' ```
#' 
#' What will happen? \cr
#' Will the `x[1]` be `list(1:2)` and `x[2]` also be `list(1:2)`? \cr
#' Or will `x[1]` be `list(2)` and `x[2]` be `list(1)`? \cr
#' It turns out the latter will happen; but this is somewhat ambiguous from the code. \cr
#' To prevent such ambiguity in your code,
#' 'squarebrackets' demands that `rp` is always provided as a list. \cr
#' \cr
#' \bold{Footnote 2} \cr
#' Most functions in (base) 'R' are vectorized for atomic objects, but not for lists. \cr
#' One of the reasons is the following: \cr
#' In an atomic vector `x` of some type `t`,
#' every single element of `x` is a scalar of type `t`. \cr
#' However, every element of some list `x` can be virtually anything: \cr
#' an atomic object, another list,
#' an unevaluated expression, even dark magic like `quote(expr =)`. \cr
#' It is difficult to make a vectorized function for an object with so many unknowns. \cr 
#' Therefore, in the vast majority of the cases,
#' one needs to loop through the list elements. \cr
#' \cr
#' 
#' 
#' @rdname aaa04_squarebrackets_modify
#' @name aaa04_squarebrackets_modify
#' @aliases squarebrackets_modify
NULL
