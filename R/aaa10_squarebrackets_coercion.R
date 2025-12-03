#' Auto-Coercion Rules for Mutable Objects
#'
#' @description
#' This help page describes the auto-coercion rules of the mutable classes,
#' as they are handled by the 'squarebrackets' package. \cr
#' This useful information for users who wish to intend to employ
#' \link[=squarebrackets_PassByReference]{Pass-by-Reference semantics}
#' as provided by 'squarebrackets'. \cr
#' \cr
#' \cr
#'
#' 
#' @section mutatomic:
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("NO")` \cr
#' Mutable atomic objects are automatically coerced to fit the modified subset values,
#' when modifying through copy, just like regular atomic classes. \cr
#' For example, replacing one or multiple values in an integer vector
#' (type `int`)
#' with a decimal number
#' (type `dbl`)
#' will coerce the entire vector to type `dbl`. \cr
#' \cr
#' Replacing or transforming subsets of mutable atomic objects \bold{by reference}
#' does not support coercion.
#' Thus, for example, the following code,
#' 
#' ```{r}
#' 
#' x <- mutatomic(1:16)
#' ii_set(x, i = 1:6, rp = 8.5)
#' print(x)
#' 
#' ```
#' gives `c(rep(8, 6) 7:16)` instead of `c(rep(8.5, 6), 7:16)`,
#' because `x` is of type `integer`, so `rp` is interpreted as type `integer` also. \cr
#' \cr
#' \cr
#' @section data.table, when replacing/transforming whole columns:
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("YES")` \cr
#' A data.table is actually a list made mutable,
#' where each column is itself a list.
#' As such, replacing/transforming whole columns using `data.table::set()`,
#' without specifying rows (not even `i = 1:nrow(x)`),
#' allows completely changing the type of the column. \cr
#' \cr
#' \cr
#' 
#' @section data.table, when partially replacing/transforming columns:
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("NO")` \cr
#' If rows are specified in the `data.table::set()` function
#' (and functions that internally use `data.table::set()`),
#' and thus not all values of columns but parts(i.e. rows) of columns are replaced,
#' no auto-coercion takes place. \cr
#' I.e.: replacing/transforming a value in an integer (`int`) column to become `1.5`,
#' will not coerce the column to the decimal type (`dbl`);
#' instead, the replacement value `1.5` is coerced to integer `1`. \cr
#' \cr
#' Using R's native copy-on-modify semantics
#' (for example by changing a `data.table` into a `data.frame`)
#' allows for coercion even when partially replacing/transforming columns. \cr
#' \cr
#' \cr
#' 
#' 
#' @section Views of Lists:
#' Regular lists are treated as immutable by 'squarebrackets'. \cr
#' But remember that a list is a
#' (potentially hierarchical)
#' structure of references to other objects. \cr
#' Thus, even if a list itself is not treated as mutable,
#' subsets of a list which are themselves mutable classes, are mutable. \cr
#' For example,
#' if you have a list of \link{mutatomic} objects,
#' the mutatomic objects themselves are mutable. \cr
#' Therefore, the following will work: \cr
#' 
#' ```{r}
#' 
#' x <- list(
#'  a = mutatomic(letters[1:10]),
#'  b = mutatomic(letters[11:20])
#' )
#' myref <- x$a
#' ii_set(myref, 1, rp = "xxx")
#' 
#' ```
#' Notice in the above code that `myref` is not a copy of `x$a`,
#' since they have the same address. \cr
#' Thus changing `myref` also changes `x$a`. \cr
#' In other words: `myref` is what could be called a "view" of `x$a`. \cr
#' Notice also that \link{ii_set}`(x$a, ...)` will not work. \cr
#' This is because \link{stopifnot_ma_safe2mutate} will give an error
#' if `x` is not an \bold{actual variable},
#' similar to in-place functions in the style of \code{`myfun()<-`}. \cr
#' \cr
#' The auto-coercion rules of Views of Lists,
#' depends entirely on the object itself. \cr
#' Thus if the View is a data.table,
#' coercion rules of data.tables apply. \cr
#' And if the View is a \link{mutatomic} object,
#' coercion rules of \link{mutatomic} objects apply,
#' etc. \cr \cr
#' 
#' 
#' 
#' @example inst/examples/aaa_squarebrackets_coercion.R
#' 
#' 
#' @rdname aaa10_squarebrackets_coercion
#' @name aaa10_squarebrackets_coercion
#' @aliases squarebrackets_coercion
NULL
