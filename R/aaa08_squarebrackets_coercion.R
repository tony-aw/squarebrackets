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
#' @section mutable_atomic:
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
#' x <- mutable_atomic(1:16)
#' sb_set(x, i = 1:6, rp = 8.5)
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
#' As such, replacing/transforming whole columns,
#' so `row = NULL` and `obs = NULL`,
#' allows completely changing the type of the column. \cr
#' \cr
#' \cr
#' 
#' @section data.table, when partially replacing/transforming columns:
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("NO")` \cr
#' If rows are specified in the \link{sb2_set} method,
#' and thus not whole columns but parts of columns are replaced or transformed,
#' no auto-coercion takes place. \cr
#' I.e.: replacing/transforming a value in an integer (`int`) column to become `1.5`,
#' will not coerce the column to the decimal type (`dbl`);
#' instead, the replacement value `1.5` is coerced to integer `1`. \cr
#' \cr
#' The \link{sb2_mod} method, however,
#' allows for coercion just like regular data.frame objects. \cr
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
#' if you have a list of `data.table` objects,
#' the data.tables themselves are mutable. \cr
#' Therefore, the following will work: \cr
#' 
#' ```{r}
#' 
#' x <- list(
#'  a = data.table::data.table(cola = 1:10, colb = letters[1:10]),
#'  b = data.table::data.table(cola = 11:20, colb = letters[11:20])
#' )
#' myref <- x$a
#' sb2_set(myref, vars = "cola", tf = \(x)x^2)
#' 
#' ```
#' Notice in the above code that `myref` is not a copy of `x$a`,
#' since they have the same address. \cr
#' Thus changing `myref` also changes `x$a`. \cr
#' In other words: `myref` is what could be called a "view" of `x$a`. \cr
#' Notice also that \link{sb2_set}`(x$a, ...)` will not work,
#' since \link{sb_set}/\link{sb2_set} requires \bold{actual variables},
#' similar to in-place functions in the style of \code{`myfun()<-`}. \cr
#' \cr
#' The auto-coercion rules of Views of Lists,
#' depends entirely on the object itself. \cr
#' Thus if the View is a data.table,
#' coercion rules of data.tables apply. \cr
#' And if the View is a \link{mutable_atomic} matrix,
#' coercion rules of \link{mutable_atomic} matrices apply,
#' etc. \cr \cr
#' 
#' 
#' 
#' @example inst/examples/aaa_squarebrackets_coercion.R
#' 
#' 

#' @rdname aaa08_squarebrackets_coercion
#' @name aaa08_squarebrackets_coercion
#' @aliases squarebrackets_coercion
NULL
