#' Supported Mutable S3 classes, With Auto-Coercion Rules
#'
#' @description
#' The `sb_` generic methods support the following Mutable S3 classes: \cr
#' 
#'  * \link{mutable_atomic}
#'  (this vector class supports any dimension, thus also matrices and arrays);
#'  * \link[data.table]{data.table}
#'  (including the classes `tidytable` and `sf-data.table`);
#'  * \bold{Views of Lists}:
#'  Though lists themselves are treated as immutable,
#'  lists can contain mutable objects,
#'  and so modification by reference of mutable views of lists \bold{is} support by 'squarebrackets'. \cr \cr
#'  
#' The mutable version of the `list` class
#' would be the environment class,
#' and the various key-value storage classes available in other packages, such as the 'collapse' package. \cr
#' Key-value storage classes generally do not use square brackets for their primary sub-setting method,
#' and thus not covered by this package. \cr \cr
#' 
#' 
#' 
#' 
#' @section Auto-Coercion Rules:
#' \bold{Coercion Semantics} \cr
#' The mutable classes support "copy-on-modify" semantics like the immutable classes,
#' but - unlike the immutable classes - they also support "pass-by-reference" semantics. \cr
#' The \link{sb_mod} method
#' modify subsets of an object through a \bold{deep copy}. \cr
#' The \link{sb_set} method and \link{dt_setcoe} function
#' modify subsets of an object \bold{by reference}. \cr
#' These 2 copy semantics - "pass by reference" or "modify copy" - 
#' have slightly different auto-coercion rules. \cr
#' These are explained in this section. \cr
#' Note that the \link{sb_before} and \link{sb_after} methods
#' usually allow coercion for all classes.
#' \cr
#' \cr
#' \bold{mutable_atomic} \cr
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
#' ```{r eval = FALSE}
#' x <- 1:16
#' sb_set(x, i = 1:6, rp = 8.5)
#' x
#' ```
#' gives `c(rep(8, 6) 7:16)` instead of `c(rep(8.5, 6), 7:16)`,
#' because `x` is of type `integer`, so `rp` is interpreted as type `integer` also. \cr
#' \cr
#' \cr
#' \bold{data.table, when replacing/transforming whole columns} \cr
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("YES")` \cr
#' A data.table is actually a list made mutable,
#' where each column is itself a list.
#' As such, replacing/transforming whole columns,
#' so `row = NULL` and `filter = NULL`,
#' allows completely changing the type of the column. \cr
#' Note that coercion of columns needs arguments
#' `row = NULL` and `filter = NULL`
#' in the \link{sb_mod} and \link{sb_set} methods;
#' no auto-coercion will take place when specifying something like `row = 1:nrow(x)`
#' (see next section). \cr
#' \cr
#' \cr
#' \bold{data.table, when partially replacing/transforming columns} \cr
#' `r .mybadge_coercion_through_copy("NO")` \cr
#' `r .mybadge_coercion_by_ref("NO")` \cr
#' If rows are specified in the \link{sb_mod} and \link{sb_set} methods,
#' and thus not whole columns but parts of columns are replaced or transformed,
#' no auto-coercion takes place. \cr
#' I.e.: replacing/transforming a value in an integer (`int`) column to become `1.5`,
#' will not coerce the column to the decimal type (`dbl`);
#' instead, the replacement value `1.5` is coerced to integer `1`. \cr
#' The `coe` argument in the \link{sb_mod} method
#' allows the user to enforce coercion,
#' even if subsets of columns are replaced/transformed instead of whole columns. \cr
#' Specifically, the `coe` arguments allows the user to specify a coercive function
#' to be applied on the entirety of every column specified in `col` or `vars`;
#' columns outside this subset are not affected. \cr
#' This coercion function is, of course,
#' applied before replacement (`rp`) or transformation (`tf()`). \cr
#' \cr
#' \cr
#' \bold{Views of Lists} \cr
#' `r .mybadge_coercion_by_ref("depends")` \cr
#' Regular lists themselves are not treated as mutable objects by 'squarebrackets'. \cr
#' However, lists are not actually really objects, 
#' merely a (potentially hierarchical) structure of pointers. \cr
#' Thus, even if a list itself is not treated as mutable,
#' subsets of a list which are themselves mutable classes, are mutable. \cr
#' For example,
#' if you have a list of `data.table` objects,
#' the data.tables themselves are mutable. \cr
#' Therefore, the following will work: \cr
#' 
#' ```{r eval = FALSE}
#' x <- list(
#'  a = data.table(cola = 1:10, colb = letters[1:10]),
#'  b = data.table(cola = 11:20, colb = letters[11:20])
#' )
#' mypointer <- x$a
#' sb_set(mypointer, col = "cola", tf = \(x)x^2)
#' 
#' ```
#' Notice in the above code that `mypointer` is not a copy of `x$a`,
#' since they have the same address. \cr
#' Thus changing `mypointer` also changes `x$a`. \cr
#' In other words: `mypointer` is what could be called a "view" of `x$a`. \cr
#' Notice also that `sb_set(x$a, ...)` will not work,
#' since `sb_set()` requires \bold{actual variables},
#' similar to in-place functions in the style of \code{`myfun()<-`}. \cr
#' \cr
#' The auto-coercion rules of Views of Lists,
#' depends entirely on the object itself. \cr
#' Thus if the list subset is a data.table,
#'  mutable matrix, coercion rules of data.tables apply. \cr
#' And if the list subset is a data.table,
#' coercion rules of mutable matrices apply.,
#' etc. \cr \cr
#' 
#' 
#' @example inst/examples/aaa2_squarebrackets_mutable_classes.R
#' 
#' 
#' 

#' @rdname aaa2_squarebrackets_mutable_classes
#' @name aaa2_squarebrackets_mutable_classes
#' @aliases squarebrackets_mutable_classes
NULL
