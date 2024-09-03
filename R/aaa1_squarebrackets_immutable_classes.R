#' Supported Immutable S3 Classes, With Auto-Coercion Rules
#'
#' @description
#' The `sb_` generic methods support the following immutable S3 classes: \cr
#' 
#'  * base `atomic` vector classes \cr
#'  (atomic vectors, matrices, and arrays).
#'  * classes derived from `atomic` vectors \cr
#'  (factors, date, POSIXct, etc.). \cr
#'  'squarebrackets' treats these classes as regular atomic vectors.
#'  * base list classes \cr
#'  (recursive vectors, matrices, and arrays) \cr
#'  (note that lists are merely pointers to other objects,
#'  and these other objects may be of a different class and may even be mutable).
#'  * \link[base]{data.frame} \cr
#'  (including the classes `tibble`, `sf-data.frame` and `sf-tibble`). \cr \cr
#'  
#' Note that "immutable" does not mean you cannot modify it. \cr
#' It simply means that modification leads to a copy being made. \cr \cr
#'
#' 
#' @section Auto-Coercion Rules:
#' 
#' \bold{Atomic} \cr
#' `r .mybadge_coercion("YES")` \cr
#' Atomic objects are automatically coerced to fit the modified subset values,
#' when modifying through copy. \cr
#' For example, replacing one or multiple values in an integer vector
#' (type `int`)
#' with a decimal number
#' (type `dbl`)
#' will coerce the entire vector to type `dbl`. \cr
#' \cr
#' \cr
#' \bold{Derived From Atomic} \cr
#' `r .mybadge_coercion("depends")` \cr
#' Factors, datetime, POSIXct and so on are derived from atomic vectors,
#' but have attributes and special methods that make them behave differently. \cr
#' Depending on their behaviour, they may or may not allow coercion. \cr
#' Factors, for example, only accept values that are part of their levels,
#' and thus do not support coercion on modification. \cr
#' \cr
#' There are highly specialized packages to handle objects derived from atomic objects. \cr
#' For example the 'forcats' package for handling factors, and the 'anytime' package
#' to handle ddate-time objects. \cr
#' \cr
#' \cr
#' \bold{List} \cr
#' `r .mybadge_coercion("depends")` \cr
#' Lists themselves allow complete change of their elements,
#' since lists are merely pointers. \cr
#' For example, the following code performs full coercion:
#' 
#' ```{r, eval = FALSE}
#' x <- list(factor(letters), factor(letters))
#' sb_mod(x, 1, rp = list(1))
#' ```
#' However, a recursive subset of a list which itself is not a list,
#' follows the coercion rules of whatever class the recursive subset is. \cr
#' For example the following code:
#' 
#' ```{r, eval = FALSE}
#' x <- list(1:10, 1:10)
#' sb_rec(x, 1, rp = "a") # coerces to character
#' ```
#' transforms recursive subsets according to the - in this case - 
#' atomic auto-coercion rules. \cr
#' \cr
#' \cr
#' \bold{Data.frames when replacing/transforming whole columns} \cr
#' `r .mybadge_coercion("YES")` \cr
#' A data.frame is actually a list, where each column is itself a list.
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
#' \bold{Data.frames, when partially replacing/transforming columns} \cr
#' `r .mybadge_coercion("NO")` \cr
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
#' applied before replacement (`rp`) or transformation (`tf()`). \cr \cr
#' 
#' 
#' 
#' @example inst/examples/aaa1_squarebrackets_immutable_classes.R
#' 
#' 

#' @rdname aaa1_squarebrackets_immutable_classes
#' @name aaa1_squarebrackets_immutable_classes
#' @aliases squarebrackets_immutable_classes
NULL
