#' Supported Immutable S3 Classes, With Auto-Coercion Rules
#'
#' @description
#' The `sb_` generic methods support the following categories of S3 classes: \cr
#' 
#'  * `atomic` classes (\link[base]{vector}, \link[base]{matrix}, \link[base]{array});
#'  * \link[base]{factor};
#'  * \link[base]{list};
#'  * \link[base]{data.frame}
#'  (classes that inherit from `data.frame`,
#'  like `tibble`, `sf-data.frame` and `sf-tibble`) \cr \cr
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
#' \bold{Factor} \cr
#' `r .mybadge_coercion("NO")` \cr
#' Factors only accept values that are part of their levels,
#' and thus do not support coercion on modification.
#' There is no mechanism for changing factors by reference at all. \cr
#' Replacing a value with a new value not part of its levels,
#' will result in the replacement value being `NA`. \cr
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
#' NO auto-coercion will take place when specifying something like `row = 1:nrow(x)`
#' (see next section). \cr
#' \cr
#' \cr
#' \bold{Data.frames, when partially replacing/transforming columns} \cr
#' `r .mybadge_coercion("NO")` \cr
#' If rows are specified in the \link{sb_mod} and \link{sb_set} methods,
#' and thus not whole columns but parts of columns are replaced or transformed,
#' NO auto-coercion takes place. \cr
#' I.e.: replacing/transforming a value in an integer (`int`) column to become `1.5`,
#' will NOT coerce the column to the decimal type (`dbl`);
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
#' @examples
#' 
#' 
#' # Coercion examples - lists ====
#' x <- list(factor(letters), factor(letters))
#' print(x)
#' sb_mod(x, 1, rp = list(1)) # first element fully changed.
#' 
#' x <- list(1:10, 1:10)
#' print(x)
#' sb_rec(x, 1, rp = "a") # coerces first element to character
#' print(x)
#' 
#' #############################################################################
#' 
#' 
#' # Coercion examples - data.frame-like - whole columns ====
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_mod(
#'   obj, vars = is.numeric,
#'   tf = sqrt # SAFE: row=NULL & filter = NULL, so coercion performed
#' )
#' 
#' #############################################################################
#' 
#' 
#' # Coercion examples - data.frame-like - partial columns ====
#' 
#' # sb_mod():
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' 
#' sb_mod(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' ) 
#' sb_mod(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   coe = as.double, tf = sqrt # SAFE: coercion performed
#' )
#' 
#' 
#' 

#' @rdname aaa1_squarebrackets_immutable_classes
#' @name aaa1_squarebrackets_immutable_classes
#' @aliases squarebrackets_immutable_classes
NULL
