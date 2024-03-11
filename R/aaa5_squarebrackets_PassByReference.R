#' Regarding Modification By Reference
#'
#'
#' @description
#' This help page describes how modification using "pass-by-reference" semantics
#' is handled by the 'squarebrackets' package. \cr
#' \cr
#' The main advantage of pass-by-reference is that much less memory is required to modify objects. \cr
#' But at least 2 things should be taken into consideration
#' when modifying an object by reference. \cr
#' First, the coercion rules are slightly different: see \link{squarebrackets_mutable_classes}. \cr
#' \cr
#' Second, if 2 or more variables refer to exactly the same object,
#' changing one variable also changes the other ones. \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' x <- y <- 1:16
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' modifies not just `x`, but also `y`. \cr
#' This is true even if one of the variables is locked
#' (see \link[base]{bindingIsLocked}). \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' tinycodet::import_LL("tinycodet", "%<-c%")
#' x <- 1:16
#' y %<-c% x
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' modifies both `x` and `y` without error,
#' even though `y` is a locked constant. \cr \cr
#' 
#'
#' @section Mutable vs Immutable types:
#' With the exception of environments,
#' most of base R's data types are treated as immutable: \cr
#' Modifying an object in 'R' will make a copy of the object,
#' something called 'copy-on-modify' semantics. \cr
#' However, almost any of base R's datatypes can be modified by reference,
#' through R's own 'C' API, or through 'C++' code (like via 'Rcpp'),
#' thus treating these objects as mutable,
#' even though they are not "supposed" to be mutable. \cr
#' Modifying a base 'R' object by reference can be problematic. \cr
#' Since 'R', and also most R-packages, treat these objects as immutable,
#' modifying them as-if they are mutable may produce undesired results. \cr
#' \cr
#' To prevent the issue described above,
#' 'squarebrackets' only supports pass-by-reference semantics
#' on objects that are actually supposed to be mutable. \cr
#' In relation to this restriction,
#' 'squarebrackets' adds a new class of objects,
#' \link[=class_mutable_atomic]{mutable_atomic},
#' which are simply atomic objects
#' that have the permission to be modified by reference. \cr \cr
#' 
#' 
#' @section Lock Binding:
#' The \link[base]{lockBinding} function locks a binding of an object,
#' preventing modification. \cr
#' 'R' also uses locked bindings to prevent modification of objects from package namespaces. \cr
#' The pass-by-reference semantics of 'squarebrackets' in principle respect this,
#' and disallows modification of objects by reference. \cr
#' \cr
#' However, \link[base]{lockBinding} does not lock the address/pointer of an object,
#' only one particular binding of an object. \cr
#' This problematic; consider the following example: \cr
#' 
#' ```{r eval = FALSE}
#' tinycodet::import_LL("tinycodet", "%<-c%")
#' x <- 1:16
#' y %<-c% x
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' 
#' In the above code, `x` and `y` share the same address,
#' thus pointing to the same memory,
#' yet only `y` is actually locked. \cr
#' Since `x` is not locked, modifying `x` is allowed. \cr
#' But since `sb_set()` performs modification by reference,
#' `y` will STILL be modified, despite being locked. \cr
#' \cr
#' To remedy the issue as explained above,
#' 'squarebrackets' provides the \link{sb_currentBindings} method. \cr
#' This method can do multiple things.
#' One of which is to look at the address of some object `x`,
#' find all \bold{currently existing} bindings in
#' the \bold{caller environment}
#' sharing the same address as `x`,
#' and locking all these bindings. \cr
#' \cr
#' Warning: \cr
#' The \link{sb_currentBindings} method
#' only locks \bold{currently existing} bindings
#' in the \bold{caller environment}; \cr
#' bindings that are created \bold{after} calling \link{sb_currentBindings}
#' will not automatically be locked. \cr
#' Thus, every time the user creates a new binding of the same object,
#' and the user wishes it to be locked,
#' \link{sb_currentBindings} must be called again. \cr \cr
#' 
#' 
#' @section Protected Addresses:
#' To prevent an accidental pass-by-reference modification of objects in the base environment,
#' all addresses of all exported objects in the base environment
#' (\link[base]{baseenv})
#' are stored in
#' the option `squarebrackets.protected` whenever 'squarebrackets' is \bold{loaded},
#' either directly or indirectly. \cr
#' Needless to say, the user should never touch this option. \cr \cr
#' 
#' 
#' @section Input Variable:
#' Methods/functions that perform in-place modification by reference
#' can be thought of as similar to functions in the style of `some_function(x, ...) <- value`,
#' in the sense that the variable \bold{must actually exist as an actual variable}. \cr
#' Thus things like any of the following, \cr
#' `sb_set(1:10, ...)`, `sb_set(x$a, ...)`, or `sb_set(base::letters)`, \cr
#' \bold{will not work}. \cr \cr
#' 
#' 
#' @section Views of Lists:
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
#' In other words: `mypointer` is what could be called a "view" of `x$a`. \cr \cr
#' 
#' 
#' @section Protection:
#' 
#' Due to the properties given in this help page so far,
#' something like the following will not work:
#' 
#' ```{r eval = FALSE}
#' 
#' # letters = base::letters
#' sb_set(letters, i = 1, rp = "XXX")
#' 
#' ```
#' 
#' The above won't work because: 
#' 
#'  1) base objects are disallowed;
#'  2) immutable objects are disallowed
#'  (you'll have to create a mutable object,
#'  which will create a copy of the original,
#'  thus keeping the original object safe from modification by reference);
#'  3) locked bindings are disallowed.
#'  
#' Despite the checks made by this package,
#' the user should never actively try to modify
#' a \bold{locked} or \bold{protected} object by reference,
#' as that would defeat the purpose of locking an object. \cr \cr
#' 
#' 
#' @example inst/examples/aaa5_squarebrackets_PassByReference.R
#' 
#' 
#' @rdname aaa5_squarebrackets_PassByReference
#' @name aaa5_squarebrackets_PassByReference
#' @aliases squarebrackets_PassByReference
NULL
