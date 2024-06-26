#' Regarding Modification By Reference
#'
#'
#' @description
#' This help page describes how modification using "pass-by-reference" semantics
#' is handled by the 'squarebrackets' package. \cr
#' This help page does not explain all the basics of pass-by-reference semantics,
#' as this is treated as prior knowledge. \cr
#' All functions/methods in the 'squarebrackets' package
#' with the word "set" in the name
#' use pass-by-reference semantics. \cr \cr
#' 
#' 
#' @section Advantages and Disadvantages:
#' The main advantage of pass-by-reference is that much less memory is required to modify objects. \cr
#' But it does have several disadvantages. \cr
#' First, the coercion rules are slightly different: see \link{squarebrackets_mutable_classes}. \cr
#' \cr
#' Second, if 2 or more variables refer to exactly the same object,
#' changing one variable also changes the other ones. \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' x <- y <- mutable_atomic(1:16)
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' modifies not just `x`, but also `y`. \cr
#' This is true even if one of the variables is locked
#' (see \link[base]{bindingIsLocked}). \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' x <- mutable_atomic(1:16)
#' y <- x
#' lockBinding("y", environment())
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' modifies both `x` and `y` without error,
#' even though `y` is a locked constant. \cr
#' \cr
#' 
#'
#' @section Mutable vs Immutable types:
#' With the exception of environments,
#' most of base R's data types are treated as immutable: \cr
#' Modifying an object in 'R' will make a copy of the object,
#' something called 'copy-on-modify' semantics. \cr
#' However, almost any of base R's data types can be modified by reference,
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
#' @section Mutability Rules With Respect To Recursive Objects:
#' Lists are difficult objects in that they do not contain elements,
#' they simply point to  other objects,
#' that one can access via a list. \cr
#' When a recursive object is of a mutable class,
#' all its subsets are treated as mutable,
#' as long as they are part of the object. \cr
#' On the other hand,
#' When a recursive object is of an immutable class,
#' than its recursive subsets retain their original mutability. \cr
#' \cr
#' \bold{Example 1: Mutable data.tables} \cr
#' A `data.table` is a mutable class. \cr
#' So all columns of the `data.table` are treated as mutable; \cr
#' There is no requirement to, for instance,
#' first change all columns into the class of \link{mutable_atomic}
#' to modify these columns by reference. \cr
#' \cr
#' \bold{Example 2: Immutable lists} \cr
#' A regular `list` is an immutable class. \cr
#' So the list itself is immutable,
#' but the recursive subsets of the list retain their mutability. \cr
#' If you have a list of `data.table` objects, for example,
#' the data.tables themselves remain mutable. \cr
#' Therefore, the following pass-by-reference modification will work: \cr
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
#' Notice in the above code that `mypointer` has the same address as `x$a`,
#' and is therefore not a copy of `x$a`. \cr
#' Thus changing `mypointer` also changes `x$a`. \cr
#' In other words: `mypointer` is what could be called a "\bold{View}" of `x$a`. \cr \cr
#' 
#' 
#' @section Input Variable:
#' Methods/functions that perform in-place modification by reference
#' can be thought of as similar to functions in the style of `some_function(x, ...) <- value`,
#' in the sense that the variable must actually exist as an actual variable. \cr
#' Thus things like any of the following, \cr
#' `sb_set(1:10, ...)`, `sb2_set(x$a, ...)`, or `sb_set(base::letters)`, \cr
#' will not work. \cr \cr
#' 
#' 
#' @section Lock Binding:
#' \link[=squarebrackets_mutable_classes]{Mutable classes} are,
#' as the name suggests,
#' meant to be mutable. \cr
#' Locking the binding of a mutable object is \bold{mostly} fruitless
#' (but not completely;
#' see the \link{currentBindings} function). \cr
#' To prevent modification of an object's binding, 2 things must be true: \cr
#' 
#'  - the object must be an \link[=squarebrackets_immutable_classes]{immutable class}.
#'  - the binding must be \bold{locked} (see \link[base]{lockBinding}).
#'
#' Some packages that provide pass-by-reference semantics
#' tend to ignore the lock of an object's binding. \cr
#' Use the 'squarebrackets'  methods and (of course) core/base 'R' methods,
#' in case the user fears the binding locks will not be respected. \cr \cr
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
#' 
#' @section Protection:
#' 
#' Due to the properties described above in this help page,
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
#'  1) addresses in `baseenv()` are protected;
#'  2) immutable objects are disallowed
#'  (you'll have to create a mutable object,
#'  which will create a copy of the original,
#'  thus keeping the original object safe from modification by reference);
#'  3) locked bindings are disallowed.
#'  
#' Despite the checks made by this package,
#' the user should never actively try to modify
#' a \bold{locked} or \bold{protected} object by reference,
#' as that would defeat the purpose of locking an object. \cr
#' \cr
#' Some packages provide functions that change class-related attributes of objects by reference. \cr
#' Using such functions is discouraged,
#' unless you know exactly what you're doing. \cr \cr
#' 
#' 
#' 
#' 
#' @example inst/examples/aaa5_squarebrackets_PassByReference.R
#' 
#' 
#' @rdname aaa5_squarebrackets_PassByReference
#' @name aaa5_squarebrackets_PassByReference
#' @aliases squarebrackets_PassByReference
NULL
