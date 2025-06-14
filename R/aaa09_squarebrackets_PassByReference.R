#' Regarding Modification By Reference
#'
#'
#' @description
#' This help page describes how modification using "pass-by-reference" semantics
#' is handled by the 'squarebrackets' package. \cr
#' \cr
#' "Pass-by-reference" refers to modifying a mutable object,
#' or a subset of a mutable object,
#' without making any copies at all. \cr
#' \cr
#' This help page does not explain all the basics of pass-by-reference semantics,
#' as this is treated as prior knowledge. \cr
#' All functions/methods in the 'squarebrackets' package
#' with the word "set" in the name
#' use pass-by-reference semantics. \cr \cr
#' 
#' 
#' 
#' @section Advantages and Disadvantages:
#' The main advantage of pass-by-reference is that much less memory is required to modify objects,
#' and modification is also generally faster. \cr
#' But it does have several disadvantages. \cr
#' \cr
#' First, the coercion rules are slightly different: see \link{squarebrackets_coercion}. \cr
#' \cr
#' Second, if 2 or more variables refer to exactly the same object
#' (i.e. have the same address),
#' changing one variable also changes the other ones. \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' x <- y <- mutatomic(1:16)
#' i_set(x, 1:6, rp = 8)
#' ```
#' modifies not just `x`, but also `y`. \cr
#' This is true even if one of the variables is locked
#' (see \link[base]{bindingIsLocked}). \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' x <- mutatomic(1:16)
#' y <- x
#' lockBinding("y", environment())
#' i_set(x, i = 1:6, rp = 8)
#' ```
#' modifies both `x` and `y` without error,
#' even though `y` is a locked constant. \cr
#' \cr
#' 
#'
#' @section Mutable vs Immutable Classes:
#' With the exception of environments,
#' most of base R's S3 classes are treated as immutable: \cr
#' Modifying an object in 'R' will make a copy of the object,
#' something called 'copy-on-modify' semantics. \cr
#' \cr
#' A prominent mutable S3 class is the `data.table` class,
#' which is a mutable data.frame class, and supported by 'squarebrackets'. \cr
#' Similarly, 'squarebrackets' adds a class for mutable atomic objects: \cr
#' \link{mutatomic}. \cr \cr
#' 
#' 
#' @section Material vs Immaterial objects:
#' Most objects in 'R' are material objects: \cr
#' the values an object contains are actually stored in memory. \cr
#' For example, given `x <- rnorm(1e6)`, `x` is a material object: \cr
#' 1 million values (decimal numbers, in this case) are actually stored in memory. \cr
#' \cr
#' In contrast, \link[=makeActiveBinding]{ActiveBindings} are immaterial: \cr
#' They are objects that,
#' when accessed,
#' call a function to generate values on the fly,
#' rather than actually storing values. \cr
#' \cr
#' Since immaterial objects do not actually store the values in memory,
#' the values obviously also cannot be changed in memory. \cr
#' Therefore, Pass-by-Reference semantics don't work on immaterial objects. \cr
#' \cr
#' 
#' @section ALTREP:
#' The \link{mutatomic} constructors
#' (i.e. \link{mutatomic}, \link{as.mutatomic}, etc.)
#' will automatically materialize ALTREP objects,
#' to ensure consistent behaviour for 'pass-by-reference' semantics. \cr
#' \cr
#' A `data.table` can have ALTREP columns. \cr
#' A `data.tables` will coerce the column to a materialized column when it is modified, even by reference. \cr
#' \cr
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
#' its recursive subsets retain their original mutability. \cr
#' \cr
#' \bold{Example 1: Mutable data.tables} \cr
#' A `data.table` is a mutable class. \cr
#' So all columns of the `data.table` are treated as mutable; \cr
#' There is no requirement to, for instance,
#' first change all columns into the class of \link{mutatomic}
#' to modify these columns by reference. \cr
#' \cr
#' \bold{Example 2: Immutable lists} \cr
#' A regular `list` is an immutable class. \cr
#' So the list itself is immutable,
#' but the recursive subsets of the list retain their mutability. \cr
#' If you have a list of `mutatomic` objects, for example,
#' the mutatomic objects themselves remain mutable. \cr
#' Therefore, the following pass-by-reference modification will work without issue: \cr
#' 
#' ```{r}
#' 
#' x <- list(
#'  a = mutatomic(letters[1:10]),
#'  b = mutatomic(letters[11:20])
#' )
#' myref <- x$a
#' i_set(myref, 1, rp = "xxx")
#' 
#' ```
#' Notice in the above code that `myref` has the same address as `x$a`,
#' and is therefore not a copy of `x$a`. \cr
#' Thus changing `myref` also changes `x$a`. \cr
#' In other words: `myref` is what could be called a "\bold{View}" of `x$a`. \cr \cr
#' 
#' 
#' @section Input Variable:
#' Methods/functions that perform in-place modification by reference
#' only works on objects that actually exist as an actual variable,
#' similar to functions in the style of `some_function(x, ...) <- value`. \cr
#' Thus things like any of the following, \cr
#' `i_set(1:10, ...)`, `i_set(x$a, ...)`, or `i_set(base::letters)`, \cr
#' will not and should not work. \cr \cr
#' 
#' 
#' @section Lock Binding:
#' Mutable classes are,
#' as the name suggests,
#' meant to be mutable. \cr
#' Locking the binding of a mutable object is fruitless. \cr
#' To ensure an object cannot be modified by any of the methods/functions from 'squarebrackets',
#' 2 things must be true: \cr
#' 
#'  - the object must be an immutable class.
#'  - the binding must be \bold{locked} (see \link[base]{lockBinding}). \cr \cr
#' 
#' 
#' 
#' @section Protection:
#' 
#' Due to the properties described above in this help page,
#' 'squarebrackets' protects the user from do something like the following:
#' 
#' ```{r eval = FALSE}
#' 
#' # letters = base::letters
#' i_set(letters, i = 1, rp = "XXX")
#' 
#' ```
#' 
#' 'squarebrackets' will give an error when running the code above, because:
#' 
#'  1) most addresses in `baseenv()` are protected;
#'  2) immutable objects are disallowed
#'  (you'll have to create a mutable object,
#'  which will create a copy of the original,
#'  thus keeping the original object safe from modification by reference);
#'  3) locked bindings are disallowed.
#'  
#' 
#' 
#' 
#' 
#' @example inst/examples/aaa_squarebrackets_PassByReference.R
#' 
#' 
#' @rdname aaa09_squarebrackets_PassByReference
#' @name aaa09_squarebrackets_PassByReference
#' @aliases squarebrackets_PassByReference
NULL
