# Regarding Modification By Reference

This help page describes how modification using "pass-by-reference"
semantics is handled by the 'squarebrackets' package.  
  
"Pass-by-reference" refers to modifying a mutable object, or a subset of
a mutable object, without making any copies at all.  
  
This help page does not explain all the basics of pass-by-reference
semantics, as this is treated as prior knowledge.  
All functions/methods in the 'squarebrackets' package with the word
"set" in the name use pass-by-reference semantics.  
  

## Advantages and Disadvantages

The main advantage of pass-by-reference is that much less memory is
required to modify objects, and modification is also generally faster.  
But it does have several disadvantages.  
  
First, the coercion rules are slightly different: see
[squarebrackets_coercion](https://tony-aw.github.io/squarebrackets/reference/aaa10_squarebrackets_coercion.md).  
  
Second, if 2 or more variables refer to exactly the same object (i.e.
have the same address), changing one variable also changes the other
ones.  
I.e. the following code,

    x <- y <- mutatomic(1:16)
    ii_set(x, 1:6, rp = 8)

modifies not just `x`, but also `y`.  
This is true even if one of the variables is locked (see
[bindingIsLocked](https://rdrr.io/r/base/bindenv.html)).  
I.e. the following code,

    x <- mutatomic(1:16)
    y <- x
    lockBinding("y", environment())
    ii_set(x, i = 1:6, rp = 8)

modifies both `x` and `y` without error, even though `y` is a locked
constant.  
  

## Mutable vs Immutable Classes

With the exception of environments, most of base R's S3 classes are
treated as immutable:  
Modifying an object in 'R' will make a copy of the object, something
called 'copy-on-modify' semantics.  
  
A prominent mutable S3 class is the `data.table` class, which is a
mutable data.frame class, and supported by 'squarebrackets'.  
Similarly, 'squarebrackets' adds a class for mutable atomic objects:  
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md).  
  

## Material vs Immaterial objects

Most objects in 'R' are material objects:  
the values an object contains are actually stored in memory.  
For example, given `x <- rnorm(1e6)`, `x` is a material object:  
1 million values (decimal numbers, in this case) are actually stored in
memory.  
  
In contrast, [ActiveBindings](https://rdrr.io/r/base/bindenv.html) are
immaterial:  
They are objects that, when accessed, call a function to generate values
on the fly, rather than actually storing values.  
  
Since immaterial objects do not actually store the values in memory, the
values obviously also cannot be changed in memory.  
Therefore, Pass-by-Reference semantics don't work on immaterial
objects.  
  

## ALTREP

The
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
constructors (i.e.
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md),
[as.mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md),
etc.) will automatically materialize ALTREP objects, to ensure
consistent behaviour for 'pass-by-reference' semantics.  
  
A `data.table` can have ALTREP columns.  
A `data.tables` will coerce the column to a materialized column when it
is modified, even by reference.  
  

## Input Variable

Methods/functions that perform in-place modification by reference only
works on objects that actually exist as an actual variable, similar to
functions in the style of `some_function(x, ...) <- value`.  
Thus things like any of the following,  
`ii_set(1:10, ...)`, `ii_set(x$a, ...)`, or `ii_set(base::letters)`,  
will not and should not work.  
  

## Lock Binding

Mutable classes are, as the name suggests, meant to be mutable.  
Locking the binding of a mutable object is fruitless.  
To ensure an object cannot be modified by any of the methods/functions
from 'squarebrackets', 2 things must be true:  

- the object must be an immutable class.

- the binding must be **locked** (see
  [lockBinding](https://rdrr.io/r/base/bindenv.html)).  
    

## Protection

Due to the properties described above in this help page,
'squarebrackets' protects the user from do something like the following:

    # letters = base::letters
    ii_set(letters, i = 1, rp = "XXX")

'squarebrackets' will give an error when running the code above,
because:

1.  most addresses in
    [`baseenv()`](https://rdrr.io/r/base/environment.html) are
    protected;

2.  immutable objects are disallowed (you'll have to create a mutable
    object, which will create a copy of the original, thus keeping the
    original object safe from modification by reference);

3.  locked bindings are disallowed.

## Examples

``` r
# the following code demonstrates how locked bindings,
# such as `base::letters`,
# are being safe-guarded

x <- list(a = base::letters)
myref <- x$a # view of a list
address(myref) == address(base::letters) # TRUE: point to the same memory
#> [1] TRUE
bindingIsLocked("letters", baseenv()) # base::letters is locked ...
#> [1] TRUE
bindingIsLocked("myref", environment()) # ... but this pointer is not!
#> [1] FALSE

if(requireNamespace("tinytest")) {
  tinytest::expect_error(
    ii_set(myref, i = 1, rp = "XXX") # this still gives an error though ...
  )
}
#> Loading required namespace: tinytest
#> ----- PASSED      : <-->
#>  call| eval(expr, envir) 

is.mutatomic(myref) # ... because it's not of class `mutatomic`
#> [1] FALSE


x <- list(
  a = as.mutatomic(base::letters) # `as.mutatomic()` makes a copy
)
myref <- x$a # view of a list
address(myref) == address(base::letters) # FALSE: it's a copy
#> [1] FALSE
ii_set(
  myref, i = 1, rp = "XXX"  # modifies x, does NOT modify `base::letters`
)
print(x) # x is modified
#> $a
#>  [1] "XXX" "b"   "c"   "d"   "e"   "f"   "g"   "h"   "i"   "j"   "k"   "l"  
#> [13] "m"   "n"   "o"   "p"   "q"   "r"   "s"   "t"   "u"   "v"   "w"   "x"  
#> [25] "y"   "z"  
#> mutatomic 
#> typeof:  character 
#> 
base::letters # but this still the same
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"

```
