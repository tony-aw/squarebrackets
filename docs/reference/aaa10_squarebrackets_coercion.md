# Auto-Coercion Rules for Mutable Objects

This help page describes the auto-coercion rules of the mutable classes,
as they are handled by the 'squarebrackets' package.  
This useful information for users who wish to intend to employ
[Pass-by-Reference
semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md)
as provided by 'squarebrackets'.  
  
  

## mutatomic

![\[coercion_through_copy:
YES\]](figures/coercion_through_copy-YES-darkgreen.svg)  
![\[coercion_by_reference:
NO\]](figures/coercion_by_reference-NO-red.svg)  
Mutable atomic objects are automatically coerced to fit the modified
subset values, when modifying through copy, just like regular atomic
classes.  
For example, replacing one or multiple values in an integer vector (type
`int`) with a decimal number (type `dbl`) will coerce the entire vector
to type `dbl`.  
  
Replacing or transforming subsets of mutable atomic objects **by
reference** does not support coercion. Thus, for example, the following
code,

    x <- mutatomic(1:16)
    ii_set(x, i = 1:6, rp = 8.5)
    #> coercing replacement to integer
    print(x)
    #>  [1]  8  8  8  8  8  8  7  8  9 10 11 12 13 14 15 16
    #> mutatomic
    #> typeof:  integer

gives `c(rep(8, 6) 7:16)` instead of `c(rep(8.5, 6), 7:16)`, because `x`
is of type `integer`, so `rp` is interpreted as type `integer` also.  
  
  

## data.table, when replacing/transforming whole columns

![\[coercion_through_copy:
YES\]](figures/coercion_through_copy-YES-darkgreen.svg)  
![\[coercion_by_reference:
YES\]](figures/coercion_by_reference-YES-darkgreen.svg)  
A data.table is actually a list made mutable, where each column is
itself a list. As such, replacing/transforming whole columns using
[`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html),
without specifying rows (not even `i = 1:nrow(x)`), allows completely
changing the type of the column.  
  
  

## data.table, when partially replacing/transforming columns

![\[coercion_through_copy:
YES\]](figures/coercion_through_copy-YES-darkgreen.svg)  
![\[coercion_by_reference:
NO\]](figures/coercion_by_reference-NO-red.svg)  
If rows are specified in the
[`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html)
function (and functions that internally use
[`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html)),
and thus not all values of columns but parts(i.e. rows) of columns are
replaced, no auto-coercion takes place.  
I.e.: replacing/transforming a value in an integer (`int`) column to
become `1.5`, will not coerce the column to the decimal type (`dbl`);
instead, the replacement value `1.5` is coerced to integer `1`.  
  
Using R's native copy-on-modify semantics (for example by changing a
`data.table` into a `data.frame`) allows for coercion even when
partially replacing/transforming columns.  
  
  

## Views of Lists

Regular lists are treated as immutable by 'squarebrackets'.  
But remember that a list is a (potentially hierarchical) structure of
references to other objects.  
Thus, even if a list itself is not treated as mutable, subsets of a list
which are themselves mutable classes, are mutable.  
For example, if you have a list of
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
objects, the mutatomic objects themselves are mutable.  
Therefore, the following will work:  

    x <- list(
     a = mutatomic(letters[1:10]),
     b = mutatomic(letters[11:20])
    )
    myref <- x$a
    ii_set(myref, 1, rp = "xxx")

Notice in the above code that `myref` is not a copy of `x$a`, since they
have the same address.  
Thus changing `myref` also changes `x$a`.  
In other words: `myref` is what could be called a "view" of `x$a`.  
Notice also that
[ii_set](https://tony-aw.github.io/squarebrackets/reference/generic_set.md)`(x$a, ...)`
will not work.  
This is because
[stopifnot_ma_safe2mutate](https://tony-aw.github.io/squarebrackets/reference/mutatomic_developer.md)
will give an error if `x` is not an **actual variable**, similar to
in-place functions in the style of `` `myfun()<-` ``.  
  
The auto-coercion rules of Views of Lists, depends entirely on the
object itself.  
Thus if the View is a data.table, coercion rules of data.tables apply.  
And if the View is a
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
object, coercion rules of
[mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
objects apply, etc.  
  

## Examples

``` r
# Coercion examples - mutatomic ====

x <- as.mutatomic(1:16)
ii_set(x, i = 1:6, rp = 8.5) # 8.5 coerced to 8, because `x` is of type `integer`
#> coercing replacement to integer
print(x)
#>  [1]  8  8  8  8  8  8  7  8  9 10 11 12 13 14 15 16
#> mutatomic 
#> typeof:  integer 

#############################################################################

# Coercion examples - data.table - whole columns ====

# tt_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 
tt_mod(
  obj, col = is.numeric,
  tf = sqrt # SAFE: obs = NULL, so coercion performed
)

# tt_set():
tt_set(
  obj, col = is.numeric,
  tf = sqrt # SAFE: obs = NULL, so coercion performed
)
str(obj)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  1 1.19 1.32 1.41 1.5 ...
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: num  1.82 1.86 1.9 1.93 1.97 ...
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 

#############################################################################


# Coercion examples - data.table - partial columns ====

# tt_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 

tt_mod(
  obj, with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # SAFE: coercion performed
)

# tt_set():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 
tt_set(
  obj, with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt
  # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
)
#> Warning: 1.414214 (type 'double') at RHS position 1 out-of-range(NA) or truncated (precision lost) when assigning to type 'integer' (column 1 named 'a')
#> Warning: 3.464102 (type 'double') at RHS position 1 out-of-range(NA) or truncated (precision lost) when assigning to type 'integer' (column 3 named 'c')
print(obj)
#>         a      b     c      d
#>     <int> <char> <int> <fctr>
#>  1:     1      a    11      a
#>  2:     1      b     3      b
#>  3:     1      c     3      c
#>  4:     2      d     3      d
#>  5:     2      e     3      e
#>  6:     2      f     4      f
#>  7:     2      g     4      g
#>  8:     8      h    18      h
#>  9:     9      i    19      i
#> 10:    10      j    20      j

obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 
tt_set(obj, col = is.numeric, tf = as.numeric) # first coerce type by whole columns
str(obj)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: num  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 
tt_set(
  obj,
  with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # SAFE: coercion performed by dt_setcoe(); so no warnings
) 
print(obj)
#>             a      b         c      d
#>         <num> <char>     <num> <fctr>
#>  1:  1.000000      a 11.000000      a
#>  2:  1.414214      b  3.464102      b
#>  3:  1.732051      c  3.605551      c
#>  4:  2.000000      d  3.741657      d
#>  5:  2.236068      e  3.872983      e
#>  6:  2.449490      f  4.000000      f
#>  7:  2.645751      g  4.123106      g
#>  8:  8.000000      h 18.000000      h
#>  9:  9.000000      i 19.000000      i
#> 10: 10.000000      j 20.000000      j


#############################################################################

# View of List ====

x <- list(
 a = data.table::data.table(cola = 1:10, colb = letters[1:10]),
 b = data.table::data.table(cola = 11:20, colb = letters[11:20])
)
print(x)
#> $a
#>      cola   colb
#>     <int> <char>
#>  1:     1      a
#>  2:     2      b
#>  3:     3      c
#>  4:     4      d
#>  5:     5      e
#>  6:     6      f
#>  7:     7      g
#>  8:     8      h
#>  9:     9      i
#> 10:    10      j
#> 
#> $b
#>      cola   colb
#>     <int> <char>
#>  1:    11      k
#>  2:    12      l
#>  3:    13      m
#>  4:    14      n
#>  5:    15      o
#>  6:    16      p
#>  7:    17      q
#>  8:    18      r
#>  9:    19      s
#> 10:    20      t
#> 
myref <- x$a
address(myref) == address(x$a) # they are the same
#> [1] TRUE
tt_set(myref, col = "cola", tf = \(x)x^2)
print(x) # notice x has been changed
#> $a
#>      cola   colb
#>     <num> <char>
#>  1:     1      a
#>  2:     4      b
#>  3:     9      c
#>  4:    16      d
#>  5:    25      e
#>  6:    36      f
#>  7:    49      g
#>  8:    64      h
#>  9:    81      i
#> 10:   100      j
#> 
#> $b
#>      cola   colb
#>     <int> <char>
#>  1:    11      k
#>  2:    12      l
#>  3:    13      m
#>  4:    14      n
#>  5:    15      o
#>  6:    16      p
#>  7:    17      q
#>  8:    18      r
#>  9:    19      s
#> 10:    20      t
#> 
```
