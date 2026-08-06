# Access, Replace, Transform, Delete, or Extend Recursive Subsets

The `lst_rec()` and `lst_recin()` methods are essentially convenient
wrappers around `[[` and `[[<-`, respectively.  
  
`lst_rec()` will access recursive subsets of lists.  
  
`lst_recin()` can do the following things:  

- replace or transform recursive subsets of a list, using R's native
  semantics, by specifying the `rp` or `tf` argument, respectively.

- delete a recursive subset of a list, using R's native semantics, by
  specifying argument `rp = NULL`.

- extending a list with additional recursive elements, using R's native
  semantics.  
  This is done by specifying an out-of-bounds index in argument `rec`,
  and entering the new values in argument `rp`.  
  Note that adding surface level elements of a dimensional list will
  delete the dimension attributes of that list.  
    

## Usage

``` r
lst_rec(x, ...)

# Default S3 method
lst_rec(x, rec, ...)

lst_recin(x, ...)

# Default S3 method
lst_recin(x, rec, ..., rp, tf)
```

## Arguments

- x:

  a list, or list-like object.

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).

- rec:

  a strictly positive integer vector or character vector, of length `p`,
  such that `lst_rec(x, rec)` is equivalent to
  `x[[ rec[1] ]]...[[ rec[p] ]]`, providing all but the final indexing
  results in a list.  
  When on a certain subset level of a nested list, multiple subsets with
  the same name exist, only the first one will be selected when
  performing recursive indexing by name, since recursive indexing can
  only select a single element.  
  `NA, NaN, Inf, -Inf, NULL` are not valid values for `rec`.

- rp:

  optional, and allows for multiple functionalities:

  - In the simplest case, performs `x[[rec]] <- rp`, using R's native
    semantics.  
    Since this is a replacement of a recursive subset, `rp` does not
    necessarily have to be a list itself;  
    `rp` can be any type of object.

  - Specifying `rp = NULL` will **delete** (recursive) subset
    `lst_rec(x, rec)`.  
    To specify actual `NULL` instead of deleting a subset, use
    `rp = list(NULL)`.

  - When `rec` is an integer, and specifies an out-of-bounds subset,
    `lst_recin()` will add value `rp` to the list.  
    Any empty positions in between will be filled with `NA`.

  - When `rec` is character, and specifies a non-existing name,
    `lst_recin()` will add value `rp` to the list as a new element at
    the end.

- tf:

  an optional function. If specified, performs
  `x[[rec]] <- tf(x[[rec]])`, using R's native semantics.  
  Does not support extending a list like argument `rp`.

## Value

For `lst_rec()`:  
Returns the recursive subset.  
  
For `lst_recin(..., rp = rp)`:  
Returns nothing, but replaces, adds, or deletes the specified recursive
subset, using R's native semantics.  
  
For `lst_recin(..., tf = tf)`:  
Returns nothin, but transforms the specified recursive subset, using R's
native semantics.  
  

## Details

Since recursive objects are references to other objects, extending a
list or deleting an element of a list does not copy the entire list, in
contrast to atomic vectors.  
  
**Tip**:  
Dimensional sub-set operations on dimensional lists are much faster and
more flexible than Recursive sub-set operations on nested lists.  
So, whenever it makes sense, consider turning your nested list into a
dimensional list.  
One can turn a hierarchical list into a dimensional list using, for
example, the
[broadcast::cast_hier2dim](https://tony-aw.github.io/broadcast/man/cast_hier2dim.html)
method from the 'broadcast' 'R' package.  
  

## Examples

``` r

lst <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    A = list(A  = "AA2A", B = "AA2B"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  ),
  C = list(
    A = 1:10,
    B = 11:20
  )
)

#############################################################################

# access recursive subsets ====

lst_rec(lst, c(1,2,2)) # this gives "AA2B"
#> [1] "AA2B"
lst_rec(lst, c("A", "B", "B")) # this gives "ABB"
#> [1] "ABB"
lst_rec(lst, c(2,2,1)) # this gives "BBA"
#> [1] "BBA"
lst_rec(lst, c("B", "B", "A")) # this gives "BBA"
#> [1] "BBA"


#############################################################################

# replace recursive subset with R's default in-place semantics ====

# replace "AAB" using R's default in-place semantics:
lst_recin(
  lst, c("A", "A", "B"),
  rp = "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
)
print(lst)
#> $A
#> $A$A
#> $A$A$A
#> [1] "AAA"
#> 
#> $A$A$B
#> [1] "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
#> 
#> 
#> $A$A
#> $A$A$A
#> [1] "AA2A"
#> 
#> $A$A$B
#> [1] "AA2B"
#> 
#> 
#> $A$B
#> $A$B$A
#> [1] "ABA"
#> 
#> $A$B$B
#> [1] "ABB"
#> 
#> 
#> 
#> $B
#> $B$A
#> $B$A$A
#> [1] "BAA"
#> 
#> $B$A$B
#> [1] "BAB"
#> 
#> 
#> $B$B
#> $B$B$A
#> [1] "BBA"
#> 
#> $B$B$B
#> [1] "BBB"
#> 
#> 
#> 
#> $C
#> $C$A
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $C$B
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
#> 



#############################################################################

# transform recursive subsets with R's default in-place semantics ====

lst_recin(lst, c("C", "A"), tf = \(x)x^2) # transforms lst$C$A

print(lst)
#> $A
#> $A$A
#> $A$A$A
#> [1] "AAA"
#> 
#> $A$A$B
#> [1] "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
#> 
#> 
#> $A$A
#> $A$A$A
#> [1] "AA2A"
#> 
#> $A$A$B
#> [1] "AA2B"
#> 
#> 
#> $A$B
#> $A$B$A
#> [1] "ABA"
#> 
#> $A$B$B
#> [1] "ABB"
#> 
#> 
#> 
#> $B
#> $B$A
#> $B$A$A
#> [1] "BAA"
#> 
#> $B$A$B
#> [1] "BAB"
#> 
#> 
#> $B$B
#> $B$B$A
#> [1] "BBA"
#> 
#> $B$B$B
#> [1] "BBB"
#> 
#> 
#> 
#> $C
#> $C$A
#>  [1]   1   4   9  16  25  36  49  64  81 100
#> 
#> $C$B
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
#> 


#############################################################################

# add/remove new recursive subsets with R's default in-place semantics ====

lst_recin(lst, c("C", "D"), rp = "NEW VALUE") # adds lst$C$D
print(lst)
#> $A
#> $A$A
#> $A$A$A
#> [1] "AAA"
#> 
#> $A$A$B
#> [1] "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
#> 
#> 
#> $A$A
#> $A$A$A
#> [1] "AA2A"
#> 
#> $A$A$B
#> [1] "AA2B"
#> 
#> 
#> $A$B
#> $A$B$A
#> [1] "ABA"
#> 
#> $A$B$B
#> [1] "ABB"
#> 
#> 
#> 
#> $B
#> $B$A
#> $B$A$A
#> [1] "BAA"
#> 
#> $B$A$B
#> [1] "BAB"
#> 
#> 
#> $B$B
#> $B$B$A
#> [1] "BBA"
#> 
#> $B$B$B
#> [1] "BBB"
#> 
#> 
#> 
#> $C
#> $C$A
#>  [1]   1   4   9  16  25  36  49  64  81 100
#> 
#> $C$B
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
#> $C$D
#> [1] "NEW VALUE"
#> 
#> 

lst_recin(lst, c("C", "A"), rp = NULL) # removes lst$C$A
print(lst) # notice lst$C$A is GONE
#> $A
#> $A$A
#> $A$A$A
#> [1] "AAA"
#> 
#> $A$A$B
#> [1] "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
#> 
#> 
#> $A$A
#> $A$A$A
#> [1] "AA2A"
#> 
#> $A$A$B
#> [1] "AA2B"
#> 
#> 
#> $A$B
#> $A$B$A
#> [1] "ABA"
#> 
#> $A$B$B
#> [1] "ABB"
#> 
#> 
#> 
#> $B
#> $B$A
#> $B$A$A
#> [1] "BAA"
#> 
#> $B$A$B
#> [1] "BAB"
#> 
#> 
#> $B$B
#> $B$B$A
#> [1] "BBA"
#> 
#> $B$B$B
#> [1] "BBB"
#> 
#> 
#> 
#> $C
#> $C$B
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
#> $C$D
#> [1] "NEW VALUE"
#> 
#> 


#############################################################################

# Modify View of List By Reference ====

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
myref <- lst_rec(x, "a")
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
