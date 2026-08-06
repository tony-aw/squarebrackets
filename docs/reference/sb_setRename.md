# Safely Change the Names of a Mutable Object By Reference

Functions to rename a [supported mutable
object](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md)
using [pass-by-reference
semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md):

- `sb_setFlatnames()` renames the (flat) names of a `mutatomic` object.

- `sb_setDimnames()` renames the dimension names of a `mutatomic`
  object.

- `sb_setVarnames()` renames the variable names of a `data.table`
  object.  
    

## Usage

``` r
sb_setFlatnames(x, i = NULL, newnames, ...)

sb_setDimnames(x, m, newdimnames, ...)

sb_setVarnames(x, old, new, skip_absent = FALSE, ...)
```

## Arguments

- x:

  a **variable** belonging to one of the [supported mutable
  classes](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md).  

- i:

  logical, numeric, character, or imaginary indices, indicating which
  flatnames should be changed.  
  If `i = NULL`, the names will be completely replaced.

- newnames:

  Atomic character vector giving the new names.  
  Specifying `NULL` will remove the names.  

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).  
    

- m:

  integer vector giving the margin(s) for which to change the names
  (`m = 1L` for rows, `m = 2L` for columns, etc.).

- newdimnames:

  a list of the same length as `m`.  
  The first element of the list corresponds to margin `m[1]`, the second
  element to `m[2]`, and so on.  
  The components of the list can be either `NULL`, or a character vector
  with the same length as the corresponding dimension.  
  Instead of a list, simply `NULL` can be specified, which will remove
  the `dimnames` completely.

- old:

  the old column names

- new:

  the new column names, in the same order as `old`

- skip_absent:

  Skip items in old that are missing (i.e. absent) in `names(x)`.  
  Default `FALSE` halts with error if any are missing.

## Value

Returns: VOID. This method modifies the object by reference.  
Do not use assignment like `names(x) <- sb_setRename(x, ...)`.  
Since this function returns void, you'll just get `NULL`.  
  

## Examples

``` r
 
# mutable atomic vector ====
x <- y <- mutatomic(1:10, names = letters[1:10])
print(x)
#>  a  b  c  d  e  f  g  h  i  j 
#>  1  2  3  4  5  6  7  8  9 10 
#> mutatomic 
#> typeof:  integer 
sb_setFlatnames(x, newnames = rev(letters[1:10]))
print(y)
#>  j  i  h  g  f  e  d  c  b  a 
#>  1  2  3  4  5  6  7  8  9 10 
#> mutatomic 
#> typeof:  integer 

x <- y <- mutatomic(1:10, names = letters[1:10])
print(x)
#>  a  b  c  d  e  f  g  h  i  j 
#>  1  2  3  4  5  6  7  8  9 10 
#> mutatomic 
#> typeof:  integer 
sb_setFlatnames(x, 1L, "XXX")
print(y)
#> XXX   b   c   d   e   f   g   h   i   j 
#>   1   2   3   4   5   6   7   8   9  10 
#> mutatomic 
#> typeof:  integer 

################################################################################


# mutable atomic matrix ====
x <- mutatomic(
  1:20, dim = c(5, 4), dimnames = n(letters[1:5], letters[1:4])
)
print(x)
#>   a  b  c  d
#> a 1  6 11 16
#> b 2  7 12 17
#> c 3  8 13 18
#> d 4  9 14 19
#> e 5 10 15 20
#> mutatomic 
#> typeof:  integer 
sb_setDimnames(
  x,
  1:2,
  lapply(dimnames(x), rev)
)
print(x)
#>   d  c  b  a
#> e 1  6 11 16
#> d 2  7 12 17
#> c 3  8 13 18
#> b 4  9 14 19
#> a 5 10 15 20
#> mutatomic 
#> typeof:  integer 



################################################################################



# data.table ====

x <- data.table::data.table(
  a = 1:20,
  b = letters[1:20]
)
print(x)
#>         a      b
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
#> 11:    11      k
#> 12:    12      l
#> 13:    13      m
#> 14:    14      n
#> 15:    15      o
#> 16:    16      p
#> 17:    17      q
#> 18:    18      r
#> 19:    19      s
#> 20:    20      t
#>         a      b
#>     <int> <char>
sb_setVarnames(x, old = names(x), new = rev(names(x)))
print(x)
#>         b      a
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
#> 11:    11      k
#> 12:    12      l
#> 13:    13      m
#> 14:    14      n
#> 15:    15      o
#> 16:    16      p
#> 17:    17      q
#> 18:    18      r
#> 19:    19      s
#> 20:    20      t
#>         b      a
#>     <int> <char>
```
