# Methods to Modify Subsets of a Mutable Object By Reference

Methods to replace or transform a subset of a [supported mutable
object](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md)
using [pass-by-reference
semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md).  

## Usage

``` r
ii_set(x, i = NULL, use = 1, ..., rp, tf)

ss_set(x, s = NULL, use = rdim(x), ..., rp, tf)

tt_set(x, row = NULL, col = NULL, use = 1:2, ..., rp, tf)

# Default S3 method
ii_set(
  x,
  i = NULL,
  use = 1,
  ...,
  rp,
  tf,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# Default S3 method
ss_set(
  x,
  s = NULL,
  use = rdim(x),
  ...,
  rp,
  tf,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# Default S3 method
tt_set(
  x,
  row = NULL,
  col = NULL,
  use = 1:2,
  ...,
  rp,
  tf,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# S3 method for class 'data.table'
tt_set(
  x,
  row = NULL,
  col = NULL,
  use = 1:2,
  ...,
  rp,
  tf,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)
```

## Arguments

- x:

  a **variable** belonging to one of the [supported mutable
  classes](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md).  

- i, use, s, row, col:

  See
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).  
  An empty index selection leaves the original object unchanged.  

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).

- rp, tf:

  see
  [squarebrackets_modify](https://tony-aw.github.io/squarebrackets/reference/aaa05_squarebrackets_modify.md).

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

## Value

Returns: VOID. This method modifies the object by reference.  
Do not use assignments like `x <- ii_set(x, ...)`.  
Since this function returns void, you'll just get `NULL`.  
  

## Details

**Transform or Replace**  
Specifying argument `tf` will transform the subset. Specifying `rp` will
replace the subset. One cannot specify both `tf` and `rp`. It's either
one set or the other.  
  

## Examples

``` r

# mutatomic objects ====

gen_mat <- function() {
  obj <- as.mutatomic(matrix(1:16, ncol = 4))
  colnames(obj) <- c("a", "b", "c", "a")
  return(obj)
}

obj <- obj2 <- gen_mat()
print(obj)
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
#> mutatomic 
#> typeof:  integer 

ss_set(obj, n(1:3), 1:ndim(obj), rp = -1:-9)
print(obj2)
#>       a  b  c  a
#> [1,] -1 -4 -7 13
#> [2,] -2 -5 -8 14
#> [3,] -3 -6 -9 15
#> [4,]  4  8 12 16
#> mutatomic 
#> typeof:  integer 
# above is like x[1:3, 1:3] <- -1:-9, but using pass-by-reference

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
#> mutatomic 
#> typeof:  integer 

ii_set(obj, i = \(x) x <= 5, rp = -1:-5)
print(obj2)
#>       a  b  c  a
#> [1,] -1 -5  9 13
#> [2,] -2  6 10 14
#> [3,] -3  7 11 15
#> [4,] -4  8 12 16
#> mutatomic 
#> typeof:  integer 
# above is like x[x <= 5] <- -1:-5, but using pass-by-reference

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
#> mutatomic 
#> typeof:  integer 

ss_set(obj, n("a"), 2L, rp = cbind(-1:-4, -5:-8))
print(obj2)
#>       a b  c  a
#> [1,] -1 5  9 -5
#> [2,] -2 6 10 -6
#> [3,] -3 7 11 -7
#> [4,] -4 8 12 -8
#> mutatomic 
#> typeof:  integer 
# above is like x[, "a"] <- cbind(-1:-4, -5:-8), but using pass-by-reference

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
#> mutatomic 
#> typeof:  integer 

ss_set(obj, n(1:3), 1:ndim(obj), tf = \(x) -x)
print(obj2)
#>       a  b   c  a
#> [1,] -1 -5  -9 13
#> [2,] -2 -6 -10 14
#> [3,] -3 -7 -11 15
#> [4,]  4  8  12 16
#> mutatomic 
#> typeof:  integer 
# above is like x[1:3, 1:3] <- -1 * x[1:3, 1:3], but using pass-by-reference

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
#> mutatomic 
#> typeof:  integer 

ii_set(obj, i = \(x) x <= 5, tf = \(x) -x)
print(obj2)
#>       a  b  c  a
#> [1,] -1 -5  9 13
#> [2,] -2  6 10 14
#> [3,] -3  7 11 15
#> [4,] -4  8 12 16
#> mutatomic 
#> typeof:  integer 
# above is like x[x <= 5] <- -1 * x[x <= 5], but using pass-by-reference

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
#> mutatomic 
#> typeof:  integer 

ss_set(obj, n("a"), 2L, tf = \(x) -x)
obj2
#>       a b  c   a
#> [1,] -1 5  9 -13
#> [2,] -2 6 10 -14
#> [3,] -3 7 11 -15
#> [4,] -4 8 12 -16
#> mutatomic 
#> typeof:  integer 
# above is like x[, "a"] <- -1 * x[, "a"], but using pass-by-reference


gen_array <- function() {
  as.mutatomic(array(1:64, c(4,4,3)))
}
obj <- obj2 <- gen_array()
obj
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    2    6   10   14
#> [3,]    3    7   11   15
#> [4,]    4    8   12   16
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   17   21   25   29
#> [2,]   18   22   26   30
#> [3,]   19   23   27   31
#> [4,]   20   24   28   32
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   33   37   41   45
#> [2,]   34   38   42   46
#> [3,]   35   39   43   47
#> [4,]   36   40   44   48
#> 
#> mutatomic 
#> typeof:  integer 

ss_set(obj, n(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
print(obj2)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   -1   -4    9   13
#> [2,]   -2   -5   10   14
#> [3,]   -3   -6   11   15
#> [4,]    4    8   12   16
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   17   21   25   29
#> [2,]   18   22   26   30
#> [3,]   19   23   27   31
#> [4,]   20   24   28   32
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   -7  -10   41   45
#> [2,]   -8  -11   42   46
#> [3,]   -9  -12   43   47
#> [4,]   36   40   44   48
#> 
#> mutatomic 
#> typeof:  integer 
# above is like x[1:3, , 1:2] <- -1:-12, but using pass-by-reference


obj <- obj2 <- gen_array()
obj
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    2    6   10   14
#> [3,]    3    7   11   15
#> [4,]    4    8   12   16
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   17   21   25   29
#> [2,]   18   22   26   30
#> [3,]   19   23   27   31
#> [4,]   20   24   28   32
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   33   37   41   45
#> [2,]   34   38   42   46
#> [3,]   35   39   43   47
#> [4,]   36   40   44   48
#> 
#> mutatomic 
#> typeof:  integer 
ii_set(obj, i = \(x)x <= 5, rp = -1:-5)
print(obj2)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   -1   -5    9   13
#> [2,]   -2    6   10   14
#> [3,]   -3    7   11   15
#> [4,]   -4    8   12   16
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   17   21   25   29
#> [2,]   18   22   26   30
#> [3,]   19   23   27   31
#> [4,]   20   24   28   32
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   33   37   41   45
#> [2,]   34   38   42   46
#> [3,]   35   39   43   47
#> [4,]   36   40   44   48
#> 
#> mutatomic 
#> typeof:  integer 
# above is like x[x <= 5] <- -1:-5, but using pass-by-reference



#############################################################################

# data.table ====

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<externalptr> 
tt_set(
  obj, with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
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

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
tt_set(obj, col = is.numeric, tf = as.numeric) # first coerce type by whole columns
str(obj)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: num  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<externalptr> 
tt_set(obj, with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # SAFE: coercion performed by column first
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

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<externalptr> 
tt_set(obj,
        col = is.numeric,
        tf = sqrt # SAFE: obs = NULL, so coercion performed
)
str(obj)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  1 1.41 1.73 2 2.24 ...
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: num  3.32 3.46 3.61 3.74 3.87 ...
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
