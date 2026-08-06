# Compute Indices for Copy-On-Modify Substitution

The `_icom()` methods compute indices, suitable for usage in R's native
copy-on-modify substitution.  
  
The `arepl(x, sub, value)` function directly evaluates the expression  
`x[ sub[[1]], ... , sub[[ndim(x)]] ] <- value`  
in the calling environment.  
  
Demonstration:

    x <- array(...)
    myss <- ss_icom(x, s, use)
    arepl(x, myss, value)

    y <- data.frame(...)
    rows <- tt_icom(y, 1:10, 1, -1L)
    cols <- tt_icom(y, c("a", "b"), 2L)
    y[rows, cols] <- value

These allow the user to benefit from the convenient index translations
from 'squarebrackets', whilst still using R's default
copy-on-modification semantics (instead of the semantics provided by
'squarebrackets').  
  
  

## Usage

``` r
arepl(x, sub, value)

ii_icom(x, i = NULL, use = 1, ...)

ss_icom(x, s = NULL, use = 1:ndim(x), ...)

tt_icom(x, slice, use, ...)

# Default S3 method
ii_icom(
  x,
  i = NULL,
  use = 1,
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# Default S3 method
ss_icom(
  x,
  s = NULL,
  use = 1:ndim(x),
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# Default S3 method
tt_icom(
  x,
  slice = NULL,
  use = NULL,
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)
```

## Arguments

- x:

  vector, matrix, array, or data.frame; both atomic and recursive
  objects are supported.

- sub:

  a list of integer subscripts.  
  The first element of the list corresponds to the first dimension
  (rows), the second element to the second dimensions (columns), etc.  
  The length of `sub` must be equal to the length of `ndim(x)`.  
  One cannot give an empty subscript; instead fill in something like
  `seq_len(dim(x)[margin])`.  

- value:

  the replacement value

- i, s, slice, use:

  See
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).  
  Duplicates are not allowed.

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

## Value

For `ii_icom()`:  
A strictly positive numeric vector of indices.  
To be used in the flat form of the `[<-` operator.  
  
For `tt_icom()`:  
A strictly positivie numeric vector of either row or column indices.  
To be used in the first (for rows) or second (for columns) slot of the
tabular form of the `[<-` operator.  
  
For `ss_icom()`:  
A list of strictly positive integer vectors, containing array
subscripts.  
To be used in the `arepl()` function.  
Can also be combined with
[ss2ii](https://tony-aw.github.io/squarebrackets/reference/ss2ii.md) to
use in the flat form of the `[<-` operator.  
  
For `arepl()`:  
Returns nothing, but modfies `x` in place using R's default semantics.  
  

## Examples

``` r
# atomic ====

x <- 1:10
x[ii_icom(x, \(x)x > 5L)] <- -5L
print(x)
#>  [1]  1  2  3  4  5 -5 -5 -5 -5 -5

x <- array(1:27, dim = c(3,3,3))
sub <- ss_icom(x, n(1:2, 1:2), c(1,3))
arepl(x, sub, -10L)
print(x)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  -10  -10  -10
#> [2,]  -10  -10  -10
#> [3,]    3    6    9
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]  -10  -10  -10
#> [2,]  -10  -10  -10
#> [3,]   12   15   18
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   22   25
#> [2,]   20   23   26
#> [3,]   21   24   27
#> 

ii <- ss_icom(x, 2:3) |> ss2ii(dim(x))
x[ii] <- -1000L
print(x)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  -10  -10  -10
#> [2,]  -10  -10  -10
#> [3,]    3    6    9
#> 
#> , , 2
#> 
#>      [,1]  [,2]  [,3]
#> [1,]  -10   -10   -10
#> [2,]  -10 -1000 -1000
#> [3,]   12 -1000 -1000
#> 
#> , , 3
#> 
#>      [,1]  [,2]  [,3]
#> [1,]   19    22    25
#> [2,]   20 -1000 -1000
#> [3,]   21 -1000 -1000
#> 


################################################################################


# recursive ====

x <- as.list(1:10)
x[ii_icom(x, \(x)x>5)] <- -5
print(x)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 
#> [[6]]
#> [1] -5
#> 
#> [[7]]
#> [1] -5
#> 
#> [[8]]
#> [1] -5
#> 
#> [[9]]
#> [1] -5
#> 
#> [[10]]
#> [1] -5
#> 

x <- array(as.list(1:27), dim = c(3,3,3))
sub <- ss_icom(x, n(1:2, 1:2), c(1,3))
arepl(x, sub, list(-10L))
print(x)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] -10  -10  -10 
#> [2,] -10  -10  -10 
#> [3,] 3    6    9   
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] -10  -10  -10 
#> [2,] -10  -10  -10 
#> [3,] 12   15   18  
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,] 19   22   25  
#> [2,] 20   23   26  
#> [3,] 21   24   27  
#> 


x <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE),
  b = 1:10,
  c = rnorm(10),
  d = letters[1:10],
  e = factor(letters[11:20])
)
rows <- tt_icom(x, 1:5, -1)
cols <- tt_icom(x, c("b", "a"), 2)
x[rows, cols] <- NA
print(x)
#>        a  b          c d e
#> 1  FALSE  1 -0.3406379 a k
#> 2   TRUE  2  0.7863626 b l
#> 3   TRUE  3 -1.2705131 c m
#> 4     NA  4  0.5421415 d n
#> 5   TRUE  5  0.0751059 e o
#> 6     NA NA  0.5585144 f p
#> 7     NA NA  0.4154064 g q
#> 8     NA NA -1.4522998 h r
#> 9     NA NA  0.9412061 i s
#> 10    NA NA -0.3389359 j t
```
