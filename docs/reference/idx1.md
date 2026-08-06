# Convert/Translate Indices (for Copy-On-Modify Substitution)

The `idx()` method converts indices.  
The type of output depends on the type of input index arguments given:

- `idx(x, i = i, ...)` converts linear indices to a strictly positive
  integer vector of linear indices.

- `idx(x, s = s, d = d, ...)` converts dimensional indices to a strictly
  positive integer vector of linear indices.

- `idx(x, slice = slice, margin = margin, ...)` converts indices of one
  dimension to a strictly positive integer vector of indices for that
  specific dimension.

Vectors (both atomic and recursive) only have index argument `i`.  
Data.frame-like objects only have the `slice, margin` argument pair.  
Arrays (both atomic and recursive) have the `s, d` argument pair, as
well as the `i` argument and the `slice, margin` argument pair.  
  
The result of the `idx()` method can be used inside the regular
square-brackets operators.  
For example like so:

    x <- array(...)
    my_ss2ii <- idx(x, s, d)
    x[my_ss2ii] <- value

    y <- data.frame(...)
    rows <- idx(y, 1:10, 1, inv = TRUE)
    cols <- idx(y, c("a", "b"), 2)
    y[rows, cols] <- value

thus allowing the user to benefit from the convenient index translations
from 'squarebrackets', whilst still using R's default
copy-on-modification semantics (instead of the semantics provided by
'squarebrackets').  
  
  

## Usage

``` r
idx(x, ...)

# Default S3 method
idx(x, i, inv = FALSE, ..., chkdup = getOption("squarebrackets.chkdup", FALSE))

# S3 method for class 'array'
idx(
  x,
  s = NULL,
  d = 1:ndim(x),
  slice = NULL,
  margin = NULL,
  i = NULL,
  inv = FALSE,
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# S3 method for class 'data.frame'
idx(
  x,
  slice,
  margin,
  inv = FALSE,
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)
```

## Arguments

- x:

  vector, matrix, array, or data.frame; both atomic and recursive
  objects are supported.

- ...:

  see
  [squarebrackets_method_dispatch](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_method_dispatch.md).

- i, s, d, margin, slice, inv:

  See
  [squarebrackets_indx_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md).  
  Duplicates are not allowed.

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

## Value

For `idx(x, i = i, ...)` and `idx(x, s = s, d = d, ...)`:  
A strictly positive integer vector of flat indices.  
  
For `idx(x, margin = margin, slice = slice, ...)`:  
A strictly positive integer vector of indices for the dimension
specified in `margin`.  
  

## Examples

``` r
# atomic ====

x <- 1:10
x[idx(x, \(x)x>5)] <- -5
print(x)
#>  [1]  1  2  3  4  5 -5 -5 -5 -5 -5

x <- array(1:27, dim = c(3,3,3))
x[idx(x, n(1:2, 1:2), c(1,3))] <- -10
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


################################################################################


# recursive ====

x <- as.list(1:10)
x[idx(x, \(x)x>5)] <- -5
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
x[idx(x, n(1:2, 1:2), c(1,3))] <- -10
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
rows <- idx(x, 1:5, 1, inv = TRUE)
cols <- idx(x, c("b", "a"), 2)
x[rows, cols] <- NA
print(x)
#>        a  b           c d e
#> 1   TRUE  1 -0.31966240 a k
#> 2  FALSE  2  0.26491352 b l
#> 3   TRUE  3 -0.99044044 c m
#> 4  FALSE  4  1.00790915 d n
#> 5   TRUE  5  0.19031222 e o
#> 6     NA NA -1.41032315 f p
#> 7     NA NA -0.01069827 g q
#> 8     NA NA  0.79808063 h r
#> 9     NA NA  0.58953258 i s
#> 10    NA NA -0.94363267 j t
```
