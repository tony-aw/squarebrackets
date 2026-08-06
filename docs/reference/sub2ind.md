# Convert Subscripts to Coordinates, Coordinates to Flat Indices, and Vice-Versa

These functions convert a list of integer subscripts to an integer
matrix of coordinates, an integer matrix of coordinates to an integer
vector of flat indices, and vice-versa.  
Inspired by the `sub2ind` function from 'MatLab'.  

- `sub2coord()` converts a list of integer subscripts to an integer
  matrix of coordinates.

- `coord2ind()` converts an integer matrix of coordinates to an integer
  vector of flat indices.

- `ind2coord()` converts an integer vector of flat indices to an integer
  matrix of coordinates.

- `coord2sub()` converts an integer matrix of coordinates to a list of
  integer subscripts;  
  it performs a very simple (one might even say naive) conversion.  

- `sub2ind()` is a faster and more memory efficient version of  
  `coord2ind(sub2coord(sub, x.dims), x.dims)`  
    

All of these functions are written to be memory-efficient.  
The `coord2ind()` is thus the opposite of
[arrayInd](https://rdrr.io/r/base/which.html), and `ind2coord` is merely
a convenient wrapper around
[arrayInd](https://rdrr.io/r/base/which.html).  
  
Note that the equivalent to the `sub2ind` function from 'MatLab' is
actually the `coord2ind()` function here.  
  

## Usage

``` r
sub2coord(sub, x.dim)

coord2sub(coord)

coord2ind(coord, x.dim, checks = TRUE)

ind2coord(ind, x.dim)

sub2ind(sub, x.dim, checks = TRUE)
```

## Arguments

- sub:

  a list of integer subscripts.  
  The first element of the list corresponds to the first dimension
  (rows), the second element to the second dimensions (columns), etc.  
  The length of `sub` must be equal to the length of `x.dim`.  
  One cannot give an empty subscript; instead fill in something like
  `seq_len(dim(x)[margin])`.  
  NOTE: The `coord2sub()` function does not support duplicate
  subscripts.

- x.dim:

  an integer vector giving the dimensions of the array in question. I.e.
  `dim(x)`.

- coord:

  an integer matrix, giving the coordinate indices (subscripts) to
  convert.  
  Each row is an index, and each column is the dimension.  
  The first columns corresponds to the first dimension, the second
  column to the second dimensions, etc.  
  The number of columns of `coord` must be equal to the length of
  `x.dim`.  

- checks:

  Boolean, indicating if arguments checks should be performed.  
  Defaults to `TRUE`.  
  Can be set to `FALSE` for minor speed improvements.  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

- ind:

  an integer vector, giving the flat position indices to convert.

## Value

For `sub2coord()` and `ind2coord()`:  
Returns an integer matrix of coordinates (with properties as described
in argument `coord`).  
  
For `coord2ind()`:  
Returns an numeric vector of flat indices (with properties as described
in argument `ind`).  
  
For `coord2sub()`:  
Returns a list of integer subscripts (with properties as described in
argument `sub`)  
  
For `sub2ind()`:  
Returns an integer vector of flat indices(if
`prod(x.dim) < (2^31 - 1)`), or an numeric vector of flat indices (if
`prod(x.dim) >= (2^31 - 1)`).  
  

## Details

The base S3 vector and array classes in 'R' use the standard Linear
Algebraic convention, as in academic fields like Mathematics and
Statistics, in the following sense:  

- vectors are **column** vectors (i.e. vertically aligned vectors);

- index counting starts at `1`;

- rows are the first dimension/subscript, columns are the second
  dimension/subscript, etc.

Thus, the orientation of flat indices in, for example, a
4-rows-by-5-columns matrix, is as follows:

         [,1] [,2] [,3] [,4] [,5]
    [1,]    1    5    9   13   17
    [2,]    2    6   10   14   18
    [3,]    3    7   11   15   19
    [4,]    4    8   12   16   20

So in a 4 by 5 matrix, subscript `[1, 2]` corresponds to flat index
`5`.  
Array subscripting in 'squarebrackets' also follows this standard
convention.  
  

## Note

These functions were not specifically designed for duplicate indices
per-sé.  
For efficiency, they do not check for duplicate indices either.  

## Examples

``` r
x.dim <- c(10, 10, 3)
x.len <- prod(x.dim)
x <- array(1:x.len, x.dim)
sub <- list(c(4, 3), c(3, 2), c(2, 3))
coord <- sub2coord(sub, x.dim)
print(coord)
#>      [,1] [,2] [,3]
#> [1,]    4    3    2
#> [2,]    3    3    2
#> [3,]    4    2    2
#> [4,]    3    2    2
#> [5,]    4    3    3
#> [6,]    3    3    3
#> [7,]    4    2    3
#> [8,]    3    2    3
ind <- coord2ind(coord, x.dim)
print(ind)
#> [1] 124 123 114 113 224 223 214 213
all(x[ind] == c(x[c(4, 3), c(3, 2), c(2, 3)])) # TRUE
#> [1] TRUE
coord2 <- ind2coord(ind, x.dim)
print(coord)
#>      [,1] [,2] [,3]
#> [1,]    4    3    2
#> [2,]    3    3    2
#> [3,]    4    2    2
#> [4,]    3    2    2
#> [5,]    4    3    3
#> [6,]    3    3    3
#> [7,]    4    2    3
#> [8,]    3    2    3
all(coord == coord2) # TRUE
#> [1] TRUE
sub2 <- coord2sub(coord2)
sapply(1:3, \(i) sub2[[i]] == sub[[i]]) |> all() # TRUE
#> [1] TRUE
```
