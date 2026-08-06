# Compute Ordered Indices

Computes ordered indices. Similar to
[order](https://rdrr.io/r/base/order.html), except the user must supply
a vector, a list of equal-length vectors, a data.frame or a matrix
(row-wise and column-wise are both supported), as the input.  
  
For a vector `x`,  
`idx_ord_v(x)` is equivalent to  
[order](https://rdrr.io/r/base/order.html)`(x)`.  
  
For a data.frame or a list of equal-length vectors `x`, with `p`
columns/elements,  
`idx_ord_df(x)` is equivalent to  
`order(x[[1]], ..., x[[p]])`.  
  
For a matrix (or array) `x` with `p` rows,  
`idx_ord_m(x, margin = 1)` is equivalent to  
`order(x[1, ], ..., x[p, ], ...)`.  
  
For a matrix (or array) `x` with `p` columns,  
`idx_ord_m(x, margin = 2)` is equivalent to  
`order(x[, 1], ..., x[, p], ...)`.  
  
Note that these are merely convenience functions, and that these are
actually slightly slower than [order](https://rdrr.io/r/base/order.html)
(except for `idx_ord_v()`), due to the additional functionality.  
  

## Usage

``` r
idx_ord_v(
  x,
  na.last = TRUE,
  decr = FALSE,
  method = c("auto", "shell", "radix")
)

idx_ord_m(
  x,
  margin,
  na.last = TRUE,
  decr = FALSE,
  method = c("auto", "shell", "radix")
)

idx_ord_df(
  x,
  na.last = TRUE,
  decr = FALSE,
  method = c("auto", "shell", "radix")
)
```

## Arguments

- x:

  a vector, data.frame, or array

- na.last, method:

  see [order](https://rdrr.io/r/base/order.html) and
  [sort](https://rdrr.io/r/base/sort.html).

- decr:

  see argument `decreasing` in
  [order](https://rdrr.io/r/base/order.html)

- margin:

  the margin over which to cut the matrix/array into vectors.  
  I.e. `margin = 1L` will cut `x` into individual rows, and apply the
  [order](https://rdrr.io/r/base/order.html) on those rows.  
  And `margin = 2L` will cut `x` into columns, etc.

## Value

See [order](https://rdrr.io/r/base/order.html).

## Examples

``` r
x <- sample(1:10)
order(x)
#>  [1]  7  6  1  2 10  9  5  8  4  3
idx_ord_v(x)
#>  [1]  7  6  1  2 10  9  5  8  4  3
idx_ord_m(rbind(x, x), 1)
#>  [1]  7  6  1  2 10  9  5  8  4  3
idx_ord_m(cbind(x, x), 2)
#>  [1]  7  6  1  2 10  9  5  8  4  3
idx_ord_df(data.frame(x, x))
#>  [1]  7  6  1  2 10  9  5  8  4  3
 
```
