# Index-less Subset Methods on (Long) Vectors

The `long_` - methods are similar to the `ii_` - methods, except they
don't require an indexing vector, and are designed for memory
efficiency.  
  

## Usage

``` r
long_x(x, ...)

# Default S3 method
long_x(
  x,
  stride,
  ...,
  use.names = TRUE,
  sticky = getOption("squarebrackets.sticky", FALSE)
)

long_set(x, ...)

# Default S3 method
long_set(x, stride, ..., rp, tf)
```

## Arguments

- x:

  an atomic object.  
  For `long_x()`, `couldb.mutatomic(x)` must be `TRUE`.  
  For `long_set()` it must be a
  [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
  **variable**.

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).

- stride:

  see
  [squarebrackets_stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md).

- use.names:

  Boolean, indicating if flat names should be preserved.  
  Note that, since the `long_` methods operates on **virtual** [interior
  indices](https://tony-aw.github.io/squarebrackets/reference/aaa02_squarebrackets_index_fundamentals.md)
  of an array/vector only, dimensions and `dimnames` are always
  dropped.  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)

- sticky:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).

- rp, tf:

  see
  [squarebrackets_modify](https://tony-aw.github.io/squarebrackets/reference/aaa05_squarebrackets_modify.md).

## Value

For `long_x()`: returns the sub-setted object.  
Fr `long_set()`: returns nothing, but modifies the object by
reference.  

## Examples

``` r
# extract all elements of x with the name "a":
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic(1:1e6, names = nms)
head(x)
#> a b c d e f 
#> 1 2 3 4 5 6 
#> mutatomic 
#> typeof:  integer 
stride <-  stride_v(names(x), v = "a")
long_x(x, stride) |> head()
#>   a   a   a   a   a   a 
#>   1  77 153 229 305 381 
#> mutatomic 
#> typeof:  integer 


# find all x smaller than or equal to 5, and replace with `-1000`:
stride <- stride_v(x, v = c(-Inf, 5))
long_set(x, stride, rp = -1000L)
head(x, n = 10)
#>     a     b     c     d     e     f     g     h     i     j 
#> -1000 -1000 -1000 -1000 -1000     6     7     8     9    10 
#> mutatomic 
#> typeof:  integer 




x <- mutatomic(1:1e7)

# extract elements 2 to 9
long_x(x, ~ 2:9:1:1)
#> [1] 2 3 4 5 6 7 8 9
#> mutatomic 
#> typeof:  integer 

# reverse:
long_x(x, ~ 9:2:1:1)
#> [1] 9 8 7 6 5 4 3 2
#> mutatomic 
#> typeof:  integer 

# remove all elements except the last 10:
long_x(x, ~ 1:(.N - 10):1:-1)
#>  [1]  9999991  9999992  9999993  9999994  9999995  9999996  9999997  9999998
#>  [9]  9999999 10000000
#> mutatomic 
#> typeof:  integer 

# replace every other element:
x <- mutatomic(1:1e7)
long_set(x, ~ 2:.N:2:1, rp = -1)
#> coercing replacement to integer
head(x)
#> [1]  1 -1  3 -1  5 -1
#> mutatomic 
#> typeof:  integer 

# replace all elements except the first element:
x <- mutatomic(1:1e7)
long_set(x, ~1:1:1:-1, rp = -1)
#> coercing replacement to integer
head(x)
#> [1]  1 -1 -1 -1 -1 -1
#> mutatomic 
#> typeof:  integer 


# extract pattern c(TRUE, FALSE, FALSE, TRUE) from first 20 elements:
x <- mutatomic(1:1e7)
long_x(x, ~ 1:20:c(TRUE, FALSE, FALSE, TRUE):1)
#>  [1]  1  4  5  8  9 12 13 16 17 20
#> mutatomic 
#> typeof:  integer 
```
