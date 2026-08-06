# Efficient Value-based Subset Methods on (Long) Vectors

The `slicev_` - methods are similar to the `ii_`/`ss_` - methods, except
they don't require an indexing vector, and are designed for memory
efficiency.  
  
`counv(y, v)` counts how often a value, or range of values, `v`, occurs
in a vector `y`.  
  

## Usage

``` r
slicev_x(x, ...)

# Default S3 method
slicev_x(
  x,
  ...,
  y = x,
  v = NULL,
  na = FALSE,
  r = TRUE,
  use.names = TRUE,
  sticky = getOption("squarebrackets.sticky", FALSE)
)

slicev_set(x, ...)

# Default S3 method
slicev_set(x, ..., y = x, v = NULL, na = FALSE, r = TRUE, rp, tf)

countv(y, ..., v = NULL, na = FALSE, r = TRUE)
```

## Arguments

- x:

  an atomic vector.  
  For `slicev_set()` it must be a
  [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
  **variable**.

- ...:

  See
  [squarebrackets_slicev](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_slicev.md).

- y, v, na, r:

  See
  [squarebrackets_slicev](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_slicev.md).

- use.names:

  Boolean, indicating if flat names should be preserved.  
  Note that, since the `slicev_` methods operates on flat indices only,
  dimensions and `dimnames` are always dropped.

- sticky:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).

- rp, tf:

  see
  [squarebrackets_modify](https://tony-aw.github.io/squarebrackets/reference/aaa05_squarebrackets_modify.md).

## Value

Similar to the `ii_` methods.  
  
For `countv()`: A single number, giving the number of elements matching
the specified condition.  
  

## Examples

``` r
# basic idea ====
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic(1:1e6, names = nms)
head(x)
#> a b c d e f 
#> 1 2 3 4 5 6 
#> mutatomic 
#> typeof:  integer 

# memory efficient form of sum(x <= 10):
countv(x, v = c(-Inf, 10))
#> [1] 10

# extract all elements of x with the name "a":
slicev_x(x, y = names(x), v = "a") |> head()
#>   a   a   a   a   a   a 
#>   1  77 153 229 305 381 
#> mutatomic 
#> typeof:  integer 

# find all x smaller than or equal to 5, and replace with `-1000`:
slicev_set(x, y = x, v = c(-Inf, 5), rp = -1000L)
head(x, n = 10)
#>     a     b     c     d     e     f     g     h     i     j 
#> -1000 -1000 -1000 -1000 -1000     6     7     8     9    10 
#> mutatomic 
#> typeof:  integer 


################################################################################
# Numeric range ====
#
x <- mutatomic(1:1e6)
head(x)
#> [1] 1 2 3 4 5 6
#> mutatomic 
#> typeof:  integer 
slicev_x(x, v= c(-Inf, 5)) # x[x <= 5]
#> [1] 1 2 3 4 5
#> mutatomic 
#> typeof:  integer 


################################################################################
# Character ====
#
if(require(stringi)) {
  x <- stringi::stri_rand_shuffle(rep("hello", 1e5))
  head(x)
  slicev_x(x, v = "hello") |> head() # find "hello"
  
  # find 2 possible misspellings of "hello":
  slicev_x(x, v = c("holle", "helol")) |> head()
  
}
#> [1] "helol" "holle" "holle" "holle" "helol" "holle"



```
