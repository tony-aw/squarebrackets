# Argument stride

‘squarebrackets’ provides the
[long_x](https://tony-aw.github.io/squarebrackets/reference/long.md) and
[long_set](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods to perform sub-set operations on the interior of a vector,
without any indexing vector at all.  
The advantage of not using an indexing vector for sub-set operations on
a long vector, is that it may safe the user a large amount of memory.
Instead of an indexing vector, they use the `stride` argument.  
The following can be used in the `stride` argument:  

- a
  [stride_v](https://tony-aw.github.io/squarebrackets/reference/stride_v.md)
  object:  
  Use this `stride` type to specify value-based subsets, like `y == v`,
  where `y` is a vector of the same length as `x`, and `v` is a value
  (or range of values) `y` might contain.  

- a
  [stride_seq](https://tony-aw.github.io/squarebrackets/reference/stride_seq.md)
  object:  
  Use this `stride` type to specify a sequence in the form of
  `seq(from, to, by)`, without actually allocating a sequence indexing
  vector.

- a
  [stride_ptrn](https://tony-aw.github.io/squarebrackets/reference/stride_ptrn.md)
  object:  
  Use this `stride` type to specify a patterned sequence in the form
  of  
  `(start:end)[pattern]`,  
  where `start` and `end` are natural scalars and `pattern` is a logical
  vector.  
  [stride_ptrn](https://tony-aw.github.io/squarebrackets/reference/stride_ptrn.md)
  specifies this sequence without actually allocating an indexing
  vector.

- A formula in the form of ` ~ from:to:by:use`, where `from`, `to` and
  `by` are natural scalars, and `use` is `1` or `-1`.  
  This will be interpreted as `stride_seq(from, to, by, use)`.  

- A formula in the form of ` ~ start:end:ptrn:use`, where `start` and
  `end` are natural scalars, `use` is `1` or `-1`, and `ptrn` is a
  logical vector.  
  This will be interpreted as `stride_ptrn(start, end, ptrn, use)`.  
    

In both formula forms, the `.N` keyword is available, which equals
`length(x)` (where `x` is the input vector).  
For example,  
` ~ 2:(.N - 1):c(TRUE, FALSE, FALSE, TRUE):1`  
is equivalent to ` ~ 2:(length(x) - 1):c(TRUE, FALSE, FALSE, TRUE):1`,  
and will be interpreted as
`stride_ptrn(2, length(x) - 1, c(TRUE, FALSE, FALSE, TRUE), 1)`.  
  

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
