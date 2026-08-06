# Regular Sequence-based stride

`stride_seq()` specifies a regular sequence for use in the `long_`
methods.  
For example:  
`long_x(x, stride_seq(from, to, by))`  
is conceptually equivalent to  
`x[seq(from, to, by)]`  

## Usage

``` r
stride_seq(from, to, by, use = 1L)
```

## Arguments

- from:

  natural scalar giving the starting point of the sequence.

- to:

  natural scalar giving the maximalle allowed end value.

- by:

  natural scalar giving the step-size.

- use:

  `1` to select indices `seq(from, to, end)`;  
  `-1` to select all indices **except** `seq(from, to, end)`

## Value

An object of class "stride".

## See also

[squarebrackets_stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md)  

## Examples

``` r

x <- mutatomic(1:1e7)

# extract elements 2 to 9
long_x(x, stride_seq(2, 9, 1))
#> [1] 2 3 4 5 6 7 8 9
#> mutatomic 
#> typeof:  integer 

# reverse:
long_x(x, stride_seq(9, 2, 1))
#> [1] 9 8 7 6 5 4 3 2
#> mutatomic 
#> typeof:  integer 

# remove elements 1 to length(x) - 10:
long_x(x, stride_seq(1, length(x) - 10, 1, -1))
#>  [1]  9999991  9999992  9999993  9999994  9999995  9999996  9999997  9999998
#>  [9]  9999999 10000000
#> mutatomic 
#> typeof:  integer 

# replace every other element:
x <- mutatomic(1:1e7)
long_set(x, stride_seq(2, length(x), 2), rp = -1)
#> coercing replacement to integer
head(x)
#> [1]  1 -1  3 -1  5 -1
#> mutatomic 
#> typeof:  integer 

# replace all elements except the first element:
x <- mutatomic(1:1e7)
long_set(x, stride_seq(1, 1, 1, -1), rp = -10)
#> coercing replacement to integer
head(x)
#> [1]   1 -10 -10 -10 -10 -10
#> mutatomic 
#> typeof:  integer 
```
