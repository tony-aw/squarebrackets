# Pattern Sequence-based stride

`stride_ptrn()` specifies a patterned sequence, in the form of
`(start:end)[ptrn]`, for use in the
[long\_](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods.  
  
For example:  
the sub-set operation `long_x(x, stride_ptrn(start, end, ptrn))`  
is conceptually equivalent to  
`x[(start:end)[ptrn]]`  
  
`ptrn_len(start, end, ptrn)` calculates the length of
`(start:end)[ptrn]`, without actually allocating said vector.  
  

## Usage

``` r
stride_ptrn(start, end, ptrn, use = 1L)

ptrn_len(start, end, ptrn)
```

## Arguments

- start:

  natural scalar giving the starting point of the sequence.

- end:

  natural scalar giving the ending point of the sequence.

- ptrn:

  a logical vector, giving the pattern.  
  `ptrn` must contain at least 1 `TRUE` and 1 `FALSE` element, to avoid
  ambiguity.  
  `ptrn` must have length that fulfils all of the following conditions:

  - it is `>= 2`

  - it is `<= length(start:end)`

  - it is `<= 2^31 - 1`

- use:

  `1` to select indices `(start:end)[ptrn]`;  
  `-1` to select all indices **except** `(start:end)[ptrn]`  

## Value

An object of class "stride".

## See also

[squarebrackets_stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md)  

## Examples

``` r

x <- mutatomic(1:1e7)

long_x(x, stride_ptrn(2, 20, c(TRUE, FALSE, FALSE, TRUE)))
#> [1]  2  5  6  9 10 13 14 17 18
#> mutatomic 
#> typeof:  integer 

long_x(x, stride_ptrn(20, 2, c(TRUE, FALSE, FALSE, TRUE)))
#> [1] 20 17 16 13 12  9  8  5  4
#> mutatomic 
#> typeof:  integer 

long_x(x, stride_ptrn(2, 20, c(TRUE, FALSE, FALSE, TRUE), -1)) |> head()
#> [1]  1  3  4  7  8 11
#> mutatomic 
#> typeof:  integer 

long_x(x, stride_ptrn(20, 2, c(TRUE, FALSE, FALSE, TRUE), -1)) |> head()
#> [1]  1  2  3  6  7 10
#> mutatomic 
#> typeof:  integer 
```
