# Drop Singular List

'squarebrackets' provides some helper functions for sub-set operations
on recursive objects (lists and data.frames).  
  
`dropl(x)` returns `x[[1L]]` if `length(x) == 1`, and returns `x`
otherwise.  
This can be used for the `_x` methods on recursive objects, where one
would like to extract the contents of the singular selection.  
  

## Usage

``` r
dropl(x)
```

## Arguments

- x:

  a list.

## Value

For `dropl()`:  
If `length(x) == 1L`, `dropl(x)` returns `x[[1L]]`;  
otherwise it returns the original `x` unchanged.  
  

## Examples

``` r
obj <- as.list(1:10)
ii_x(obj, 1) |> dropl() # equivalent to obj[[1L]]
#> [1] 1

```
