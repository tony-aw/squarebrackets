# Get Size Properties of an Object

`ndim(x)` is short-hand for `length(dim(x))`.  
  
`s(x, m)` gets the size of an object `x` along margin `m`.  
If `m == 0`, `s()` gets the length of an object.  
If `m > 0`, `s()` gets `dim(x)[m]` of the object.  
  

## Usage

``` r
ndim(x)

rdim(x)

s(x, ...)

# Default S3 method
s(x, m = if (is.null(dim(x))) 0L else dim(x), ...)
```

## Arguments

- x:

  a vector, array, or data.frame.

- ...:

  further arguments passed to or from methods.

- m:

  the margin.

## Value

For `ndim()`:  
An integer, giving the number of dimensions `x` has.  
For vectors, gives `0L`.  
  
For `rdim()`:  
An integer vector, giving the range `1:ndim(x)`.  
For vectors, gives `0L`.  
  
For `s()`:  
A numeric vector giving the size(s) of the object at the given
margin(s).

## Examples

``` r
x <- 1:10
ndim(x)
#> [1] 0
obj <- array(1:64, c(4,4,3))
print(obj)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    2    6   10   14
#> [3,]    3    7   11   15
#> [4,]    4    8   12   16
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   17   21   25   29
#> [2,]   18   22   26   30
#> [3,]   19   23   27   31
#> [4,]   20   24   28   32
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   33   37   41   45
#> [2,]   34   38   42   46
#> [3,]   35   39   43   47
#> [4,]   36   40   44   48
#> 
ndim(obj)
#> [1] 3
```
