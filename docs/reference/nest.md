# Nest

The [c](https://rdrr.io/r/base/c.html)`()` function concatenates vectors
or lists into a vector (if possible) or else a list.  
In analogy to that function, the `n()` function **nests** objects into a
list (not into an atomic vector, as atomic vectors cannot be nested).  
It is a short-hand version of the
[list](https://rdrr.io/r/base/list.html) function.  
This is handy because lists are often needed in 'squarebrackets',
especially for arrays.  

## Usage

``` r
n()
```

## Value

The list.

## Examples

``` r
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
ss_x(obj, n(1:3, 1:2), c(1,3))
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    2    6   10   14
#> [3,]    3    7   11   15
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   17   21   25   29
#> [2,]   18   22   26   30
#> [3,]   19   23   27   31
#> 
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
```
