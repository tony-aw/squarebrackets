# Compute Integer Index Range

`idx_r()` computes integer index range(s).  
  

## Usage

``` r
idx_r(x, m = 0L, from = NULL, to = NULL, by = 1L)
```

## Arguments

- x:

  the object for which to compute subset indices.

- m, from, to, by:

  see
  [cp_seq](https://tony-aw.github.io/squarebrackets/reference/cp_seq.md).  
    

## Value

If `length(m) == 1L`: a vector of numeric indices.  
  
If `length(m) > 1L`: a list of the same length as `m`, containing
numeric vectors of indices.  
  

## Examples

``` r
x <- data.frame(
  a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = -1:-10
)
print(x)
#>     a b c   d
#> 1   1 a a  -1
#> 2   2 b b  -2
#> 3   3 c c  -3
#> 4   4 d d  -4
#> 5   5 e e  -5
#> 6   6 f f  -6
#> 7   7 g g  -7
#> 8   8 h h  -8
#> 9   9 i i  -9
#> 10 10 j j -10
ind1 <- idx_r(x, 1, 2, 2* -1i) # rows 2:(nrow(x)-1)
sbt_x(x, ind1) # extract the row range
#>   a b c  d
#> 1 2 b b -2
#> 2 3 c c -3
#> 3 4 d d -4
#> 4 5 e e -5
#> 5 6 f f -6
#> 6 7 g g -7
#> 7 8 h h -8
#> 8 9 i i -9

x <- array(1:125, c(5,5,5))
d <- 1:3
s <- idx_r(x, d, 2, 2* -1i) # 2:(n-1) for every dimension
ss_x(x, s = s, d = d) # same as x[ 2:4, 2:4, 2:4, drop = FALSE]
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]   32   37   42
#> [2,]   33   38   43
#> [3,]   34   39   44
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   57   62   67
#> [2,]   58   63   68
#> [3,]   59   64   69
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   82   87   92
#> [2,]   83   88   93
#> [3,]   84   89   94
#> 

x <- letters
x[idx_r(x, 0, 2, 2* -1i)]
#>  [1] "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
#> [20] "u" "v" "w" "x" "y"
```
