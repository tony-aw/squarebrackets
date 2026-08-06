# Methods to Return Object Without Specified Subset

S3 Methods to return an object **without** the specified subset.  
  

## Usage

``` r
ii_wo(x, ...)

ss_wo(x, ...)

sbt_wo(x, ...)

# Default S3 method
ii_wo(x, i = NULL, ..., chkdup = getOption("squarebrackets.chkdup", FALSE))

# Default S3 method
ss_wo(
  x,
  s = NULL,
  d = 1:ndim(x),
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# Default S3 method
sbt_wo(
  x,
  row = NULL,
  col = NULL,
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

# S3 method for class 'data.frame'
sbt_wo(
  x,
  obs = NULL,
  vars = NULL,
  ...,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)
```

## Arguments

- x:

  see
  [squarebrackets_supported_structures](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md).

- ...:

  see
  [squarebrackets_method_dispatch](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_method_dispatch.md).

- i, s, d, row, col, obs, vars:

  See
  [squarebrackets_indx_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md).  
  An empty index selection results in nothing being removed, and the
  entire object is returned.  

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

## Value

A copy of the sub-setted object.

## Examples

``` r
# atomic objects ====

obj <- matrix(1:16, ncol = 4)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
ss_wo(obj, n(1:3), 1:ndim(obj))
#>       a
#> [1,] 16
# above is equivalent to  obj[-1:-3, -1:-3, drop = FALSE]
ii_wo(obj, i = \(x) x > 5)
#> [1] 1 2 3 4 5
# above is equivalent to  obj[!obj > 5]
ss_wo(obj, n("a"), 2L)
#>      b  c
#> [1,] 5  9
#> [2,] 6 10
#> [3,] 7 11
#> [4,] 8 12
# above is equivalent to  obj[, which(!colnames(obj) %in% "a")]

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
ss_wo(obj, n(1, c(1, 3)), c(1, 3))
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   18   22   26   30
#> [2,]   19   23   27   31
#> [3,]   20   24   28   32
#> 
# above is equivalent to obj[-1, , c(-1, -3), drop = FALSE]
ii_wo(obj, i = \(x)x > 5)
#> [1] 1 2 3 4 5
# above is equivalent to obj[!obj > 5]



#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
#> $a
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $c
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
ii_wo(obj, "a")
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $c
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
# above is equivalent to obj[which(!names(obj) %in% "a")]
ii_wo(obj, 1) # obj[-1]
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $c
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
ii_wo(obj, 1:2)
#> $c
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
# above is equivalent to obj[seq_len(length(obj))[-1:-2]]
obj <- list(a = 1:10, b = letters[1:11], c = letters)
ii_wo(obj, is.numeric)
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $c
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
# above is equivalent to obj[!sapply(obj, is.numeric)] # this time singular brackets?
# for recusive indexing, see lst_rec()


obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
#>      a            b            c            a           
#> [1,] logical,3    logical,3    logical,3    logical,3   
#> [2,] integer,10   integer,10   integer,10   integer,10  
#> [3,] numeric,10   numeric,10   numeric,10   numeric,10  
#> [4,] character,26 character,26 character,26 character,26
ss_wo(obj, n(1:3), 1:ndim(obj))
#>      a           
#> [1,] character,26
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
ii_wo(obj, i = is.numeric)
#> [[1]]
#> [1]    NA  TRUE FALSE
#> 
#> [[2]]
#>  [1] "d" "r" "t" "h" "g" "x" "j" "l" "n" "o" "q" "c" "m" "k" "y" "b" "v" "a" "s"
#> [20] "f" "p" "w" "u" "e" "z" "i"
#> 
#> [[3]]
#> [1] FALSE    NA  TRUE
#> 
#> [[4]]
#>  [1] "s" "h" "o" "v" "a" "g" "l" "y" "b" "f" "u" "e" "t" "q" "k" "x" "r" "z" "j"
#> [20] "n" "i" "p" "m" "c" "d" "w"
#> 
#> [[5]]
#> [1] FALSE  TRUE    NA
#> 
#> [[6]]
#>  [1] "d" "x" "l" "m" "e" "z" "w" "v" "t" "f" "u" "j" "y" "p" "g" "n" "i" "b" "a"
#> [20] "c" "q" "r" "k" "o" "s" "h"
#> 
#> [[7]]
#> [1] FALSE    NA  TRUE
#> 
#> [[8]]
#>  [1] "y" "o" "t" "e" "i" "g" "s" "j" "m" "x" "k" "d" "u" "l" "r" "h" "z" "n" "c"
#> [20] "a" "b" "v" "p" "w" "f" "q"
#> 
# above is equivalent to obj[sapply(obj, is.numeric)]
ss_wo(obj, n(c("a", "a")), 2L)
#>      b            c           
#> [1,] logical,3    logical,3   
#> [2,] integer,10   integer,10  
#> [3,] numeric,10   numeric,10  
#> [4,] character,26 character,26
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(as.list(1:64), c(4,4,3))
print(obj)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] 1    5    9    13  
#> [2,] 2    6    10   14  
#> [3,] 3    7    11   15  
#> [4,] 4    8    12   16  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] 17   21   25   29  
#> [2,] 18   22   26   30  
#> [3,] 19   23   27   31  
#> [4,] 20   24   28   32  
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] 33   37   41   45  
#> [2,] 34   38   42   46  
#> [3,] 35   39   43   47  
#> [4,] 36   40   44   48  
#> 
ss_wo(obj, n(1, c(1, 3)), c(1, 3))
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] 18   22   26   30  
#> [2,] 19   23   27   31  
#> [3,] 20   24   28   32  
#> 
# above is equivalent to obj[-1, , c(-1, -3), drop = FALSE]
ii_wo(obj, i = \(x)x>5)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 
# above is equivalent to obj[!sapply(obj, \(x) x > 5)]



#############################################################################

# data.frame-like objects ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
print(obj)
#>     a b  c d
#> 1   1 a 11 a
#> 2   2 b 12 b
#> 3   3 c 13 c
#> 4   4 d 14 d
#> 5   5 e 15 e
#> 6   6 f 16 f
#> 7   7 g 17 g
#> 8   8 h 18 h
#> 9   9 i 19 i
#> 10 10 j 20 j
sbt_wo(obj, 1:3, 1:3)
#>   d
#> 1 d
#> 2 e
#> 3 f
#> 4 g
#> 5 h
#> 6 i
#> 7 j
# above is equivalent to obj[-1:-3, -1:-3, drop = FALSE]
sbt_wo(obj, ~ (a > 5) & (c < 19), is.numeric)
#>   b d
#> 1 a a
#> 2 b b
#> 3 c c
#> 4 d d
#> 5 e e
#> 6 i i
#> 7 j j


```
