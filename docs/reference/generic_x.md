# Methods to Extract, Exchange, Exclude, or Duplicate Subsets of an Object

Methods to extract, exchange, exclude, or duplicate (i.e. repeat x
times) subsets of an object.  
  

## Usage

``` r
ii_x(x, i = NULL, use = 1, ...)

ss_x(x, s = NULL, use = Inf, ...)

tt_x(x, row = NULL, col = NULL, use = 1:2, ...)

# Default S3 method
ii_x(x, i = NULL, use = 1, ...)

# Default S3 method
ss_x(x, s = NULL, use = Inf, ...)

# Default S3 method
tt_x(x, row = NULL, col = NULL, use = 1:2, ...)

# S3 method for class 'data.frame'
tt_x(x, row = NULL, col = NULL, use = 1:2, ...)
```

## Arguments

- x:

  see
  [squarebrackets_supported_structures](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md).

- i, use, s, row, col:

  See
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).  
  Duplicates are allowed, resulting in duplicated indices.  
  An empty index selection results in an empty object of length 0.  

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).

## Value

Returns a copy of the sub-setted object.

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
ss_x(obj, n(1:3), 1:ndim(obj))
#>      a b  c
#> [1,] 1 5  9
#> [2,] 2 6 10
#> [3,] 3 7 11
# above is equivalent to obj[1:3, 1:3, drop = FALSE]

ss_x(obj, n(c("a", "a")), 2L)
#>      a  a a  a
#> [1,] 1 13 1 13
#> [2,] 2 14 2 14
#> [3,] 3 15 3 15
#> [4,] 4 16 4 16
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

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
ii_x(obj, 1) # obj[1]
#> $a
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
ii_x(obj, 1:2) # obj[1:2]
#> $a
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
ii_x(obj, is.numeric) # obj[sapply(obj, is.numeric)]
#> $a
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $c
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
# for recursive subsets, see lst_rec()


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
ss_x(obj, n(1:3), 1:ndim(obj))
#>      a          b          c         
#> [1,] logical,3  logical,3  logical,3 
#> [2,] integer,10 integer,10 integer,10
#> [3,] numeric,10 numeric,10 numeric,10
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
ii_x(obj, i = is.numeric)
#> [[1]]
#>  [1]  3  1  8  6  7 10  2  5  9  4
#> 
#> [[2]]
#>  [1] -1.2176410  0.8419704 -1.6192106  0.2641659  0.0973107  1.7888654
#>  [7] -0.9886828  1.8909032  1.2008275 -1.8132110
#> 
#> [[3]]
#>  [1]  8  7  2  5 10  9  4  3  6  1
#> 
#> [[4]]
#>  [1]  0.37043223 -0.48267678  0.10128448  0.03907731  1.06482570 -3.70984947
#>  [7]  1.60017834  1.17966984 -2.65051546 -0.77477614
#> 
#> [[5]]
#>  [1]  9  2  7  5  3 10  4  6  1  8
#> 
#> [[6]]
#>  [1]  0.6058778 -0.3002806  1.0104924 -1.7693832  0.6722027 -0.8262573
#>  [7] -0.2918959 -0.7778171 -0.3485538  1.0117173
#> 
#> [[7]]
#>  [1]  1  9  8  2  7  3 10  6  5  4
#> 
#> [[8]]
#>  [1] -0.314772157 -0.005381841 -1.358302802  1.572927578 -1.199384161
#>  [6] -0.072655731 -0.024083976  1.584497498  0.005675627 -0.648204484
#> 
# above is equivalent to obj[sapply(obj, is.numeric)]
ss_x(obj, n(c("a", "a")), 2L)
#>      a            a            a            a           
#> [1,] logical,3    logical,3    logical,3    logical,3   
#> [2,] integer,10   integer,10   integer,10   integer,10  
#> [3,] numeric,10   numeric,10   numeric,10   numeric,10  
#> [4,] character,26 character,26 character,26 character,26
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
ss_x(obj, n(1:3, 1:2), c(1,3))
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] 1    5    9    13  
#> [2,] 2    6    10   14  
#> [3,] 3    7    11   15  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] 17   21   25   29  
#> [2,] 18   22   26   30  
#> [3,] 19   23   27   31  
#> 
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
ii_x(obj, i = \(x)x > 5)
#> [[1]]
#> [1] 6
#> 
#> [[2]]
#> [1] 7
#> 
#> [[3]]
#> [1] 8
#> 
#> [[4]]
#> [1] 9
#> 
#> [[5]]
#> [1] 10
#> 
#> [[6]]
#> [1] 11
#> 
#> [[7]]
#> [1] 12
#> 
#> [[8]]
#> [1] 13
#> 
#> [[9]]
#> [1] 14
#> 
#> [[10]]
#> [1] 15
#> 
#> [[11]]
#> [1] 16
#> 
#> [[12]]
#> [1] 17
#> 
#> [[13]]
#> [1] 18
#> 
#> [[14]]
#> [1] 19
#> 
#> [[15]]
#> [1] 20
#> 
#> [[16]]
#> [1] 21
#> 
#> [[17]]
#> [1] 22
#> 
#> [[18]]
#> [1] 23
#> 
#> [[19]]
#> [1] 24
#> 
#> [[20]]
#> [1] 25
#> 
#> [[21]]
#> [1] 26
#> 
#> [[22]]
#> [1] 27
#> 
#> [[23]]
#> [1] 28
#> 
#> [[24]]
#> [1] 29
#> 
#> [[25]]
#> [1] 30
#> 
#> [[26]]
#> [1] 31
#> 
#> [[27]]
#> [1] 32
#> 
#> [[28]]
#> [1] 33
#> 
#> [[29]]
#> [1] 34
#> 
#> [[30]]
#> [1] 35
#> 
#> [[31]]
#> [1] 36
#> 
#> [[32]]
#> [1] 37
#> 
#> [[33]]
#> [1] 38
#> 
#> [[34]]
#> [1] 39
#> 
#> [[35]]
#> [1] 40
#> 
#> [[36]]
#> [1] 41
#> 
#> [[37]]
#> [1] 42
#> 
#> [[38]]
#> [1] 43
#> 
#> [[39]]
#> [1] 44
#> 
#> [[40]]
#> [1] 45
#> 
#> [[41]]
#> [1] 46
#> 
#> [[42]]
#> [1] 47
#> 
#> [[43]]
#> [1] 48
#> 
# above is equivalent to obj[sapply(obj, \(x) x > 5)]

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
tt_x(obj, 1:3, 1:3)
#>   a b  c
#> 1 1 a 11
#> 2 2 b 12
#> 3 3 c 13
tt_x(obj, with(obj,  (a > 5) & (c < 19)), is.numeric)
#>   a  c
#> 1 6 16
#> 2 7 17
#> 3 8 18

```
