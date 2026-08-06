# Methods to Replace Subsets using R's Native Modification Semantics

Methods to replace subsets.  
Atomic objects are modified using R's native Modification semantics.  
Recursive objects are modified via a careful shallow (not deep) copy.  
  

## Usage

``` r
ii_mod(
  x,
  i = NULL,
  use = 1,
  ...,
  rp,
  tf,
  env = NULL,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

ss_mod(
  x,
  s = NULL,
  use = Inf,
  ...,
  rp,
  tf,
  env = NULL,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

tt_mod(
  x,
  row = NULL,
  col = NULL,
  use = 1:2,
  ...,
  rp,
  tf,
  env = NULL,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)
```

## Arguments

- x:

  see
  [squarebrackets_supported_structures](https://tony-aw.github.io/squarebrackets/reference/aaa03_squarebrackets_supported_structures.md).

- i, use, s, row, col:

  See
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).  
  An empty index selection returns the original object unchanged.  

- ...:

  see
  [squarebrackets_ellipsis](https://tony-aw.github.io/squarebrackets/reference/aaa07_squarebrackets_ellipsis.md).

- rp, tf, env:

  see
  [squarebrackets_modify](https://tony-aw.github.io/squarebrackets/reference/aaa05_squarebrackets_modify.md).

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)

## Value

Nothing. The object is modified in-place as-if running
`x[...] <- value`  
  

## Details

**Method Dispatch**  
Method dispatching is handled primarily through R's own `[<-` method
dispatch.  
The exception is data.frames, wh **Transform or Replace**  
Specifying argument `tf` will transform the subset.  
Specifying `rp` will replace the subset.  
One cannot specify both `tf` and `rp`. It's either one set or the
other.  
  

## Examples

``` r
# atomic objects ====

gen_mat <- function() {
  obj <- matrix(1:16, ncol = 4)
  colnames(obj) <- c("a", "b", "c", "a")
  return(obj)
}

obj <- obj2 <- gen_mat()
print(obj)
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16

ss_mod(obj, n(1:3), 1:ndim(obj), rp = -1:-9)
print(obj2)
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
# above is like x[1:3, 1:3] <- -1:-9

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16

ss_mod(obj, n("a"), 2L, rp = cbind(-1:-4, -5:-8))
print(obj2)
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
# above is like x[, "a"] <- cbind(-1:-4, -5:-8)

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16

ss_mod(obj, n(1:3), 1:ndim(obj), tf = \(x) -x)
print(obj2)
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
# above is like x[1:3, 1:3] <- -1 * x[1:3, 1:3]

obj <- obj2 <- gen_mat()
obj
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16

ss_mod(obj, "a", 2L, tf = \(x) -x)
obj2
#>      a b  c  a
#> [1,] 1 5  9 13
#> [2,] 2 6 10 14
#> [3,] 3 7 11 15
#> [4,] 4 8 12 16
# above is like x[, "a"] <- -1 * x[, "a"]


gen_array <- function() {
  as.mutatomic(array(1:64, c(4,4,3)))
}
obj <- obj2 <- gen_array()
obj
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
#> mutatomic 
#> typeof:  integer 

ss_mod(obj, n(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
print(obj2)
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
#> mutatomic 
#> typeof:  integer 
# above is like x[1:3, , 1:2] <- -1:-12



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
ii_mod(obj, "a", rp = list(1L))
print(obj)
#> $a
#> [1] 1
#> 
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $c
#>  [1] 11 12 13 14 15 16 17 18 19 20
#> 
# above is equivalent to  obj[["a"]] <- 1L; obj

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
ii_mod(obj, is.numeric, rp = list(-1:-10, -11:-20))
print(obj)
#> $a
#>  [1]  -1  -2  -3  -4  -5  -6  -7  -8  -9 -10
#> 
#> $b
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $c
#>  [1] -11 -12 -13 -14 -15 -16 -17 -18 -19 -20
#> 
# above is equivalent to  obj[which(sapply(obj, is.numeric))] <- list(-1:-10, -11:-20); obj

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
ss_mod(obj, n(1:3), 1:ndim(obj),rp = n(-1))
print(obj)
#>      a            b            c            a           
#> [1,] -1           -1           -1           logical,3   
#> [2,] -1           -1           -1           integer,10  
#> [3,] -1           -1           -1           numeric,10  
#> [4,] character,26 character,26 character,26 character,26
# above is equivalent to obj[1:3, 1:3] <- list(-1)
ii_mod(obj, is.numeric, rp = n(-1))
print(obj)
#>      a            b            c            a           
#> [1,] -1           -1           -1           logical,3   
#> [2,] -1           -1           -1           -1          
#> [3,] -1           -1           -1           -1          
#> [4,] character,26 character,26 character,26 character,26
# above is equivalent to obj[sapply(obj, is.numeric)] <- list(-1)
ss_mod(obj, n("a"), 2L, rp = n(-1))
print(obj)
#>      a  b            c            a 
#> [1,] -1 -1           -1           -1
#> [2,] -1 -1           -1           -1
#> [3,] -1 -1           -1           -1
#> [4,] -1 character,26 character,26 -1
# above is equivalent to
# obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()] <- list(-1)


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
ss_mod(obj, n(1:3, 1:2), c(1,3), rp = as.list(-1:-24))
print(obj)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] -1   -4   -7   -10 
#> [2,] -2   -5   -8   -11 
#> [3,] -3   -6   -9   -12 
#> [4,] 4    8    12   16  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] -13  -16  -19  -22 
#> [2,] -14  -17  -20  -23 
#> [3,] -15  -18  -21  -24 
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
# above is equivalent to obj[1:3, , 1:2] <- as.list(-1:-24)

obj <- array(as.list(1:64), c(4,4,3))
ii_mod(obj, i = \(x) x <= 5, rp = as.list(-1:-5))
print(obj)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,] -1   -5   9    13  
#> [2,] -2   6    10   14  
#> [3,] -3   7    11   15  
#> [4,] -4   8    12   16  
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
# above is equivalent to obj[sapply(onj, \(x) x <= 5)] <- as.list(-1:-5)


#############################################################################

# data.frame-like objects  - whole columns ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> 'data.frame':    10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
tt_mod(
  obj, col = is.numeric,
  tf = sqrt
)

#############################################################################

# data.frame-like objects  - partial columns ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> 'data.frame':    10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10

tt_mod(
  obj, with(obj,  (a > 2) & (c < 17)), is.numeric,
  tf = sqrt
) 
tt_mod(
  obj, with(obj,  (a > 2) & (c < 17)), is.numeric,
  tf = sqrt
) 
tt_mod(
  obj, with(obj,  (a > 2) & (c < 17)), is.numeric,
  tf = sqrt
) 



```
