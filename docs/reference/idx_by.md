# Compute Grouped Indices

Given:

- a sub-set function `f`;

- an object `x` with its margin `m`;

- and a grouping factor `grp`;

the `idx_by()` function takes `indices` **per group** `grp`.  
The result of `idx_by()` can be supplied to the indexing arguments (see
[squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md))
to perform **grouped** subset operations.  
  

## Usage

``` r
idx_by(x, m, f, grp, parallel = FALSE, mc.cores = 1L)
```

## Arguments

- x:

  the object from which to compute the indices.

- m:

  a single non-negative integer giving the margin for which to compute
  indices.  
  For flat indices or for non-dimensional objects, use `m = 0L`.  

- f:

  a subset function to be applied per group on `indices`.  
  If `m == 0L`, `indices` is here defined as
  `setNames(1:length(x), names(x))`.  
  If `m > 0L`, `indices` is here defined as
  `setNames(1:dim(x)[m], dimnames(x)[[m]])`.  
  The function must produce a character or integer vector as output.  
  For example, to subset the last element per group, specify:  
  `f = last`

- grp:

  a factor giving the groups.

- parallel, mc.cores:

  see [BY](https://fastverse.org/collapse/reference/BY.html).

## Value

A vector of indices.

## Examples

``` r

# vectors ====
(a <- 1:20)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
(grp <- factor(rep(letters[1:5], each = 4)))
#>  [1] a a a a b b b b c c c c d d d d e e e e
#> Levels: a b c d e

# get the last element of `a` for each group in `grp`:
s <- list(idx_by(a, 0L, last, grp))
ss_x(cbind(a, grp), s, 1L)
#>       a grp
#> [1,]  4   1
#> [2,]  8   2
#> [3,] 12   3
#> [4,] 16   4
#> [5,] 20   5


# data.frame ====
x <- data.frame(
  a = sample(1:20),
  b = letters[1:20],
  group = factor(rep(letters[1:5], each = 4))
)
print(x)
#>     a b group
#> 1   1 a     a
#> 2   8 b     a
#> 3   5 c     a
#> 4   9 d     a
#> 5  10 e     b
#> 6  17 f     b
#> 7  20 g     b
#> 8  19 h     b
#> 9   6 i     c
#> 10 16 j     c
#> 11 12 k     c
#> 12 13 l     c
#> 13  3 m     d
#> 14 18 n     d
#> 15 14 o     d
#> 16  7 p     d
#> 17  2 q     e
#> 18 11 r     e
#> 19  4 s     e
#> 20 15 t     e
# get the first row for each group in data.frame `x`:
row <- idx_by(x, 1, first, x$group)
tt_x(x, row)
#>    a b group
#> 1  1 a     a
#> 2 10 e     b
#> 3  6 i     c
#> 4  3 m     d
#> 5  2 q     e
# get the first row for each group for which a > 10:
x2 <- tt_x(x, with(x,  a > 10))
row <- na.omit(idx_by(x2, 1, first, x2$group))
tt_x(x2, row)
#>    a b group
#> 1 17 f     b
#> 2 16 j     c
#> 3 18 n     d
#> 4 11 r     e
 
```
