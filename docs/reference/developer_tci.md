# Type Cast Indices

These functions typecast indices to proper integer indices.  

## Usage

``` r
tci_atomic(
  indx,
  n,
  nms,
  use = 1,
  chkdup = FALSE,
  uniquely_named = FALSE,
  .abortcall = sys.call()
)

tci_bool(indx, n, use = 1L, .abortcall = sys.call())

tci_int(indx, n, use = 1L, chkdup = FALSE, .abortcall = sys.call())

tci_chr(
  indx,
  nms,
  use = 1L,
  chkdup = FALSE,
  uniquely_named = FALSE,
  .abortcall = sys.call()
)

tci_formula(form, m, n, nms, .abortcall)

tci_zerolen(n, use = 1L)
```

## Arguments

- indx:

  the indices to typecast

- n:

  the relevant size, when typecasting integer or logical indices.  
  Examples:

  - If the target is row indices, input nrow for `n`.

  - If the target is flat indices, input the length for `n`.

- nms:

  the relevant names, when typecasting character indices.  
  Examples:

  - If the target is row indices, input row names for `nms`.

  - If the target is flat indices, input flat names for `nms`.

- use:

  1 or -1, indicating how to use the indices.  
  See
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

- uniquely_named:

  Boolean, indicating if the user knows a-priori that the relevant names
  of `x` are unique.  
  If set to `TRUE`, speed may increase.  
  But specifying `TRUE` when the relevant names are not unique will
  result in incorrect output.

- .abortcall:

  environment where the error message is passed to.

- form:

  a formula; see
  [keywords](https://tony-aw.github.io/squarebrackets/reference/aaa11_squarebrackets_keywords.md).

- m:

  a number giving the margin; set to `0` for dimensionless vectors.

## Value

An integer vector of type-cast indices.

## Examples

``` r
x <- matrix(1:25, 5, 5)
colnames(x) <- c("a", "a", "b", "c", "d")
print(x)
#>      a  a  b  c  d
#> [1,] 1  6 11 16 21
#> [2,] 2  7 12 17 22
#> [3,] 3  8 13 18 23
#> [4,] 4  9 14 19 24
#> [5,] 5 10 15 20 25

bool <- sample(c(TRUE, FALSE), 5, TRUE)
int <- 1:4
chr <- c("a", "a")
tci_bool(bool, nrow(x))
#> [1] 2 3 4
tci_int(int, ncol(x), -1)
#> [1] 5
tci_chr(chr, colnames(x))
#> [1] 1 2 1 2

ci_ii(x, 1:10)
#>  [1]  1  2  3  4  5  6  7  8  9 10
ci_margin(x, 1:4, 2)
#> [1] 1 2 3 4
ci_ss(x, n(~ .bi(-1:-5), 1:4), 1:2)
#> [[1]]
#> [1] 5 4 3 2 1
#> 
#> [[2]]
#> [1] 1 2 3 4
#> 
```
