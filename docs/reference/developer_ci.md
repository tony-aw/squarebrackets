# Construct Indices

These functions construct indices.  

- `ci_ii()` constructs an integer vector flat/interior indices.

- `ci_margin()` constructs an integer vector of indices for one
  particular dimension margin.

- `ci_ss()` constructs a list of integer subscripts.

- `ci_df()` constructs a list of row- and column indices for data.frames
  specifically.

## Usage

``` r
ci_ii(
  x,
  i = NULL,
  use = 1L,
  chkdup = FALSE,
  uniquely_named = FALSE,
  .abortcall = sys.call()
)

ci_margin(
  x,
  slice = NULL,
  margin,
  use = 1L,
  chkdup = FALSE,
  uniquely_named = FALSE,
  .abortcall = sys.call()
)

ci_ss(
  x,
  s = NULL,
  use = Inf,
  chkdup = FALSE,
  uniquely_named = FALSE,
  .abortcall = sys.call()
)

ci_df(x, row, col, use = 1:2, chkdup = FALSE, .abortcall)
```

## Arguments

- x:

  the object for which the indices are meant.

- i, s, row, col, slice, margin, use:

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

## Value

An integer vector of constructed indices.

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
#> [1] 1 2 4 5
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
