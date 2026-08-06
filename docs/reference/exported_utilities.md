# Exported Utilities

Exported utilities.  
Usually the user won't need these functions.  
  

## Usage

``` r
indx_x(i, x, xnames, xsize)

indx_wo(i, x, xnames, xsize)

.is.0(x)
```

## Arguments

- i:

  See
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).

- x:

  a vector, vector-like object, factor, data.frame, data.frame-like
  object, or a list.

- xnames:

  names or dimension names

- xsize:

  length or dimension size

## Value

The subsetted object.

## Examples

``` r
x <- 1:10
names(x) <- letters[1:10]
indx_x(1:5, x, names(x), length(x))
#> [1] 1 2 3 4 5
indx_wo(1:5, x, names(x), length(x))
#> [1]  6  7  8  9 10
```
