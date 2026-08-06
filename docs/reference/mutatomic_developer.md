# Developer Functions for the mutatomic Class

The `stopifnot_ma_safe2mutate()` function checks if an atomic object is
actually safe to mutate.  
The `.internal_set_ma()` function sets an object to class 'mutatomic' by
reference.  

## Usage

``` r
stopifnot_ma_safe2mutate(sym, envir, .abortcall)

address(x)

.internal_set_ma(x)
```

## Arguments

- sym:

  the symbol of the object; i.e. `substitute(x)`.

- envir:

  the environment where the object resides; i.e. `parent.frame(n = 1)`.

- .abortcall:

  environment where the error message is passed to.

- x:

  atomic object

## Value

Nothing. Only gives an error if the object is not safe to mutate.

## Examples

``` r

testfun1 <- function(x) {
  .internal_set_ma(x)
}


x <- sample(1:10)
is.mutatomic(x)
#> [1] FALSE

testfun1(x)
is.mutatomic(x)
#> [1] TRUE
print(x)
#>  [1]  9  1  6  8  2 10  3  7  4  5
#> mutatomic 
#> typeof:  integer 
```
