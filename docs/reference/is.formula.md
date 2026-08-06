# Check if an Object is a Formula

`is.formula()` checks if an object is a formula.  
  

## Usage

``` r
is.formula(form)
```

## Arguments

- form:

  object to check

## Value

The `is_formula()` function returns `TRUE` if the input is a formula,
and `FALSE` otherwise.  
  

## Examples

``` r
is.formula(~ x)
#> [1] TRUE
is.formula(1:10)
#> [1] FALSE
```
