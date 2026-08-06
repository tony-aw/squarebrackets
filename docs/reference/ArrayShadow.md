# Array Shadow

The Array Shadow object is a "shadow" of an vector or array `x`:  
It has the same (dim)names as `x`, and pretends to have the same length
and dimensions.  
But it holds no actual data.  

## Usage

``` r
cast_ArrayShadow(type, length, dim, names, dimnames, oldClass)

cast_ArrayShadow2(x_expr, env)
```

## Arguments

- type:

  the type of the vector/array (see
  [atomic](https://rdrr.io/r/base/vector.html) and
  [list](https://rdrr.io/r/base/list.html)).

- length:

  the length of the vector/array.

- dim:

  the dimensions of the vector/array.

- names:

  the names of the vector/array.

- dimnames:

  the `dimnames` of the vector/array.

- oldClass:

  the class attribute of the vector/array.

- x_expr:

  the substituted name of the vector/array.

- env:

  the environment where the vector/array name (`x_expr`) actually
  lives.  

## Value

The "shadow" of the vector/array.

## Examples

``` r
x <- array(1:27, c(3,3,3))
cast_ArrayShadow(typeof(x), length(x), dim(x), names(x), dimnames(x), oldClass(x))
#> [1] 0
#> attr(,"shadow_len")
#> [1] 27
#> attr(,"shadow_dim")
#> [1] 3 3 3
#> attr(,"class")
#> [1] "ArrayShadow"
```
