# Evaluate Stride Object

`eval_stride()` evaluates a stride or formula object.  
This can be handy to check exactly what parameters will be fed to the
`long_` methods.  
  
`formula2stride()` converts a formula to a stride_seq or stride_ptrn
object.

## Usage

``` r
eval_stride(stride, x)

# Default S3 method
eval_stride(stride, x)

is.stride(x)

formula2stride(form, x)
```

## Arguments

- stride:

  see
  [squarebrackets_stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md).

- x:

  a (long) atomic vector.

- form:

  a formula, as described in
  [squarebrackets_stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md).

## Value

**Using
[`stride_v()`](https://tony-aw.github.io/squarebrackets/reference/stride_v.md)**  
The original stride object, but as a list.  
  
  
**Using
[`stride_seq()`](https://tony-aw.github.io/squarebrackets/reference/stride_seq.md)
or `stride_ptr()`**  
A list with at least the following elements:  
  
**`start`**:  
The actual starting point of the sequence.  
This is simply `from` translated to regular numeric.  
  
**`end`**:  
The **actual** ending point of the sequence.  
This is not the same as `to`.  
For example, the following code:

    seq(from = 1L, to = 10L, by = 2L)
    #> [1] 1 3 5 7 9

specifies `to = 10L`.  
But the sequence doesn't actually end at `10`; it ends at `9`.  
Therefore, `stride_seq(x, 1, 10, 2) |> eval_stride()` will return
`end = 9`, not `end = 10`.  
This allows the user to easily predict where an sequence given in
[stride_seq](https://tony-aw.github.io/squarebrackets/reference/stride_seq.md)/[stride_ptrn](https://tony-aw.github.io/squarebrackets/reference/stride_ptrn.md)
will actually end.  
  
**`len`**:  
The actual vector length the sequence would be, given the translated
parameters.  
  

## Examples

``` r
# extract all elements of x with the name "a":
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic(1:1e6, names = nms)
head(x)
#> a b c d e f 
#> 1 2 3 4 5 6 
#> mutatomic 
#> typeof:  integer 
stride <-  stride_v(names(x), v = "a")
long_x(x, stride) |> head()
#>   a   a   a   a   a   a 
#>   1  77 153 229 305 381 
#> mutatomic 
#> typeof:  integer 


# find all x smaller than or equal to 5, and replace with `-1000`:
stride <- stride_v(x, v = c(-Inf, 5))
long_set(x, stride, rp = -1000L)
head(x, n = 10)
#>     a     b     c     d     e     f     g     h     i     j 
#> -1000 -1000 -1000 -1000 -1000     6     7     8     9    10 
#> mutatomic 
#> typeof:  integer 




x <- mutatomic(1:1e7)

# extract elements 2 to 9
long_x(x, ~ 2:9:1:1)
#> [1] 2 3 4 5 6 7 8 9
#> mutatomic 
#> typeof:  integer 

# reverse:
long_x(x, ~ 9:2:1:1)
#> [1] 9 8 7 6 5 4 3 2
#> mutatomic 
#> typeof:  integer 

# remove all elements except the last 10:
long_x(x, ~ 1:(.N - 10):1:-1)
#>  [1]  9999991  9999992  9999993  9999994  9999995  9999996  9999997  9999998
#>  [9]  9999999 10000000
#> mutatomic 
#> typeof:  integer 

# replace every other element:
x <- mutatomic(1:1e7)
long_set(x, ~ 2:.N:2:1, rp = -1)
#> coercing replacement to integer
head(x)
#> [1]  1 -1  3 -1  5 -1
#> mutatomic 
#> typeof:  integer 

# replace all elements except the first element:
x <- mutatomic(1:1e7)
long_set(x, ~1:1:1:-1, rp = -1)
#> coercing replacement to integer
head(x)
#> [1]  1 -1 -1 -1 -1 -1
#> mutatomic 
#> typeof:  integer 


# extract pattern c(TRUE, FALSE, FALSE, TRUE) from first 20 elements:
x <- mutatomic(1:1e7)
long_x(x, ~ 1:20:c(TRUE, FALSE, FALSE, TRUE):1)
#>  [1]  1  4  5  8  9 12 13 16 17 20
#> mutatomic 
#> typeof:  integer 
```
