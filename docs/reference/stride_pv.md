# Value-based stride

`stride_pv()` is used in the
[long\_](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods to specify sub-set operations based on values in an atomic
vector of properties.  
  
`stride_pv()` can be used in the
[long\_](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods to perform some rather complex sub-setting operations;  
but for a very basic understanding, consider the following
illustration.  
In the simplest terms, the sub-set operation
`long_x(x, stride_pv(p, v, na))`  
is conceptually equivalent to the following:  

    x[ifelse(is.na(p), na, p == v)] # if `na` is `TRUE` or `FALSE`
    x[is.na(p)] # if `na` is `NA`

`countv()` is a helper function, that counts how often `v` appears in
`p`. The sections further below give more details.  

## Usage

``` r
stride_pv(p, v = NULL, na = FALSE)

countv(p, v = NULL, na = FALSE, use = 1)
```

## Arguments

- p:

  class-less atomic vector of properties, with the same length as `x`,
  and ideally related to `x`  
  For example, `p` may be the character vector `names(x)`, the raw
  vector `broadcast::checkNA(x, "raw")`, the classless (i.e. raw data)
  values of `x`, or even the raw data values of another long vector with
  the same length as `x`.  
  Note that `couldb.mutatomic(p)` must be `TRUE`, otherwise an error is
  returned.

- v:

  a scalar or vector, depending on the type of `p`, indicating what
  values in `p` to look for.  
  Details are given in the sections below.  

- na:

  `TRUE`, `FALSE`, or `NA`, indicating what to do with `NA`s/`NaN`s.  
  If `na = TRUE`, `NA`s/`NaN`s are included in the sub-set operation
  (i.e. `NA`s/`NaN`s are extracted, removed, replaced, etc.).  
  If `na = FALSE`, `NA`s/`NaN`s are excluded from the sub-set operation
  (i.e. `NA`s/`NaN`s are not extracted, not removed, not replaced,
  etc.).  
  If `na = NA`, `v` is ignored, and **only** `NA` values are searched
  for the sub-set operation.  
  See also the additional sections below.

- use:

  `1` to check for specified condition, `-1` to check for the negated
  condition (i.e. `!condition`).

## Value

An object of class "stride".

## The Basic Idea

The basic idea is as follows.  
Let `x` and `p` be 2 atomic vectors of the same length (but they don't
have to be of the same type).  
Let `v` be some atomic scalar of the same type as `p`.  
Given the result of the condition `y == v`, the basic idea is to perform
the following sub-set operations:  

    long_x(x, stride_pv(p, v))            # ==> x[p == v]
    long_set(x, stride_pv(p, v), rp = rp) # ==> x[p == v] <- rp
    long_set(x, stride_pv(p, v), tf = tf) # ==> x[p == v] <- tf(x[p == v])

The above is with the default argument specification `use = 1`.  
Of course one can invert the relationship by specifying argument
`use = -1`, to get something like the following:

    long_x(x, stride_pv(p, v), use = -1)             # ==> x[p != v]
    long_set(x, stride_pv(p, v), use = -1, rp = rp)  # ==> x[p != v] <- rp
    long_set(x, stride_pv(p, v), use = -1, tf = tf)  # ==> x[p != v] <- tf(x[p != v])

And `p` is allowed to be the same vector as `x`, of course.  
  
This basic idea, however, can become more complicated, depending on the
atomic type of `p`, which is discussed in the next section.  
  
  

## Details per Atomic Type

**Logical, Raw, Complex**  
For `p` of type `logical`, `raw`, and `complex`, stride_pv works exactly
as explained in the previous section.  
`p` and `v` must be of the same atomic type.  
  
  

**Numeric**  
For `p` of type `integer` or `double` (collectively referred to as
"numeric"), the basic idea laid-out before still holds:  
one can use atomic vector `p` and atomic scalar `v` to perform sub-set
operations like  
`x[p == v]`.  
  
But one may be more interested in a range of numbers, rather than one
specific number (especially considering things like measurement error,
and machine precision, and greater-than/larger-than relationships).  
So for numeric `p`, one can also supply `v` of length **2**.  
When `length(v) == 2L`, `long_` will check whether `p` is inside (or
outside if `use = -1`) the bounded range given by `v`.  
I.e. :

    p >= v[1] & p <= v[2]

Note that `p` and `v` must both be numeric here, but they don't have to
be the same type.  
I.e. one can have `p` of type `integer` and `v` of type `double`,
without problems.  
  
  

**Character**  
For `p` of type `character`, the basic idea is still to do something
like `x[p == v]`.  
  
When searching for string `v` for sub-setting purposes, one may want to
take into consideration things like different spelling, spacing, or even
encodings of the same string.  
Implementing every form of fuzzy matching or encoding matching is
computationally intensive, and also quite beyond the scope of this
package.  
Instead, the user may supply a character vector `v` of arbitrary length,
containing all the variations (in terms of spelling, spacing, encoding,
or whatever) of all the strings to look for.  
  
So if a vector is given for `v` (instead of a single string), the
following check is performed:

    p %in% v

**NOTE**  
The order of `v` is **irrelevant**.  
  

## Smaller Than, Greater Than

For numeric `p`, one can specify a range for `v`, as explained
earlier.  
But note one can also specify something like `v = c(-Inf, 4)`, which
essentially corresponds to the condition `y <= 4`.  
Thus, when `v` specifies a range, "greater-than" and "smaller-than"
comparisons are also possible.  
  
  

## Handling NAs and NaN

We also have to handle the `NA`s and `NaN`s.  
The `na` argument can be used to specify what to do when a `p` is
`NA`.  
  
When `na = FALSE`, all `NA` values of `p` are always ignored.  
I.e. `long_x(x, stride_pv(p, v, na = FALSE), use = 1)` will not extract
`NA`s/`NaN`s,  
and `long_x(x, stride_pv(p, v, na = FALSE), use = -1)` will not remove
`NA`s/`NaN`s.  
  
When `na = TRUE`, `NA` values of `p` are always included.  
I.e. `long_x(x, stride_pv(p, v, na = TRUE), use = 1)` will also extract
`NA`s/`NaN`s,  
and `long_x(x, stride_pv(p, v, na = TRUE), use = -1)` will also remove
`NA`s/`NaN`s.  
  
One can also specify `na = NA`, which will ignore `v` completely, and
explicitly look for `NA`s/`NaN`s in `p` instead - like so:

    long_x(x, stride_pv(y, na = NA))                        # ==> x[is.na(y)]
    long_x(x, stride_pv(y, na = NA), use = -1)             # ==> x[!is.na(y)]
    long_set(x, stride_pv(y, na = NA), rp = rp)             # ==> x[is.na(y)] <- rp
    long_set(x, stride_pv(y, na = NA), use = -1, rp = rp)  # ==> x[!is.na(y)] <- rp
    long_set(x, stride_pv(y, na = NA), tf = tf)             # ==> x[is.na(y)] <- tf(x[is.na(y)])
    long_set(x, stride_pv(y, na = NA), use = -1, tf = tf)  # ==> x[!is.na(y)] <- tf(x[!is.na(y)])

Handling `NA`s/`NaN`s works the same for all atomic types.  
For `p` of type `complex`, a value `p[i]` is considered `NA` if
`Re(p[i])` is `NA`/`NaN` and/or `Im(p[i])` is `NA`/`NaN`.  
  
Argument `v` is never allowed to contain `NA`/`NaN`.  
  
  

## All in One

Combining all of the above, one can allocate indices in base 'R' to be
equivalent to the virtual indices produced by
`stride_pv(p, v, na), use)`, with the following code:

    # if `na = NA`:
    ind <- which(is.na(p)) * sign(use)

    # else if using scalar `v`:
    ind <- which(ifelse(is.na(p), na, p == v)) * sign(use)

    # else if using numeric range for `v`:
    ind <- which(ifelse(is.na(p), na, p >= v[1] & p <= v[2])) * sign(use)

    # else if using character vector for `v`:
    ind <- which(ifelse(is.na(p), na, p %in% v)) * sign(use)

## See also

[squarebrackets_stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md)  

## Examples

``` r
# basic idea ====
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic(1:1e6, names = nms)
head(x)
#> a b c d e f 
#> 1 2 3 4 5 6 
#> mutatomic 
#> typeof:  integer 

# extract all elements of x with the name "a":
stride <-  stride_pv(names(x), v = "a")
long_x(x, stride) |> head()
#>   a   a   a   a   a   a 
#>   1  77 153 229 305 381 
#> mutatomic 
#> typeof:  integer 

# find all x smaller than or equal to 5, and replace with `-1000`:
stride <- stride_pv(x, v = c(-Inf, 5))
long_set(x, stride, rp = -1000L)
head(x, n = 10)
#>     a     b     c     d     e     f     g     h     i     j 
#> -1000 -1000 -1000 -1000 -1000     6     7     8     9    10 
#> mutatomic 
#> typeof:  integer 


################################################################################
# Numeric range ====
#
x <- mutatomic(1:1e6)
head(x)
#> [1] 1 2 3 4 5 6
#> mutatomic 
#> typeof:  integer 
stride <- stride_pv(x, c(-Inf, 5))
long_x(x, stride) # x[x <= 5]
#> [1] 1 2 3 4 5
#> mutatomic 
#> typeof:  integer 


################################################################################
# Character ====
#
if(require(stringi)) {
  x <- stringi::stri_rand_shuffle(rep("hello", 1e5))
  head(x)
  stride <- stride_pv(x, "hello")
  long_x(x, stride) |> head() # find "hello"
  
  # find 2 possible misspellings of "hello":
  stride <- stride_pv(x, c("holle", "helol"))
  long_x(x, stride) |> head()
  
}
#> Loading required package: stringi
#> [1] "helol" "holle" "helol" "holle" "holle" "helol"



```
