# Value-based stride

`stride_v()` is used in the
[long\_](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods to specify sub-set operations based on values in an atomic
vector of properties.  
  
`stride_v()` can be used in the
[long\_](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods to perform value-based sub-setting operations.  
For a very basic understanding, consider the following illustration.  
In the simplest terms, the sub-set operation
`long_x(x, stride_v(y, v = v, na = na))`  
is conceptually equivalent to the following:  

    x[ifelse(is.na(y), na, y == v)] # if `na` is `TRUE` or `FALSE`
    x[is.na(y)] # if `na` is `NA`

## Usage

``` r
stride_v(y, ...)

# Default S3 method
stride_v(y, ..., v = NULL, na = FALSE, use = 1)
```

## Arguments

- y:

  a vector, with the same length as `x`, and ideally related to `x`  
  For example, `y` may be the character vector `names(x)`, the raw
  vector `broadcast::checkNA(x, "raw")`, the classless (i.e. raw data)
  values of `x`, or even the raw data values of another long vector with
  the same length as `x`.  
  Note that, in the default method, `couldb.mutatomic(y)` must be
  `TRUE`, otherwise an error is returned.

- ...:

  not supported: all arguments in `stride_v()` other than `y` must be
  explicitly named.

- v:

  a scalar or vector, depending on the type of `y`, indicating what
  values in `y` to look for.  
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
Let `x` and `y` be 2 atomic vectors of the same length (but they don't
have to be of the same type).  
Let `v` be some atomic scalar of the same type as `y`.  
Given the result of the condition `y == v`, the basic idea is to perform
the following sub-set operations:  

    long_x(x, stride_v(y, v = v))            # ==> x[y == v]
    long_set(x, stride_v(y, v = v), rp = rp) # ==> x[y == v] <- rp
    long_set(x, stride_v(y, v = v), tf = tf) # ==> x[y == v] <- tf(x[y == v])

The above is with the default argument specification `use = 1`.  
Of course one can invert the relationship by specifying argument
`use = -1`, to get something like the following:

    long_x(x, stride_v(y, v = v, use = -1))             # ==> x[p != v]
    long_set(x, stride_v(y, v = v, use = -1), rp = rp)  # ==> x[p != v] <- rp
    long_set(x, stride_v(y, v = v, use = -1), tf = tf)  # ==> x[p != v] <- tf(x[p != v])

And `y` is allowed to be the same vector as `x`, of course.  
  
This basic idea, however, can become more complicated, depending on the
atomic type of `y`, which is discussed in the next section.  
  
  

## Details per Atomic Type

**Logical, Raw, Complex**  
For `y` of type `logical`, `raw`, and `complex`, stride_v works exactly
as explained in the previous section.  
`y` and `v` must be of the same atomic type.  
  
  

**Numeric**  
For `y` of type `integer` or `double` (collectively referred to as
"numeric"), the basic idea laid-out before still holds:  
one can use atomic vector `y` and atomic scalar `v` to perform sub-set
operations like  
`x[y == v]`.  
  
But one may be more interested in a range of numbers, rather than one
specific number (especially considering things like measurement error,
and machine precision, and greater-than/larger-than relationships).  
So for numeric `y`, one can also supply `v` of length **2**.  
When `length(v) == 2L`, `long_` will check whether `y` is inside (or
outside if `use = -1`) the bounded range given by `v`.  
I.e. :

    y >= v[1] & y <= v[2]

Note that `y` and `v` must both be numeric here, but they don't have to
be the same type.  
I.e. one can have `y` of type `integer` and `v` of type `double`,
without problems.  
  
  

**Character**  
For `y` of type `character`, the basic idea is still to do something
like `x[y == v]`.  
  
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

    y %in% v

**NOTE**  
The order of `v` is **irrelevant**.  
  

## Smaller Than, Greater Than

For numeric `y`, one can specify a range for `v`, as explained
earlier.  
But note one can also specify something like `v = c(-Inf, 4)`, which
essentially corresponds to the condition `y <= 4`.  
Thus, when `v` specifies a range, "greater-than" and "smaller-than"
comparisons are also possible.  
  
  

## Handling NAs and NaN

We also have to handle the `NA`s and `NaN`s.  
The `na` argument can be used to specify what to do when a `y` is
`NA`.  
  
When `na = FALSE`, all `NA` values of `y` are always ignored.  
I.e. `long_x(x, stride_v(y, v = v, na = FALSE), use = 1)` will not
extract `NA`s/`NaN`s,  
and `long_x(x, stride_v(y, v = v, na = FALSE, use = -1))` will not
remove `NA`s/`NaN`s.  
  
When `na = TRUE`, `NA` values of `y` are always included.  
I.e. `long_x(x, stride_v(y, v = v, na = TRUE), use = 1)` will also
extract `NA`s/`NaN`s,  
and `long_x(x, stride_v(y, v = v, na = TRUE, use = -1))` will also
remove `NA`s/`NaN`s.  
  
One can also specify `na = NA`, which will ignore `v` completely, and
explicitly look for `NA`s/`NaN`s in `y` instead - like so:

    long_x(x, stride_v(y, na = NA))                        # ==> x[is.na(y)]
    long_x(x, stride_v(y, na = NA, use = -1))             # ==> x[!is.na(y)]
    long_set(x, stride_v(y, na = NA), rp = rp)             # ==> x[is.na(y)] <- rp
    long_set(x, stride_v(y, na = NA, use = -1), rp = rp)  # ==> x[!is.na(y)] <- rp
    long_set(x, stride_v(y, na = NA), tf = tf)             # ==> x[is.na(y)] <- tf(x[is.na(y)])
    long_set(x, stride_v(y, na = NA, use = -1), tf = tf)  # ==> x[!is.na(y)] <- tf(x[!is.na(y)])

Handling `NA`s/`NaN`s works the same for all atomic types.  
For `y` of type `complex`, a value `p[i]` is considered `NA` if
`Re(y[i])` is `NA`/`NaN` and/or `Im(y[i])` is `NA`/`NaN`.  
  
Argument `v` is never allowed to contain `NA`/`NaN`.  
  
  

## All in One

Combining all of the above, one can allocate indices in base 'R' to be
equivalent to the virtual indices produced by
`stride_v(y, v = v, na = na, use = use)`, with the following code:

    # if `na = NA`:
    ind <- which(is.na(y)) * sign(use)

    # else if using scalar `v`:
    ind <- which(ifelse(is.na(y), na, y == v)) * sign(use)

    # else if using numeric range for `v`:
    ind <- which(ifelse(is.na(y), na, y >= v[1] & y <= v[2])) * sign(use)

    # else if using character vector for `v`:
    ind <- which(ifelse(is.na(y), na, y %in% v)) * sign(use)

## Technical Details

On a technical level, the `stride_v()` method does the following:

1.  Determine if we go through vector `y` in one go, or in chunks.  
    If `lenght(y) >= 2^16`, vector `y` will be dealt with in
    `ceiling(length(x)^0.1)` equal-sized (so far as possible) chunks.  
    Otherwise, treat `y` in one go; i.e. in one chunk.

2.  Make a list equal to number of chunks from step one.

3.  For each chunk, count the number of condition matches (`count`), the
    location of the first ( `first`) match, and location of last
    (`last`) match.

4.  For each chunk, do the following.  
    If `count` is equal to `last - first + 1`, fill in list element for
    this chunk with `NULL`.  
    If `count <= 2`, fill in list element for this chunk with `NULL`.  
    Otherwise, fill list with `last - first + 1` bits (not bytes),
    specifying bit `1` if there's a match and bit `0` if no match.  
    The bits will be stored in 32 bit integers. Thus each integer holds
    32 elements worth of bits.  
      

In 'R', an expression like `x[y == v]` is internally translated to
`x[which(y == v)]`.  
This means, 'R' will store 32 bits per element for the logical vector
`y == v`, and 64 bits per element for the numeric vector from
`which(y == v)`.  
`stride_v()` stores information about the matches as 1 bit per condition
(instead of 32 bits per condition), and only for the regions (chunks)
where there's a need to store such data.  
And
[`long_x()`](https://tony-aw.github.io/squarebrackets/reference/long.md)/
[`long_set()`](https://tony-aw.github.io/squarebrackets/reference/long.md)
will never call [`which()`](https://rdrr.io/r/base/which.html).  
As such, `stride_v()` will **guarantee** to be **at least** 32 times
more memory efficient than the base 'R' approach.  
And the whole `long_x(x, stride_v(...))` / `long_set(x, stride_v(...))`
operation will in most practical cases use **hundreds of times** less
memory than the base 'R' approach!  

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


################################################################################
# Numeric range ====
#
x <- mutatomic(1:1e6)
head(x)
#> [1] 1 2 3 4 5 6
#> mutatomic 
#> typeof:  integer 
stride <- stride_v(x, v = c(-Inf, 5))
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
  stride <- stride_v(x, v = "hello")
  long_x(x, stride) |> head() # find "hello"
  
  # find 2 possible misspellings of "hello":
  stride <- stride_v(x, v = c("holle", "helol"))
  long_x(x, stride) |> head()
  
}
#> Loading required package: stringi
#> [1] "helol" "holle" "helol" "holle" "holle" "helol"



```
