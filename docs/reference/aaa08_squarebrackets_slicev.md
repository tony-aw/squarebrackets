# On Index-Less Value-Based Sub-Set Operations

This help page explains the details on the arguments used in the
[slicev](https://tony-aw.github.io/squarebrackets/reference/slicev.md)`_`
methods and the
[countv](https://tony-aw.github.io/squarebrackets/reference/slicev.md)
function.  
  

## The Basic Idea

The basic idea is as follows.  
Let `x` and `y` be 2 atomic vectors of the same length (but they don't
have to be of the same type).  
Let `v` be some atomic scalar of the same type as `y`.  
Given the result `r` of the condition `y == v`, the basic idea is to
perform the following sub-set operations:  

    slicev_x(x, y = y, v = v)            # ==> x[y == v]
    slicev_set(x, y = y, v = v, rp = rp) # ==> x[y == v] <- rp
    slicev_set(x, y = y, v = v, tf = tf) # ==> x[y == v] <- tf(x[y == v])
    countv(y,v = v)                      # ==> sum(y == v)

The above is with the default argument specification `r = TRUE`.  
Of course one can invert the relationship by specifying argument
`r = FALSE`, to get something like the following:

    slicev_x(x, y = y, v = v, r = FALSE)             # ==> x[y != v]
    slicev_set(x, y = y, v = v, r = FALSE, rp = rp)  # ==> x[y != v] <- rp
    slicev_set(x, y = y, v = v, r = FALSE, tf = tf)  # ==> x[y != v] <- tf(x[y != v])
    countv(y, v = v, r = FALSE)                  # ==> sum(y != v)

And `y` is allowed to be the same vector as `x`, of course.  
  
This basic idea, however, can become more complicated, depending on the
atomic type of `y`, which is discussed in the next section.  
  
  

## Details per Atomic Type

**Logical, Raw, Complex**  
For `y` of type `logical`, `raw`, and `complex`,
[slicev](https://tony-aw.github.io/squarebrackets/reference/slicev.md)
works exactly as explained in the previous section.  
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
When `length(v) == 2L`, `slicev_`/ `countv` will check whether `y` is
inside (or outside if `r = FALSE`) the bounded range given by `v`.  
I.e. :

    y >= v[1] & y <= v[2]  # if r = TRUE
    y < v[1] | y > v[2]    # if r = FALSE

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

    y %in% v   # if r = TRUE
    !y %in% v  # if r = FALSE

**NOTE**  
The order of `v` is **irrelevant**.  
  

## Factors

Technically, a factor has the type of `integer`, but it has special
behaviour to the extend that it is treated differently in 'R'.  
It is similarly treated by the `slicev_`/ `countv_` methods and
functions.  
  
When `y` is a factor, `v` can be given as:

- a single string (matching one of the levels of `y`);

- a single integer (matching one of the unique values of `unclass(y)`);

- a factor of length 1, with the same levels and level-ordering as
  `y`.  

Note that factors with `NA` levels are not supported, and passing such a
factor to `y` will result in an error.  
  

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
So these are not extracted
([slicev_x](https://tony-aw.github.io/squarebrackets/reference/slicev.md)),
replaced
([slicev_set](https://tony-aw.github.io/squarebrackets/reference/slicev.md)),
or counted
([countv](https://tony-aw.github.io/squarebrackets/reference/slicev.md)).  
  
When `na = TRUE`, `NA` values of `y` are always included.  
So these will be included in the extractions
([slicev_x](https://tony-aw.github.io/squarebrackets/reference/slicev.md)),
replacements
([slicev_set](https://tony-aw.github.io/squarebrackets/reference/slicev.md)),
and counts
([countv](https://tony-aw.github.io/squarebrackets/reference/slicev.md)).  
  
One can also specify `na = NA`, which will ignore `v` completely, and
explicitly look for `NA`s/`NaN`s in `y` instead - like so:

    slicev_x(x, y = y, na = NA)                        # ==> x[is.na(y)]
    slicev_x(x, y = y, na = NA, r = FALSE)             # ==> x[!is.na(y)]
    slicev_set(x, y = y, na = NA, rp = rp)             # ==> x[is.na(y)] <- rp
    slicev_set(x, y = y, na = NA, r = FALSE, rp = rp)  # ==> x[!is.na(y)] <- rp
    slicev_set(x, y = y, na = NA, tf = tf)             # ==> x[is.na(y)] <- tf(x[is.na(y)])
    slicev_set(x, y = y, na = NA, r = FALSE, tf = tf)  # ==> x[!is.na(y)] <- tf(x[!is.na(y)])
    countv(y, na = NA)                                 # ==> sum(is.na(y))
    countv(y, na = NA, r = FALSE)                      # ==> sum(!is.na(y))

Handling `NA`s works the same for all atomic types.  
For `y` of type `complex`, a value `y[i]` is considered `NA` if
`Re(y[i])` is `NA`/`NaN` and/or `Im(y[i])` is `NA`/`NaN`.  
  
Argument `v` is never allowed to contain `NA`/`NaN`.  
  
  

## Inverting

[`countv()`](https://tony-aw.github.io/squarebrackets/reference/slicev.md)
and
[`slicev_set()`](https://tony-aw.github.io/squarebrackets/reference/slicev.md)
do not have an "invert" argument, and likewise there is no `slicev_wo()`
function.  
Inverting the sub-set operation comes down to inverting the sub-set
condion, which is done by specifying `r = FALSE`.  
  
  

## Ellipsis

The ellipsis (`...`) is intentionally placed right after the first
argument (`x` in `slicev_` and `y` in `countv`) to force the user to
explicitly name all arguments, as doing so will avoid a lot of
unnecessary confusion.  
  

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

# memory efficient form of sum(x <= 10):
countv(x, v = c(-Inf, 10))
#> [1] 10

# extract all elements of x with the name "a":
slicev_x(x, y = names(x), v = "a") |> head()
#>   a   a   a   a   a   a 
#>   1  77 153 229 305 381 
#> mutatomic 
#> typeof:  integer 

# find all x smaller than or equal to 5, and replace with `-1000`:
slicev_set(x, y = x, v = c(-Inf, 5), rp = -1000L)
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
slicev_x(x, v= c(-Inf, 5)) # x[x <= 5]
#> [1] 1 2 3 4 5
#> mutatomic 
#> typeof:  integer 


################################################################################
# Character ====
#
if(require(stringi)) {
  x <- stringi::stri_rand_shuffle(rep("hello", 1e5))
  head(x)
  slicev_x(x, v = "hello") |> head() # find "hello"
  
  # find 2 possible misspellings of "hello":
  slicev_x(x, v = c("holle", "helol")) |> head()
  
}
#> Loading required package: stringi
#> [1] "holle" "helol" "holle" "holle" "helol" "helol"



```
