# Introduction to squarebrackets

``` r
library(squarebrackets)
#> Run `?squarebrackets::squarebrackets_help` to open the introduction help page of 'squarebrackets'.
set.seed(1L)
```

 

## Introduction

‘squarebrackets’ provides subset methods that may be more convenient
alternatives to the `[` and `[<-` operators, whilst maintaining similar
performance.

The goal of this Vignette is to present some problems in sub-setting
objects programmatically in ‘R’, and how the ‘squarebrackets’ package
solves these problems.

 

## Vectors: Improved Index Specification

‘squarebrackets’ provides a set of methods that work on both atomic and
recursive vectors:

- `ii_x` to extract subsets
- `ii_mod` to modify an object using R’s native semantics
- `ii_set` to modify an object by reference.

base ‘R’ supports specifying indices for sub-set operations through
logical, integer, and character vectors.  
‘squarebrackets’ enhances these capabilities, and adds more
possibilities.  
The following sub-sections show *some* of these capabilities; a more
exhaustive list of the possibilities can be found in the package
documentation.

 

### Specify Indices by Names

Base ‘R’ only selects the first matching names when selecting indices
through a character vector. ‘squarebrackets’ selects all matching names.

For example:

``` r
nms <- c("a", sample(letters[1:4], 9, replace = TRUE))
x <- sample(1:10)
names(x) <- nms
print(x) # `x` has multiple elements with the name "a"
#>  a  a  d  c  a  b  a  c  c  b 
#>  2  3  1  5  7 10  6  4  9  8

x["a"] # only selects only the first index with name "a"
#> a 
#> 2
ii_x(x, "a") # selects all indices with the name "a"
#> a a a a 
#> 2 3 7 6

x[c("a", "a")] # repeats only the first index with name "a"
#> a a 
#> 2 2
ii_x(x, c("a", "a")) # repeats all indices with the name "a"
#> a a a a a a a a 
#> 2 3 7 6 2 3 7 6
```

To select the indices `c("a", "a", "b")`, whilst ensuring **all**
indices with those names get selected, one needs to do the following in
base ‘R’:

``` r
x[lapply(c("a", "a", "b"), \(i)which(names(x) == i)) |> unlist()]
#>  a  a  a  a  a  a  a  a  b  b 
#>  2  3  7  6  2  3  7  6 10  8
```

See how much easier it is with ‘squarebrackets’:

``` r
ii_x(x, c("a", "a", "b"))
#>  a  a  a  a  a  a  a  a  b  b 
#>  2  3  7  6  2  3  7  6 10  8
```

Not only is the syntax shorter, the computation is also **faster**, as
‘squarebrackets’ does not rely on
[`lapply()`](https://rdrr.io/r/base/lapply.html) (or friends) to find
all matching names, but uses compiled ‘C’ code (partly from the
‘collapse’ package).

Moreover, ‘squarebrackets’ is safe in that attempting to select an
non-existing name gives an explicit error:

``` r
ii_x(x, c("xxx", "a"))
Error in ii_x.default(x, c("xxx", "a")) : unknown names given

x[c("xxx", "a")] # no error, but a silent NA added magically:
<NA>    a 
  NA    1 
```

 

### Inverting Index Specification

Inverting indices in base ‘R’ is done in different ways. (negative
numbers for numeric indexing, negation for logical indexing, manually
un-matching for character vectors).

‘squarebrackets’ provides a (somewhat) consistent syntax to invert
indices, namely through the `use` argument. Setting `use` to a negative
value will invert the indices.

As a consequence, removing sub-sets has the same syntax as extracting
indices.

For example:

``` r
x <- sample(1:10)
names(x) <- letters[1:10]

x["a"] # extract element "a" in base R
#> a 
#> 9
x[!names(x) %in% "a"] # but removing has different syntax
#>  b  c  d  e  f  g  h  i  j 
#>  5 10  1  7  8  6  2  3  4

ii_x(x, "a") # extract element "a" with 'squarebrackets'
#> a 
#> 9
ii_x(x, "a", -1) # extract all elements except "a", with 'squarebrackets'
#>  b  c  d  e  f  g  h  i  j 
#>  5 10  1  7  8  6  2  3  4
```

 

### Not Just Vectors

The given enhanced indexing is not just available for regular vectors,
but for all types supported by ‘squarebrackets’.

 

## Arrays: sub-setting unknown number of dimensions

### Basics

In order to perform subset operations on some array `x` with the square
brackets operator (`[`, `[<-`), one needs to know how many dimensions it
has. I.e. `x[i, j, k]` for a 3D array and `x[i, j, k, l]` for a 4D
array. Using `x[i, j, k]` on an array with 4 dimensions produces an
error, since the number of indices or empty arguments does not conform
to the number of dimensions.

Suppose the number of dimensions of an array is not known a-priori (for
example when looping through multiple arrays with different dimensions).
How would one the use the `[` and `[<-` operators in such a situation?
It’s not strictly impossible, but it is very convoluted.

‘squarebrackets’ provides a set of methods for atomic and recursive
arrays, which require no prior knowledge on the number of dimensions:

- `ss_x` to extract subsets
- `ss_mod` to modify an object using R’s native semantics
- `ss_set` to modify an object by reference.

These methods use the `s, use` argument pair to specify indices for
subset operations.

`s` and `use` must be specified as follows:

- The `s` argument must be a list, specifying the subscripts
  (i.e. dimensional indices).
- The `use` argument must be an integer vector, specifying the
  dimensions for which `s` holds. Negative integers will invert indices
  (i.e. select all indices for that dimension EXCEPT the specified
  ones). By default, `use = 1:length(dim(x))`.
- If the subscripts are the same for all dimensions specified in `use`,
  `s` can also be given as an atomic vector, or as a list of length 1.

Examples where `s` can be given as a simple atomic vector:

``` r
x <- array(1:27, c(5,4,3), dimnames = list(NULL, letters[1:4], NULL)

# specify rows 1:3:
ss_x(x, 1:3, 1L) # s = 1:3, use = 1L

# specify columns "a" and "b":
ss_x(x, c("a", "b"), 2L) # s = c("a", "b"), use = 2L

# specify subscripts 1:2 of all dimensions:
ss_x(x, 1:2) # s = 1:2, use = 1:ndim(x)

# remove columns 1:2:
ss_x(x, 1:2, -2L)
```

To minimize keystrokes, ‘squarebrackets’ provides the
[`n()`](https://tony-aw.github.io/squarebrackets/reference/nest.md)
function, which is short-hand for
[`list()`](https://rdrr.io/r/base/list.html);
[`n()`](https://tony-aw.github.io/squarebrackets/reference/nest.md)
**nests** multiple objects together, just like
[`c()`](https://rdrr.io/r/base/c.html) concatenates multiple objects
together. Examples where `s` is given as a list (using
[`n()`](https://tony-aw.github.io/squarebrackets/reference/nest.md)):

``` r
x <- array(1:27, c(5,4,3), dimnames = list(NULL, letters[1:4], NULL))

# select rows 1:3 and *remove* columns c("a", "b"):
ss_x(x, n(1:3, c("a", "b")), c(1, -2)) # s = n(1:3, c("a", "b")), use = c(1, -2)
```

 

### Advanced Indexing

Consider the following array:

``` r
x <- array(1:(prod(5:3)), 5:3, list(letters[1:5], LETTERS[1:4], month.abb[1:3]))
print(x)
#> , , Jan
#> 
#>   A  B  C  D
#> a 1  6 11 16
#> b 2  7 12 17
#> c 3  8 13 18
#> d 4  9 14 19
#> e 5 10 15 20
#> 
#> , , Feb
#> 
#>    A  B  C  D
#> a 21 26 31 36
#> b 22 27 32 37
#> c 23 28 33 38
#> d 24 29 34 39
#> e 25 30 35 40
#> 
#> , , Mar
#> 
#>    A  B  C  D
#> a 41 46 51 56
#> b 42 47 52 57
#> c 43 48 53 58
#> d 44 49 54 59
#> e 45 50 55 60
```

Extracting the first 2 elements of each dimension of this array is
relatively easy in base ‘R’:

``` r
x[1:2, 1:2, 1:2]
#> , , Jan
#> 
#>   A B
#> a 1 6
#> b 2 7
#> 
#> , , Feb
#> 
#>    A  B
#> a 21 26
#> b 22 27
```

But suppose you wish to extract the **last** 2 elements of each
dimension. In base ‘R’, you would have to do something like this:

``` r
x[c(dim(x)[1] - 1, dim(x)[1]), c(dim(x)[2] - 1, dim(x)[2]), c(dim(x)[3] - 1, dim(x)[3])]
#> , , Feb
#> 
#>    C  D
#> d 34 39
#> e 35 40
#> 
#> , , Mar
#> 
#>    C  D
#> d 54 59
#> e 55 60
```

‘squarebrackets’ allows indexing by **keywords** via a formula, which
allows one to do more advanced sub-setting operations. We can do the
above operations using keywords in a few ways:

``` r
ss_x(x, ~ (.N-1):.N)
#> , , Feb
#> 
#>    C  D
#> d 34 39
#> e 35 40
#> 
#> , , Mar
#> 
#>    C  D
#> d 54 59
#> e 55 60

ss_x(x, ~ .bi(-2:-1))
#> , , Feb
#> 
#>    C  D
#> d 34 39
#> e 35 40
#> 
#> , , Mar
#> 
#>    C  D
#> d 54 59
#> e 55 60
```

‘squarebrackets’ allows users to specify indices by using keywords in a
formula, like just shown; the following keywords are available:

- `.M`: the given margin/dimension; 0 if not relevant.
- `.Nms`: the (dim)names at the given margin.
- `.N`: the size of a given dimension (if `.M` is not 0) or else the
  length of `x`.
- `.I`: equal to `seq_len(.N)`.
- `.bi(...)`: a function to specify bilateral indices.
- `.ptrn()`: a function to specify a recurring pattern.

 

Let’s use keywords to select all sub-sets whose dimnames contains a “a”,
“A”, “e” or “E”, and compare it to how to do it in base ‘R’:

``` r
library(stringi)

p <- "a|A|e|E"
# in base R:
x[
  stri_detect(dimnames(x)[[1]], regex = p),
  stri_detect(dimnames(x)[[2]], regex = p),
  stri_detect(dimnames(x)[[3]], regex = p),
  drop = FALSE
]
#> , , Jan
#> 
#>   A
#> a 1
#> e 5
#> 
#> , , Feb
#> 
#>    A
#> a 21
#> e 25
#> 
#> , , Mar
#> 
#>    A
#> a 41
#> e 45

# using 'squarebrackets':
ss_x(x, ~ stri_detect(.Nms, regex = p))
#> , , Jan
#> 
#>   A
#> a 1
#> e 5
#> 
#> , , Feb
#> 
#>    A
#> a 21
#> e 25
#> 
#> , , Mar
#> 
#>    A
#> a 41
#> e 45
```

Keywords are available for vectors, arrays, and also data.frame-like
objects.

 

## Data.frame: different types, different rules

There are several types of data.frame-like objects available in ‘R’:
data.frames, data.tables, tibbles, tidytables; and they all have their
own rules regarding sub-set operations.

Consider the following example, where values of the column “a” are being
replaced with “XXX”, but only in the rows for which holds that column
“b” is larger than 10:

``` r
tinycodet::import_as(~ dpr., "dplyr", dependencies = "tibble")

x <- data.frame(a = month.abb, b = 1:12)
y <- dpr.$tibble(a = month.abb, b = 1:12)
z <- data.table::data.table(a = month.abb, b = 1:12)

x[with(x, b > 10), "a"] <- "XXX" # data.frame with base
y <- dpr.$mutate(y, a = ifelse(b > 10, "XXX", b)) # tibble with tidyverse
z[b > 10, a := "XXX"] # data.table with fastverse/tinyverse
```

Note that the syntax is different for each type of data.frame.  
‘squarebrackets’ provides a set of methods that work consistently on all
manner of tabular (data.frames and matrix) types, with the exact same
syntax:

- `tt_x` to extract subsets
- `tt_mod` to modify an object using R’s native semantics
- `tt_set` to modify an object by reference.

So let’s do the same operation as above, but now using ‘squarebrackets’:

``` r

x <- data.frame(a = month.abb, b = 1:12)
y <- tibble::tibble(a = month.abb, b = 1:12)
z <- data.table::data.table(a = month.abb, b = 1:12)

tt_mod(x, ~~ b > 10, "a", rp = "XXX")
tt_mod(y, ~~ b > 10, "a", rp = "XXX")
tt_mod(z, ~~ b > 10, "a", rp = "XXX")

print(z)
#>          a     b
#>     <char> <int>
#>  1:    Jan     1
#>  2:    Feb     2
#>  3:    Mar     3
#>  4:    Apr     4
#>  5:    May     5
#>  6:    Jun     6
#>  7:    Jul     7
#>  8:    Aug     8
#>  9:    Sep     9
#> 10:    Oct    10
#> 11:    XXX    11
#> 12:    XXX    12
```

Notice that the syntax is exactly the same for all classes.

The original attributes are also preserved when using
[`tt_mod()`](https://tony-aw.github.io/squarebrackets/reference/generic_mod.md);
i.e. nothing is forced to become a tibble, data.table, or something
else. Input class = output class.

For data.tables specifically, the user can also use
[`tt_set()`](https://tony-aw.github.io/squarebrackets/reference/generic_set.md),
to perform pass-by-reference semantics, which is considerably faster and
more memory efficient:

``` r

z <- data.table::data.table(a = month.abb, b = 1:12)
tt_set(z, ~~ b > 10, "a", rp = "XXX")
print(z)
#>          a     b
#>     <char> <int>
#>  1:    Jan     1
#>  2:    Feb     2
#>  3:    Mar     3
#>  4:    Apr     4
#>  5:    May     5
#>  6:    Jun     6
#>  7:    Jul     7
#>  8:    Aug     8
#>  9:    Sep     9
#> 10:    Oct    10
#> 11:    XXX    11
#> 12:    XXX    12
```

This is all powered by the class-agnostic ‘C’ code from the fantastic
‘collapse’ and ‘data.table’ packages.

 

## Programmatically aggregate data.table

‘squarebrackets’ provides functions like
[`dt_aggr()`](https://tony-aw.github.io/squarebrackets/reference/dt.md)
to aggregate a `data.table` programmatically:

``` r
d <- data.table::as.data.table(iris)

fun <- rep(list(mean = mean, var = var), 2L)
col <- rep(c("Sepal.Length", "Sepal.Width"), each = 2L)

# explictly naming every argument for the reader's convenience:
dt_aggr(d, row = 1:5, col = col, use = c(-1, 2), fun = fun, by = "Species")
#>       Species mean(Sepal.Length) var(Sepal.Length) mean(Sepal.Width)
#>        <fctr>              <num>             <num>             <num>
#> 1:     setosa           5.022222         0.1317677          3.444444
#> 2: versicolor           5.936000         0.2664327          2.770000
#> 3:  virginica           6.588000         0.4043429          2.974000
#>    var(Sepal.Width)
#>               <num>
#> 1:       0.15116162
#> 2:       0.09846939
#> 3:       0.10400408

# realistically shorter call:
dt_aggr(d, 1:5, col, c(-1, 2), fun, "Species")
#>       Species mean(Sepal.Length) var(Sepal.Length) mean(Sepal.Width)
#>        <fctr>              <num>             <num>             <num>
#> 1:     setosa           5.022222         0.1317677          3.444444
#> 2: versicolor           5.936000         0.2664327          2.770000
#> 3:  virginica           6.588000         0.4043429          2.974000
#>    var(Sepal.Width)
#>               <num>
#> 1:       0.15116162
#> 2:       0.09846939
#> 3:       0.10400408
```

The above is the same as the following pure data.table operation:

``` r
d[-1:-5,
  .(`mean(Sepal.Length)` = mean(Sepal.Length),
    `var(Sepal.Length)` = var(Sepal.Length),
    `mean(Sepal.Width)` = mean(Sepal.Width),
    `var(Sepal.Width)` = var(Sepal.Width)
  ),
  by = c("Species"), keyby = FALSE]
#>       Species mean(Sepal.Length) var(Sepal.Length) mean(Sepal.Width)
#>        <fctr>              <num>             <num>             <num>
#> 1:     setosa           5.022222         0.1317677          3.444444
#> 2: versicolor           5.936000         0.2664327          2.770000
#> 3:  virginica           6.588000         0.4043429          2.974000
#>    var(Sepal.Width)
#>               <num>
#> 1:       0.15116162
#> 2:       0.09846939
#> 3:       0.10400408
```

though
[`dt_aggr()`](https://tony-aw.github.io/squarebrackets/reference/dt.md)
is much more programmatically friendly, and is actually short to write.

Here’s an example using an `sf-data.table`, computing the union of all
geometries, per region:

``` r

x <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#> Reading layer `nc' from data source 
#>   `D:\Programs\R\R-4.6.1\library\sf\shape\nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
x <- data.table::as.data.table(x)

x$region <- ifelse(x$CNTY_ID <= 2000, 'high', 'low')
d.aggr <- dt_aggr(
  x, 0L, "geometry", fun = list(union = sf::st_union), by = "region"
)

head(d.aggr)
#>    region                union(geometry)
#>    <char>             <sfc_MULTIPOLYGON>
#> 1:   high MULTIPOLYGON (((-75.78317 3...
#> 2:    low MULTIPOLYGON (((-76.46926 3...
```

 

## Pass by Reference or Pass By Value?

R’s `[<-` and `[[<-` sometimes make a copy of an object, and sometimes
they perhaps don’t. This brings 2 issues:

- Making unnecessary copies wastes memory (and speed);
- On a technical level, it may be difficult to predict if a copy is made
  or not.

Data.tables from the ‘data.table’ package natively uses
pass-by-reference semantics, meaning no copy is made. Tibbles from the
‘tidyverse’ often returns a (very wasteful) copy.

‘squarebrackets’ provides the user the ability to explicitly **choose**
whether to modify an object by reference (like data.table), or to use
R’s native semantics. The `*_mod` methods use R’s native semantics. The
`*_set` methods modify an object by reference. The `*_set` methods are
only available for the mutable classes `data.table` and `mutatomic`;
`mutatomic` is a class of mutable atomic object provided by
‘squarebrackets’ for the explicit purpose of being able to modify atomic
objects by reference, and doing so **safely**.

 

## Long Vectors: So much memory usage

### Sub-set operations without indices

Long Vectors take in quite a bit of memory. Performing a sub-set
operation in base ‘R’ on a vector requires an indexing vector, which -
for a long vector - may itself also be a long vector. This is a lot of
memory usage. We can do better.

‘squarebrackets’ provides the
[`long_x()`](https://tony-aw.github.io/squarebrackets/reference/long.md)
and
[`long_set()`](https://tony-aw.github.io/squarebrackets/reference/long.md)
methods to perform sub-set operations on the interior of a vector,
without an explicit indexing vector. Instead of an indexing vector, they
use a `stride` object. There are 3 types of `stride` objects that can be
used:

- [`stride_v()`](https://tony-aw.github.io/squarebrackets/reference/stride_v.md):
  Use this `stride` type to specify subsets based on values, like
  `y == v`, where `y` is an atomic vector (for example `names(x)`), and
  `v` is a value (or range of values) `y` might contain.
- [`stride_seq()`](https://tony-aw.github.io/squarebrackets/reference/stride_seq.md):
  Use this `stride` type to specify a sequence in the form of
  `seq(from, to, by)`, without actually allocating a sequence indexing
  vector.
- [`stride_ptrn()`](https://tony-aw.github.io/squarebrackets/reference/stride_ptrn.md):
  Use this `stride` type to specify a patterned sequence in the form of
  `(start:end)[pattern]`, where `start` and `end` are natural scalars
  and `pattern` is a logical vector.
  [`stride_ptrn()`](https://tony-aw.github.io/squarebrackets/reference/stride_ptrn.md)
  specifies this sequence without actually allocating an indexing
  vector.

An example using
[`stride_v()`](https://tony-aw.github.io/squarebrackets/reference/stride_v.md):

``` r

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
```

An example using
[`stride_seq()`](https://tony-aw.github.io/squarebrackets/reference/stride_seq.md):

``` r
x <- 1:50
long_x(x, stride_seq(1, 10, 2)) # equivalent to x[seq(1, 10, 2)]
#> [1] 1 3 5 7 9

# the above can also be specified as a formula:
long_x(x, ~ 1:10:2:1)
#> [1] 1 3 5 7 9
```

An example using
[`stride_ptrn()`](https://tony-aw.github.io/squarebrackets/reference/stride_ptrn.md):

``` r
x <- 1:50
ptrn <- c(TRUE, FALSE, FALSE, TRUE)
long_x(x, stride_ptrn(1, 20, ptrn)) # equivalent to x[(1:20)[ptrn]]
#>  [1]  1  4  5  8  9 12 13 16 17 20

# the above can also be specified as a formula:
long_x(x, ~ 1:20:ptrn:1)
#>  [1]  1  4  5  8  9 12 13 16 17 20
```

Both extracting sub-sets and pass-by-reference modification of sub-sets,
is available for both methods.

 

### Sub-set Modifications without Copies

R’s `[<-` operator (sometimes) makes copies of objects; making copies of
long vectors, however, is an enormous waste of memory.

To reduce memory usage, ‘squarebrackets’ provides a class of mutable
atomic objects that can be modified **without** making copies, similar
to how the ‘data.table’ package works. This new class of mutable atomic
objects is called `mutatomic`, and can be created with ease:

``` r
x <- mutatomic(seq(1L, 1e6L, 2L))
head(x)
#> [1]  1  3  5  7  9 11
#> mutatomic 
#> typeof:  integer
```

We can modify this vector by reference using the various methods that
end with `_set`.

For example like so:

``` r
long_set(x, ~ 2:4:1:1, rp = -1L)
head(x)
#> [1]  1 -1 -1 -1  9 11
#> mutatomic 
#> typeof:  integer
```

You can still use regular indices, for example using
[`ii_set()`](https://tony-aw.github.io/squarebrackets/reference/generic_set.md):

``` r
ii_set(x, 1, rp = -1000L)
head(x)
#> [1] -1000    -1    -1    -1     9    11
#> mutatomic 
#> typeof:  integer
```

 

## Closing Remarks

If this introductory article has piqued your interest, I kindly invite
you to read the rest of the (admittedly rather extensive) documentation,
and perhaps try out the package for yourself.

 

 
