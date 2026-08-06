# Index Arguments in the Generic Sub-setting Methods

There are several types of arguments that can be used in the generic
methods of 'squarebrackets' to specify the indices to perform operations
on:

- `i, use`: to specify interior (i.e. dimensionless) indices.

- `s, use`: to specify subscripts of arbitrary dimensions in arrays.

- `row, col, use`: to specify rows and columns in tabular objects
  (matrices and data.frames).  

- `slice, use`: to specify indices of one particular dimension (for
  arrays and data.frame-like objects).  
  Currently only used in the `_icom` methods.  
    

For the fundamentals of indexing in 'squarebrackets', see
[squarebrackets_index_fundamentals](https://tony-aw.github.io/squarebrackets/reference/aaa02_squarebrackets_index_fundamentals.md).  
In this help page `x` refers to the object on which subset operations
are performed.  
  
  

## Argument Pair `i, use`

![\[class: atomic vector\]](figures/class-atomic_vector-blue.svg)  
![\[class: recursive
vector\]](figures/class-recursive_vector-blue.svg)  
![\[class: atomic array\]](figures/class-atomic_array-blue.svg)  
![\[class: recursive array\]](figures/class-recursive_array-blue.svg)  

`i` specifies the interior (or flat) indices.  
`use` can be `1` or `-1`:  
`1` means the specified indices in `i` are to be used for the sub-set
operation.  
`-1L` means **all** indices **except** the indices in `i` are to be used
for the sub-set operation.  
  

Any of the following can be specified for argument `i`:

- `NULL` or `0L`, corresponds to missing argument.

- a vector of length 0, in which case no indices are selected for the
  operation (i.e. empty selection).

- a numeric vector of **strictly positive whole numbers** giving
  indices.

- a **logical vector**, of the same length as `x`, giving the indices to
  select for the operation.

- a **character** vector of index names.  
  If an object has multiple indices with the given name, ALL the
  corresponding indices will be selected for the operation.

- a **function** that takes as input `x`, and returns a logical vector,
  giving the element indices to select for the operation.  
  For atomic objects, `i` is interpreted as `i(x)`.  
  For recursive objects, `i` is interpreted as `lapply(x, i)`.

- a formula; see
  [keywords](https://tony-aw.github.io/squarebrackets/reference/aaa11_squarebrackets_keywords.md).  
    

Using the `i` arguments corresponds to doing something like the
following:

     ii_x(x, i = i) # ==> x[i]   # if `x` is atomic
     ii_x(x, i = i) # ==> x[i]   # if `x` is recursive

If `i` is a function, it corresponds to the following:

     ii_x(x, i = i) # ==> x[i(x)]          # if `x` is atomic
     ii_x(x, i = i) # ==> x[lapply(x, i)]  # if `x` is recursive

## Argument Pair `s, use`

![\[class: atomic array\]](figures/class-atomic_array-blue.svg)  
![\[class: recursive array\]](figures/class-recursive_array-blue.svg)  
The `s, use` argument pair, inspired by the
`abind::`[asub](https://rdrr.io/pkg/abind/man/asub.html) function from
the 'abind' package, is the primary indexing argument for sub-set
operations on atomic arrays or recursive arrays.  
  
The `s` argument specifies the **subscripts** (i.e. dimensional
indices).  
The **absolute value** of `use` argument gives the dimensions for which
the `s` holds (i.e. `use` specifies the "non-missing" margins), and the
sign (+ or -) of `use` specifies if the indices are to be selected or
excluded, similar to the `use` argument used in combination with `i`
(see previous section).  
  
Specifically, the `use` argument can be any of the following:

- a vector of length `0`, which will be interpreted as "all subscripts
  are missing".

- an integer vector.

- the real scalar `Inf`, which will be interpreted as `1:ndim(x)`.

- the real scalar `-Inf`, which will be interpreted as `-1:-ndim(x)`.  

`use` is not allowed to have any duplicate values, nor is it allowed to
include zero (`0`)`.`  
The default value for `use` is `Inf`.  

`s` must be a list of the same length as `use`, a list of length 1, or
an atomic vector.  
If `s` is a list of length 1, it is internally recycled to become the
same length as `use`.  
If `s` is an atomic vector, it is internally treated as `list(s)`, and
(as with the previous case) recycled to become the same length as
`use`.  
  
Each element of `s` when `s` is a list, or `s` as a whole when `s` is
atomic, can be any of the following:

- `NULL` or `0L`, which corresponds to a missing index argument.

- a vector of length 0, in which case no indices are selected for the
  operation (i.e. empty selection).

- a numeric vector of **strictly positive whole numbers** with indices
  of the specified dimension to select for the operation.

- a **logical** vector of the same length as the corresponding dimension
  size, giving the indices of the specified dimension to select for the
  operation.

- a **character** vector giving the `dimnames` to select.  
  If a dimension has multiple indices with the given name, ALL the
  corresponding indices will be selected for the operation.

- a formula; see
  [keywords](https://tony-aw.github.io/squarebrackets/reference/aaa11_squarebrackets_keywords.md).  
    

**EXAMPLES**  
Here are some examples for clarity, using an atomic array `x` of 3
dimensions.  
  
Examples with `s` as atomic vector:

    # extract the first 10 columns:
    ss_x(x, 1:10, 2)                 # ==> x[ , 1:10, , drop = FALSE]

    # extract first 10 indices of every dimension:
    ss_x(x, 1:10)                    # ==> x[1:10, 1:10, 1:10, drop = FALSE]

    # remove first 10 indices of every dimension:
    ss_x(x, 1:10, -Inf)              # ==> x[-1:-10, -1:-10, -1:-10, drop = FALSE]

    extract the first 10 rows, all columns, and the first 10 layers
    ss_x(x, 1:10, c(1, 3))           # ==> x[1:10, , 1:10, drop = FALSE]

To keep the syntax short, the user can use the
[n](https://tony-aw.github.io/squarebrackets/reference/nest.md) function
instead of [`list()`](https://rdrr.io/r/base/list.html) to specify
`s`.  
Here are examples where `s` is used as a list:

    # extract first 10 rows, all columns, and first 5 layers:
    ss_x(x, n(1:10, 0L, 1:5))        # ==> x[1:10, , 1:5, drop = FALSE]

    # extract first 10 rows, extract all columns, and **remove** first 5 layers:
    ss_x(x, n(1:10, 1:5), c(1, -3))  # ==> x[1:10, , -1:-5, drop = FALSE]

For a brief explanation on the relationship between flat indices (`i`)
and subscripts (`s`, `use`) in arrays, see
[ss2ii](https://tony-aw.github.io/squarebrackets/reference/ss2ii.md).  
  

## Arguments set `row, col, use`

![\[class: atomic matrix\]](figures/class-atomic_matrix-blue.svg)  
![\[class: recursive
matrix\]](figures/class-recursive_matrix-blue.svg)  
![\[class: data.frame-like\]](figures/class-data.frame-like-blue.svg)  

Specifies rows and columns in an atomic matrix, a recursive matrix, or
data.frame. The argument `row` and `col` can each be any of the
following:

- `NULL` or `0L`, which corresponds to a missing index argument.

- a vector of length 0, in which case no indices are selected for the
  operation (i.e. empty selection).

- a numeric vector of **strictly positive whole numbers** with dimension
  indices to select for the operation.

- a **logical** vector of the same length as the corresponding dimension
  size, giving the dimension indices to select for the operation.

- a **character** vector of index names.  
  If a dimension has multiple indices with the given name, ALL the
  corresponding indices will be selected for the operation.

- a formula; see
  [keywords](https://tony-aw.github.io/squarebrackets/reference/aaa11_squarebrackets_keywords.md).  

`use` is set to `1:2` by default.  
If `use` is `c(-1, 2)` or `-1`, the row indices will be inverted (i.e.
select all rows **except** those specified in `row`).  
If `use` is `c(1, -2)` or `-2`, the column indices will be inverted
(i.e. select all columns **except** those specified in `col`).  
If `use` is `-1:-2`, both the row- and column- indices will be
inverted.  
Specifying `use = 2:1` or `use = -2:-1` will result in an error.  
Moreover, a `use` of length other than 1 or 2, or a `use` containing a
`0` (zero), will also lead to an error.  
  

Examples:

     # Extracting only rows or columns:
     tt_x(x, 0, c)           # ==> x[, c]
     tt_x(x, r, 0)           # ==> x[r, ]

     # Extracting both:
     tt_x(x, r, c)           # ==> x[r, c]

     # Excluding rows:
     tt_x(x, r, c, c(-1, 2)) # ==> x[-r, c]
     tt_x(x, r, c, -1)       # ==> x[-r, c] (same as previous)

     # Excluding columns:
     tt_x(x, r, c, c(1, -2)) # ==> x[r, -c]
     tt_x(x, r, c, -2)       # ==> x[r, -c] (same as previous)

     # Excluding both:
     tt_x(x, r, c, -1:-2)    # ==> x[-r, -c]

## `row,col` for Data.frames

The `row,col` arguments, as explained in the previous section, works for
both matrices and data.frames.  
But data.frames have additional features for these arguments:

- `row` can also be a **double-tilded** formula, specifying column-based
  conditions to filter rows.  
  I.e. `~~ column1 > 10`.

- `col` can also be a function, where the function returns `TRUE` or
  `FALSE`.  

For example:

    d <- data.frame(a = 1:10, b = letters[1:10], c = rnorm(10), d = month.abb[1:10])

    # extract: rows for which `a > 5 & b != "j"` and columns which are NOT numeric
    tt_x(d, ~~ a > 5 & b != "j", is.numeric, -2L)
    #>   b   d
    #> 1 f Jun
    #> 2 g Jul
    #> 3 h Aug
    #> 4 i Sep

## Argument Pair `slice, use`

![\[class: atomic array\]](figures/class-atomic_array-blue.svg)  
![\[class: recursive array\]](figures/class-recursive_array-blue.svg)  
![\[class: data.frame-like\]](figures/class-data.frame-like-blue.svg)  

Relevant only for the `_icom` methods.  
The absolute value of `use` specifies the dimension on which argument
`slice` is used.  
The sign of `use`, just like in the previous argument sets, specifies
whether to invert the indices or not.  
I.e. if `use = 1` , `slice` specifies which rows to select;  
if `use = -2`, `slice` specifies which columns to **not** select;  
etc.  
  
The `slice` argument can be any of the following:

- `NULL` or `0L`, which corresponds to a missing index argument.

- a numeric vector of **strictly positive whole numbers** with dimension
  indices to select for the operation.

- a **logical** vector of the same length as the corresponding dimension
  size, giving the dimension indices to select for the operation.

- a **character** vector of index names.  
  If a dimension has multiple indices with the given name, ALL the
  corresponding indices will be selected for the operation.  

- a formula; see
  [keywords](https://tony-aw.github.io/squarebrackets/reference/aaa11_squarebrackets_keywords.md).

One could also give a vector of length `0` for `slice`;  
Argument `slice` is only used in the `_icom` methods, and the results
are meant to be used inside the regular `[` and `[<-` operators.  
Thus the effect of a zero-length index specification depends on the
rule-set of `[.class(x)` and `[<-.class(x)`.  
  

## All Missing Indices

`NULL` and `0L` in the indexing arguments correspond to a missing
argument.  
For `s, use`, specifying `use` of length 0 also corresponds to all
subscripts being missing.  
Thus, for the `_x` methods, using missing indexing arguments for all
indexing arguments corresponds to something like the following:

    x[]

Similarly, for the `_mod` and `_set` methods, using missing or `NULL`
indexing arguments corresponds to something like the following:

    x[] <- rp # for replacement
    x[] <- tf(x) # for transformation

The above is true even when using negative values for `use`.  
  

## Drop

Sub-setting with the generic methods from the 'squarebrackets' R-package
using dimensional arguments (`s, row, col, use`) always use
`drop = FALSE`.  
To drop potentially redundant (i.e. single level) dimensions, use the
[drop](https://rdrr.io/r/base/drop.html) function, like so:

     ss_x(x, s, use) |> drop() # ==> x[..., drop = TRUE]

## References

Plate T, Heiberger R (2016). *abind: Combine Multidimensional Arrays*. R
package version 1.4-5, <https://CRAN.R-project.org/package=abind>.
