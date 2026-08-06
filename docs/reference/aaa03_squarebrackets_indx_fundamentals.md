# Indexing Fundamentals

This help page explains the fundamentals regarding how 'squarebrackets'
treats indexing.  
Some familiarity with base R's `[` and `[<-` operators is required to
follow this help page.  
  

## Indexing Forms

Consider the following representation of array indices for a (in this
case) 5 by 4 matrix:

         [,1] [,2] [,3] [,4]
    [1,]    1    6   11   16
    [2,]    2    7   12   17
    [3,]    3    8   13   18
    [4,]    4    9   14   19
    [5,]    5   10   15   20

The numbers `1` to `20` on the interior of this representation, are
referred to in this documentation as "interior indices" (abbreviated as
"ii"), also known as "flat indices".  
The numbers on the edges of this representations, `1` to `5` for the
rows and `1` to `4` for the columns, are referred to in this
documentation as "subscripts" (abbreviated as "ss"), also known as
"dimensional indices".  
  
Thus 'squarebracets' supports these 2 forms of indexing:  
Indexing by interior indices, and indexing by subscripts.  
  
Arrays support both interior indices as well as subscripts.  
Vectors only support interiod indices.  
Data.frames only support subscripts.  
  
One can operate on flat/interior indices (often simply referred to as
"indices") using the `ii_`/ `ii2_` methods;  
These primarily use the
[i](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md)
argument.  
  
One can operate on subscripts (= dimensional indices) using the `ss_`/
`ss2_` methods;  
These primarily use the the [s,
d](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md)
argument pair.  
  
For the relationship between flat/interior indices and subscripts for
arrays, see the
[sub2ind](https://tony-aw.github.io/squarebrackets/reference/sub2ind.md)
help page.  
  
  

## Indexing Types

Base 'R' supports indexing through `logical`, `integer`, and `character`
vectors.  
'squarebrackets' supports these also (albeit with some improvements),
but also supports some additional methods of indexing.  
  
  
**Whole numbers**  
Whole numbers are the most basic form on index selection.  
All forms of indexing in 'squarebrackets' are internally translated to
integer (or double if` > (2^31 - 1)`) indexing first, ensuring
consistency.  
Indexing through integer/numeric indices in 'squarebrackets' works the
same as in base 'R', except that negative values are not allowed.  
So indexing starts at `1` and is inclusive.  
  
  
**Logical**  
Selecting indices with a logical vector in 'squarebrackets' works the
same as in base 'R', except that recycling is not allowed.  
  
  
**Characters**  
When selecting indices using a character vector, base 'R' only selects
the first matches in the names.  
'squarebrackets', however, selects all matches:

    nms <- c("a", letters[4:1], letters[1:5])
    x <- 1:10
    names(x) <- nms
    print(x) #' `x` has multiple elements with the name "a"
    #>  a  d  c  b  a  a  b  c  d  e
    #>  1  2  3  4  5  6  7  8  9 10

    ii_x(x, "a") # extracts all indices with the name "a"
    #> a a a
    #> 1 5 6

    ii_x(x, c("a", "a")) # repeats all indices with the name "a"
    #> a a a a a a
    #> 1 5 6 1 5 6

Character indices are internally translated to integer indices using
[match_all](https://tony-aw.github.io/squarebrackets/reference/match_all.md).  
  
  
**Imaginary Numbers**  
A [complex](https://rdrr.io/r/base/complex.html) vector `y` is
structured as  
`y = a + b * i`  
where `Re(y)` returns `a`, and `Im(y)` returns `b`.  
squarebrackets' includes support for indexing through imaginary numbers
(`Im(y)`) of [complex](https://rdrr.io/r/base/complex.html) vectors.  
Indexing with imaginary numbers is a generalization of indexing with
regular integers.  
  
It works as follows:  
Imaginary numbers that are positive integers, like `1:10 * 1i`, work the
same as regular integers.  
Imaginary numbers that are negative integers, like `1:10 * -1i`, index
by counting backwards (i.e. from the end).  
Positive and negative numbers can be combined, like
`c(-10:1, 1:10) * 1i`.  
Note that **only** the `Imaginary` part of a complex vector is used
(`Im(y)`);  
the `Real` part (`Re(y)`) is **ignored**.  
  
See the results of the following code as an example:

    x <- 1:30 # vector of 30 elements

    ii_x(x, 1:10 * 1i) # extract first 10 elements
    #>  [1]  1  2  3  4  5  6  7  8  9 10

    ii_x(x, 1:10 * -1i) # extract last 10 elements
    #>  [1] 30 29 28 27 26 25 24 23 22 21

    ii_x(x, 10:1 * -1i) # last 10 elements, in tail()-like order
    #>  [1] 21 22 23 24 25 26 27 28 29 30

    ii_x(x, c(1, -1) * 1i) # extract first and last element
    #> [1]  1 30

Thus complex vectors allow the user to choose between counting from the
beginning, like regular integers, or backwards counting from the end, or
a combination of both.  
  
  
**Missing Index Argument**  
Both `NULL` and the numeric scalar `0L` can be used to specify an
missing index argument.  
For example, given a matrix `x`, `ss_x(x, n(0, 1:10))` is equivalent to
`x[ , 1:10]`.  
  
  

## Inverting

Inverting indices means to specify all elements **except** the given
indices.  
Consider for example the atomic vector `month.abb` (abbreviate month
names).  
Given this vector, indices `1:5` gives
`c("Jan" "Feb" "Mar" "Apr", "May")`.  
Inverting those same indices will give
`c("Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")`.  
  
In base 'R', inverting an index is done in different ways.  
(negative numbers for numeric indexing, negation for logical indexing,
manually un-matching for character vectors).  
  
'squarebrackets' provides a (somewhat) consistent syntax to invert
indices:  

- The methods that end with `_x` perform extraction;  
  to invert extraction, i.e. return the object **without** the specified
  subset, use the methods that end with `_wo`.  

- In the modification methods (`_mod_`/`_set_`) one can set the argument
  [inv](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md)` = TRUE`
  to invert indices.  
    

**EXAMPLES**

    x <- month.abb
    print(x)
    #>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


    ii_x(x, 1:5) # extract first 5 elements
    #> [1] "Jan" "Feb" "Mar" "Apr" "May"

    ii_wo(x, 1:5) # return WITHOUT first 5 elements
    #> [1] "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


    ii_mod(x, 1:5, rp = "XXX") # copy, replace first 5 elements, return result
    #>  [1] "XXX" "XXX" "XXX" "XXX" "XXX" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

    ii_mod(x, 1:5, inv = T, rp = "XXX") # same, but for all except first 5 elements
    #>  [1] "Jan" "Feb" "Mar" "Apr" "May" "XXX" "XXX" "XXX" "XXX" "XXX" "XXX" "XXX"

**ABOUT ORDERING**  
The order in which the user gives indices when inverting indices
generally does not matter.  
The order of the indices as they appear in the original object `x` is
maintained, just like in base 'R'.  
  
  

## Out-of-Bounds Integers, Non-Existing Names, and NAs

- Integer indices that are out of bounds (including `NaN` and
  `NA_integer_`) always give an error.

- Character indices that specify non-existing names is considered a form
  of zero-length indexing.  
  Specifying `NA` names returns an error.  

- Logical indices are translated internally to integers using
  [which](https://rdrr.io/r/base/which.html), and so `NA`s are
  ignored.  
    

## Index-less Sub-set Operations

Until now this help page focussed on performing sub-set operations with
an indexing vector.  
  
Performing sub-set operations on a long vector using a index vector
(which may itself also be a long vector) is not very memory-efficient.  
'squarebrackets' therefore introduces index-less sub-set operations,
through the
[slice](https://tony-aw.github.io/squarebrackets/reference/slice.md)`_`
and
[slicev](https://tony-aw.github.io/squarebrackets/reference/slicev.md)`_`
methods.  
These methods are much more memory and computationally efficient than
index-based sub-set methods (and so also a bit better for the
environment!).  
  
The
[slice](https://tony-aw.github.io/squarebrackets/reference/slice.md)`_`
methods perform sequence based sub-set operations.  
  
The
[slicev](https://tony-aw.github.io/squarebrackets/reference/slicev.md)`_`
methods (notice the "v" at the end) perform value-based sub-set
operations.  
Though this method is intentionally kept relatively simple, it is still
involved enough to warrant its own help page;  
for the details on value-based index-less sub-set operations, please see
[squarebrackets_slicev](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_slicev.md).  
  
  

## Regarding Performance

Integer vectors created through the `:` operator are "compact ALTREP"
integer vectors, and provide the fastest way to specify indices.  
Indexing through names (i.e. character vectors) is the slowest.  
Complex vectors of imaginary numbers are somewhat in the middle in terms
of speed.  
  
Index-less sub-set operations are usually faster and more memory
efficient than any index-based sub-set operation.  
So if performance is important, use index-less sub-set operations, or
use compact ALTREP integer indices.  
  
  

## Indexing in Recursive Subsets

Until now this help page focussed on indexing for regular (or "shallow")
subsets.  
This section will discuss indexing in recursive subsets.  
  
One of the differences between atomic and recursive objects, is that
recursive objects support recursive subsets, while atomic objects do
not.  
  
Bear in mind that every element in a recursive object is a reference to
another object.  
Consider the following list `x`:

    x <- list(
       A = 1:10,
       B = letters,
       C = list(A = 11:20, B = month.abb)
    )

Regular subsets, AKA surface-level subset operations (`[`, `[<-` in base
'R'), operate on the recursive object itself.  
I.e.
[ii2_x](https://tony-aw.github.io/squarebrackets/reference/sb_x.md)`(x, 1)`,
or equivalently `x[1]`, returns the **list** `list(A = 1:10)`:

    ii2_x(x, 1) # equivalent to x[1]; returns list(A = 1:10)
    #> $A
    #>  [1]  1  2  3  4  5  6  7  8  9 10

Recursive subset operations (`[[`, `[[<-`, and `$` in base 'R'), on the
other hand, operate on an object a subset of the recursive object
references to.  
I.e.
[lst_rec](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)`(x, 1)`,
or equivalently `x[[1]]`, returns the **integer vector** `1:10`:

    lst_rec(x, 1) # equivalent to x[[1]]; returns 1:10
    #>  [1]  1  2  3  4  5  6  7  8  9 10

Recursive objects can refer to other recursive objects, which can
themselves refer to recursive objects, and so on.  
Recursive subsets can go however deep you want.  
So, for example, to extract the character vector `month.abb` from the
aforementioned list `x`, one would need to do:  
[lst_rec](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)`(x, c("C","B"))`,
(in base R: `x$C$B`):

    lst_rec(x, c("C","B")) # equivalent to x$C$B
    #>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

    # or:

    lst_rec(x, c(3, 2)) # equivalent to x[[3]][[2]]
    #>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

**LIMITATIONS**  
Indexing in recursive subsets is significantly more limited than in
regular (or "shallow") subsets:

- Recursive subset operations using
  [lst_rec](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)/[lst_recin](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)
  only support positive integer vectors and character vectors.

- Imaginary numbers (using complex vectors) and logical vectors are not
  supported.

- Since a recursive subset operation only operates on a single element,
  specifying the index with a character vector only selects the first
  matching element (just like base 'R'), not all matches.

- Inverting indices is also **not** available for recursive indexing.

- Unlike regular sub-setting, out-of-bounds specification for indices is
  acceptable, as it can be used to add new values to lists.  
    

## Non-Standard Evaluation

'squarebrackets' is designed primarily for programming, and seeks to be
fully programmatically friendly.  
As part of this endeavour, 'squarebrackets' never uses Non-Standard
Evaluation.  
All input for all methods and functions in 'squarebrackets' are objects
that can be stored in a variable.  
Like atomic vectors, lists, formulas, etc.  
  
