# Indexing Fundamentals of 'squarebrackets'

This help page explains the fundamentals regarding how 'squarebrackets'
treats indexing.  
Some familiarity with base R's `[` and `[<-` operators is required to
follow this help page.  
  

## Indexing Forms

Consider the following representation of array indices for a (in this
case) 4 by 5 matrix:

         [,1] [,2] [,3] [,4] [,5]
    [1,] [1]  [5]  [9]  [13] [17]
    [2,] [2]  [6]  [10] [14] [18]
    [3,] [3]  [7]  [11] [15] [19]
    [4,] [4]  [8]  [12] [16] [20]

The numbers `1` to `20` on the interior of this representation, are
referred to in this documentation as "interior indices" (abbreviated as
"ii"), also known as "flat indices".  
The numbers on the edges of this representations, `1` to `4` for the
rows and `1` to `5` for the columns, are referred to in this
documentation as "subscripts" (abbreviated as "ss"), also known as
"dimensional indices".  
Indexing by rows and columns, referred to as tabular tiles (abbrevated
as "tt"), is a commonly used special subset of using subscripts,
available only for data.frames and matrices.  
  
Thus 'squarebracets' supports these 3 forms of indexing:  
Indexing by interior indices, indexing by subscripts, and tabular
tiles.  
  
Regarding which kind of object supports which kind of indexing form:

- Matrices, which are simply 2-dimensional arrays, support all 3 of the
  above given indexing forms.

- Arrays in general can always support both interior indices and
  subscripts.

- Dimensionless vectors (i.e. objects for which
  [`dim()`](https://rdrr.io/r/base/dim.html) returns `NULL`) only
  support interior indices.

- Data.frames only support tabular tiles.  
    

Regarding which set of
[methods](https://tony-aw.github.io/squarebrackets/reference/aaa01_squarebrackets_methods.md)
support which kind of indexing form:

- One can operate on flat/interior indices (often simply referred to as
  "indices") using the
  [ii\_](https://tony-aw.github.io/squarebrackets/reference/aaa01_squarebrackets_methods.md)
  methods.  
  These primarily use the [i,
  use](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md)
  argument pair.  
  One can also use the
  [long\_](https://tony-aw.github.io/squarebrackets/reference/long.md)
  methods, though these operate on **virtual** interior indices called a
  [stride](https://tony-aw.github.io/squarebrackets/reference/aaa08_squarebrackets_stride.md).

- One can operate on general subscripts (= dimensional indices) using
  the
  [ss\_](https://tony-aw.github.io/squarebrackets/reference/aaa01_squarebrackets_methods.md)
  methods;  
  These primarily use the the [s,
  use](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md)
  argument pair.

- One can operate on tabular tiles using the
  [tt\_](https://tony-aw.github.io/squarebrackets/reference/aaa01_squarebrackets_methods.md)
  methods;  
  These primarily use the the [row, col,
  use](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_indx_args.md)
  argument pair.  
    

For the relationship between flat/interior indices and subscripts for
arrays, see the
[ss2ii](https://tony-aw.github.io/squarebrackets/reference/ss2ii.md)
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
  
In 'squarebrackets', inverting is consistently done through the `use`
argument:  
A positive sign for `use` means to select the specified indices, a
negative sign for `use` means to select all indices **except** the
specified indices.  
  

**EXAMPLES**

    x <- month.abb
    print(x)
    #>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


    ii_x(x, 1:5) # extract first 5 elements
    #> [1] "Jan" "Feb" "Mar" "Apr" "May"

    ii_x(x, 1:5, -1) # return WITHOUT first 5 elements
    #> [1] "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


    ii_mod(x, 1:5, rp = "XXX") # copy, replace first 5 elements, return result
    #>  [1] "XXX" "XXX" "XXX" "XXX" "XXX" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

    ii_mod(x, 1:5, -1, rp = "XXX") # same, but for all except first 5 elements
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
[long](https://tony-aw.github.io/squarebrackets/reference/long.md)`_`
methods.  
These methods are much more memory and computationally efficient than
index-based sub-set methods (and so also a bit better for the
environment!).  
  
  

## Regarding Performance

Integer vectors created through the `:` operator are "compact ALTREP"
integer vectors, and provide the fastest way to specify indices.  
Indexing through names (i.e. character vectors) is the slowest.  
  
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
[ii_x](https://tony-aw.github.io/squarebrackets/reference/sb_x.md)`(x, 1)`,
or equivalently `x[1]`, returns the **list** `list(A = 1:10)`:

    ii_x(x, 1) # equivalent to x[1]; returns list(A = 1:10)
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

- Logical vectors are not supported.

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
  
