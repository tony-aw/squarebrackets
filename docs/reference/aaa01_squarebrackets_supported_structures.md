# Supported Structures

'squarebrackets' only supports the most common S3 objects, and only
those that primarily use square brackets for sub-set operations (hence
the name of the package).  
  
One can generally divide the structures supported by 'squarebrackets'
along 3 key properties:

- atomic vs recursive:  
  Types `logical`, `integer`, `double`, `complex`, `character`, and
  `raw` are [`atomic`](https://rdrr.io/r/base/is.recursive.html).  
  Lists and data.frames are
  [`recursive`](https://rdrr.io/r/base/is.recursive.html).

- dimensionality:  
  Whether an object is a vector, array, or data.frame.  
  Note that a matrix is simply an array with 2 dimensions.

- mutability:  
  Base R's S3 classes (except Environments) are generally immutable:  
  Modifying the object will create a copy (called 'copy-on-modify').  
  'squarebrackets also supports `data.tables` and
  [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
  objects, which are mutable:  
  If desired, one can modify them without copy using pass-by-reference
  semantics.  
    

**Supported Structures**  

'squarebrackets' supports the following immutable structures:

- basic `atomic` classes  
  (atomic vectors and arrays).

- [factor](https://rdrr.io/r/base/factor.html).  

- basic list classes  
  (recursive vectors and arrays).  

- [data.frame](https://rdrr.io/r/base/data.frame.html)  
  (including the classes `tibble`, `sf-data.frame` and `sf-tibble`).  
    

'squarebrackets' supports the following mutable structures:

- [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)  
  (`mutatomic` vectors arrays);

- [data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  (including the classes `tidytable`, `sf-data.table`, and
  `sf-tidytable`).  
    

The methods provided by 'squarebrackets', like any method, can be
extended (by other 'R' package authors) to support additional classes
that are not already supported natively by 'squarebrackets'.  
  

## Details

**Atomic vs Recursive**  
The `ii_`/`ss_` methods provided by 'squarebrackets' work on **atomic**
(see [is.atomic](https://rdrr.io/r/base/is.recursive.html)) objects.  
The `ii2_`/`ss2_` methods provided by 'squarebrackets' work on
**recursive** (see
[is.recursive](https://rdrr.io/r/base/is.recursive.html)) objects.  
See
[squarebrackets_methods](https://tony-aw.github.io/squarebrackets/reference/aaa02_squarebrackets_methods.md)
for more details.  
  

**Dimensionality**  
'squarebrackets' supports dimensionless or vector objects (i.e.
[ndim](https://tony-aw.github.io/squarebrackets/reference/ndim.md)` == 0L`).  
squarebrackets' supports arrays (see
[is.array](https://rdrr.io/r/base/array.html) and
[is.matrix](https://rdrr.io/r/base/matrix.html)); note that a matrix is
simply an array with 2 dimensions.  
'squarebrackets' also supports data.frame-like objects (see
[is.data.frame](https://rdrr.io/r/base/as.data.frame.html)).  
Specifically, squarebrackets' supports a wide variety of data.frame
classes:  
`data.frame`, `data.table`, `tibble`, `tidytable`;  
'squarebrackets' also supports their 'sf'-package compatible
counter-parts:  
`sf-data.frame`, `sf-data.table`, `sf-tibble`, `sf-tidytable`.  
  
Dimensionless vectors and dimensional arrays are supported in both their
atomic and recursive forms.  
Data.frame-like objects, in contrast, only exist in the recursive form
(and, as stated, are supported by 'squarebrackets').  
Recursive vectors, recursive matrices, and recursive arrays, are
collectively referred to as "lists" in the 'squarebrackets'
documentation.  
  
Note that the dimensionality of data.frame-like objects is not the same
as the dimensionality of (recursive) arrays/matrices.  
For example:  
For any array/matrix `x`, it holds that `length(x) == prod(dim(x))`.  
But for any data.frame `x`, it is the case that
`length(x) == ncol(x)`.  
  

**Mutable vs Immutable**  
Most of base R's S3 classes (except Environments) are generally
immutable:  
Modifying the object will create a copy (called 'copy-on-modify').  
They have no explicit
[pass-by-reference](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md)
semantics.  
Most S3 objects in base 'R' are immutable:  
Environments do have
[pass-by-reference](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md)
semantics, but they are not supported by 'squarebrackets'.  
  
Supported mutable structures:

- 'squarebrackets' supports the mutable `data.table` class  
  (and thus also `tidytable`, which inherits from `data.table`).

- 'squarebrackets' supports the
  [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
  class.  
  `mutatomic` objects are the same as atomic objects, except they are
  mutable (hence the name).  
    

Supported immutable structures:  
Atomic and recursive vectors/matrices/arrays, data.frames, and
tibbles.  
  
All the functions in the 'squarebrackets' package with the word "set" in
their name perform pass-by-reference modification, and thus only work on
mutable structures.  
All other functions work the same way for both mutable and immutable
structures.  
  

**Derived Atomic Vector**  
A special class of objects are the Derived Atomic Vector structures:  
structures that are derived from atomic objects, but behave
differently.  
For example:  
Factors, datetime, POSIXct and so on are derived from atomic vectors.  
But they have attributes and special methods that make them behave
differently.  
  
'squarebrackets' treats derived atomic classes as regular atomic
vectors.  
There are highly specialized packages to handle objects derived from
atomic objects.  
For example, the 'anytime' package to handle date-time objects.  
  
'squarebrackets does provide some more explicit support for factors.  
  

**Not Supported S3 structures**  
Key-Values storage S3 structures, such as environments, are not
supported by 'squarebrackets'.  
  
