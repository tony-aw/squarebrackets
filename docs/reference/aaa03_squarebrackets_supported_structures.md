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
  If desired, one can modify them without copy using [pass-by-reference
  semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md).  
    

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
  (`mutatomic` vectors and arrays);

- [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)  
  (including the classes `tidytable`, `sf-data.table`, and
  `sf-tidytable`).  
    

The methods provided by 'squarebrackets', like any method, can be
extended (by other 'R' package authors) to support additional classes
that are not already supported natively by 'squarebrackets'.  
  

## Details

**Atomic vs Recursive**  
Atomic and recursive objects are quite different from each other in some
ways:

- **homo- or heterogeneous**: an atomic object can only have values of
  one data type.  
  recursive objects can hold values of any combination of data types.

- **nesting**: Recursive objects can be nested, while atomic objects
  cannot be nested.

- **copy and coercion effect**: One can coerce or copy a subset of a
  recursive object, without copying the rest of the object. For atomic
  objects, however, a coercion or copy operation coerces or copies the
  entire vector (ignoring attributes).

- **vectorization**: most vectorized operations generally work on atomic
  objects, whereas recursive objects often require loops or apply-like
  functions.  
  Therefore, when transforming recursive objects using the `*_mod` and
  `*_set` methods, 'squarebrackets' will apply transformation via
  [lapply](https://rdrr.io/r/base/lapply.html).

- **recursive subsets**: Recursive objects distinguish between "regular"
  subset operations (in base R using `[`, `[<-`), and recursive subset
  operations (in base R using `[[`, `[[<-`).  
  See the
  [lst_rec](https://tony-aw.github.io/squarebrackets/reference/lst_rec.md)
  method and the
  [dropl](https://tony-aw.github.io/squarebrackets/reference/dropl.md)
  helper function.  
  For atomic objects, these 2 have no meaningful difference (safe for
  perhaps some minor attribute handling).  

- **views**: For recursive objects, one can create a View of a recursive
  subset (see also
  [squarebrackets_coercion](https://tony-aw.github.io/squarebrackets/reference/aaa10_squarebrackets_coercion.md)).  
  Subset views do not exist for atomic objects.  
    

**Dimensionality**  
'squarebrackets' supports dimensionless or vector objects (i.e.
[ndim](https://tony-aw.github.io/squarebrackets/reference/size.md)` == 0L`).  
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
  
Supported mutable structures:

- 'squarebrackets' supports the mutable `data.table` class  
  (and thus also `tidytable`, which inherits from `data.table`).

- 'squarebrackets' supports the
  [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
  class.  
  [mutatomic](https://tony-aw.github.io/squarebrackets/reference/mutatomic_class.md)
  objects are the same as atomic objects, except they are mutable (hence
  the name).  
    

Supported immutable structures:  
Atomic and recursive vectors/matrices/arrays, data.frames, and
tibbles.  
  
All the functions in the 'squarebrackets' package with the word "set" in
their name perform [pass-by-reference
modification](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md),
and thus only work on mutable structures.  
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
  
