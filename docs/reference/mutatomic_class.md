# A Class of Mutable Atomic Array-Like Objects

The `mutatomic` class is a class of mutable atomic array-like objects.  
It works exactly the same in all aspects as regular atomic vectors and
arrays.  
There is only one real difference:  
Pass-by-reference functions in 'squarebrackets' only accept atomic
objects when they are of class `mutatomic`, for greater safety.  
In all other aspects, `mutatomic` objects are the same as R's regular
atomic objects, including the behaviour of the `[<-` operator .  
  
Exposed functions (beside the S3 methods):

- `mutatomic()`: create a `mutatomic` object from given data.

- `couldb.mutatomic()`: checks if an object could become `mutatomic`.  

## Usage

``` r
mutatomic(data, names = NULL, dim = NULL, dimnames = NULL, comment = NULL)

as.mutatomic(x, ...)

is.mutatomic(x)

couldb.mutatomic(x)
```

## Arguments

- data:

  atomic vector giving data to fill the `mutatomic` object.

- names, dim, dimnames, comment:

  see their respective help pages.

- x:

  an atomic object.

- ...:

  method dependent arguments.

## Value

For `mutatomic()`, `as.mutatomic()`:  
Returns a `mutatomic` object.  
  
For `is.mutatomic()`:  
Returns `TRUE` if the object is `mutatomic`, and returns `FALSE`
otherwise.  
  
For `couldb.mutatomic()`:  
Returns `TRUE` if the object can be converted to `mutatomic`.  
Returns `FALSE` otherwise.  
  

## Details

**`couldb.mutatomic()`**:  
The `couldb.mutatomic()` function checks if an object can be converted
to a `mutatomic` object.  
Only objects with the following properties can be converted to
`mutatomic`:  

- **`atomic` data type**:  
  The data type of the object is `atomic`.  
  I.e. [raw](https://rdrr.io/r/base/raw.html),
  [logical](https://rdrr.io/r/base/logical.html),
  [integer](https://rdrr.io/r/base/integer.html),
  [double](https://rdrr.io/r/base/double.html),
  [complex](https://rdrr.io/r/base/complex.html), or
  [character](https://rdrr.io/r/base/character.html).  
  Thus recursive objects, like lists, and S4 objects, and so on cannot
  become `mutatomic`.  
  `ALTREP` objects will be materialized when coerced to `mutatomic`.

- **length equals number of elements**:  
  The length of the object equals the number of elements.  
  This is the case for the vast majority of objects.  
  But some objects, like the various classes of the 'bit' package, break
  this principle and are thus not compatible with `mutatomic`.

- **product of dimensions equals number of elements**:  
  If the object has dimensions, the product of the dimension sizes must
  equal the number of elements.

- **array-like**:  
  The object is "array-like", meaning that the object is a vector where
  one can *meaningfully* set, remove, or modify the dimensions, without
  breaking its functionality.  
  Thus a factor (which already represents a dummy matrix), most
  data-time objects (which generally do not support dimensions), 'dist'
  objects (which are 1d arrays pretending to be lower-triangle
  matrices), and special printed vectors (hexmode, octmode, romans),
  cannot be `mutatomic`.

- **missing values consistent with base 'R'**:  
  The meaning of `NA` in the vector is consistent with that of base
  'R'.  
  This is the case for the vast majority of objects.  
  But some classes, like `integer64` has a different definition of `NA`
  than base 'R', which is not supported by the `mutatomic` class.

- **not a table-like**:  
  `table` and `ftable` objects cannot be mutatomic.  
    

## Footnotes

- Always use the exported functions given by 'squarebrackets' to create
  a `mutatomic` object, as they make necessary safety checks.

- Mutable objects that can only exist as vectors can be modified via
  functions from the 'data.table' or 'collapse' packages.

- Mutable date-time objects already exist in the form of S4 classes, and
  need no coverage from the 'mutatomic' class.

- While one can force dimension attributes upon factors and base R's
  date-time objects, this may break or corrupt it's interactions with
  methods (like the `[` and
  [`print()`](https://rdrr.io/r/base/print.html) methods), coercions
  (like [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)),
  and array manipulators (like
  [`aperm()`](https://rdrr.io/r/base/aperm.html)).  
  'squarebrackets' does not allow such sloppiness, and thus strictly
  consider factors and date-time objects to be vectors but not
  'array-like'.  

- It would not make much sense to make factors dimensional, since they
  are primarily used to represent a dummy matrix for statistical
  modelling.  
  A dimensional array of categorical values that's not used for
  modelling is better off being represented as either character array or
  integer array.  
    

## Examples

``` r
x <- mutatomic(
  1:20, dim = c(5, 4), dimnames = list(letters[1:5], letters[1:4])
)
x
#>   a  b  c  d
#> a 1  6 11 16
#> b 2  7 12 17
#> c 3  8 13 18
#> d 4  9 14 19
#> e 5 10 15 20
#> mutatomic 
#> typeof:  integer 

x <- matrix(1:10, ncol = 2)
x <- as.mutatomic(x)
is.mutatomic(x)
#> [1] TRUE
print(x)
#>      [,1] [,2]
#> [1,]    1    6
#> [2,]    2    7
#> [3,]    3    8
#> [4,]    4    9
#> [5,]    5   10
#> mutatomic 
#> typeof:  integer 
x[, 1]
#> [1] 1 2 3 4 5
#> mutatomic 
#> typeof:  integer 
x[] <- as.double(x)
#> coercing type from `integer` to `double`
print(x)
#>      [,1] [,2]
#> [1,]    1    6
#> [2,]    2    7
#> [3,]    3    8
#> [4,]    4    9
#> [5,]    5   10
#> mutatomic 
#> typeof:  double 
is.mutatomic(x)
#> [1] TRUE
```
