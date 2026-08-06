# Functional Forms of data.table Operations

Functional forms of special data.table operations.  
These functions do not use Non-Standard Evaluation.  
These functions also benefit from the security measures that
'squarebrackets' implements for the [pass-by-reference
semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md).  

- `dt_aggr()` aggregates a data.table or tidytable, applying functions
  over columns specified in `col`.

- `dt_setcoe()` coercively transforms whole columns of a data.table,
  using [pass-by-reference
  semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md).

- `dt_setmutate()` modifies, adds, or removes columns, possibly based on
  other existing columns, using [pass-by-reference
  semantics](https://tony-aw.github.io/squarebrackets/reference/aaa09_squarebrackets_PassByReference.md).  
    

## Usage

``` r
dt_aggr(
  x,
  row = NULL,
  col = NULL,
  use = 1:2,
  fun,
  by,
  newnames = NULL,
  keyby = FALSE
)

dt_setcoe(
  x,
  col = NULL,
  use = 2L,
  v,
  chkdup = getOption("squarebrackets.chkdup", FALSE)
)

dt_setmutate(x, mutations)
```

## Arguments

- x:

  a `data.table` or `tidytable`.

- row, col, use:

  see
  [squarebrackets_index_args](https://tony-aw.github.io/squarebrackets/reference/aaa04_squarebrackets_index_args.md).  
  For `dt_setcoe()`, `use` must be either a scalar positive number to
  select columns, or a scalar negative number to exclude columns.

- fun:

  an aggregation function, or a list of aggregation functions.  
  A named list is the preferred form.

- by:

  a character vector, giving the names of the grouping column(s).

- newnames:

  a vector of names for the aggregated columns.  
  If `NULL` (default), `newnames` will be constructed as:  
  `paste0(names(fun), "(", names(tt_x(x, 0L, col)), ")")`.

- keyby:

  Boolean, indicating if the aggregated result should be ordered by the
  columns specified in `by`.

- v:

  the coercive transformation function

- chkdup:

  see
  [squarebrackets_options](https://tony-aw.github.io/squarebrackets/reference/aaa06_squarebrackets_options.md).  
  ![\[for performance: set to
  FALSE\]](figures/for_performance-set_to_FALSE-red.svg)  

- mutations:

  a named list, or a formula that evaluates to a named list.  
  List names that correspond to existing columns, will modify those
  columns.  
  List names that don't will new create columns with those names.  
  List contents can be `NULL` to remove a column, or a vector to replace
  the column.  
  In formula form, columns from `x` can be used as variables.  
  For example, the following formula will transform the existing column
  named "column3" using other existing columns, and it will create a new
  column called "newcolumn" as functions from existing columns:  
  `~ list(column3 = column1 / column2, newcolumn = column1 + column2)`  

## Value

For `dt_aggr()`:  
The aggregated `data.table` object.  
  
For the rest of the functions:  
Returns: VOID. These functions modify the object by reference.  
Do not use assignments like `x <- dt_setcoe(x, ...)`.  
Since these functions return void, you'll just get `NULL`.  
  

## Examples

``` r
# Aggregate data.table ====
d <- data.table::as.data.table(iris)
dt_setcoe(d, "Sepal.Length", 2L, v = as.integer)
print(d)
#>      Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#>             <int>       <num>        <num>       <num>    <fctr>
#>   1:            5         3.5          1.4         0.2    setosa
#>   2:            4         3.0          1.4         0.2    setosa
#>   3:            4         3.2          1.3         0.2    setosa
#>   4:            4         3.1          1.5         0.2    setosa
#>   5:            5         3.6          1.4         0.2    setosa
#>  ---                                                            
#> 146:            6         3.0          5.2         2.3 virginica
#> 147:            6         2.5          5.0         1.9 virginica
#> 148:            6         3.0          5.2         2.0 virginica
#> 149:            6         3.4          5.4         2.3 virginica
#> 150:            5         3.0          5.1         1.8 virginica
fun <- rep(list(mean = mean, var = var), 2L)
cols <- rep(c("Sepal.Length", "Sepal.Width"), each = 2L)
dt_aggr(d, 1:5, cols, c(-1, 2), fun = fun, by = "Species")
#>       Species mean(Sepal.Length) var(Sepal.Length) mean(Sepal.Width)
#>        <fctr>              <num>             <num>             <num>
#> 1:     setosa           4.622222         0.2404040          3.444444
#> 2: versicolor           5.480000         0.3363265          2.770000
#> 3:  virginica           6.080000         0.4424490          2.974000
#>    var(Sepal.Width)
#>               <num>
#> 1:       0.15116162
#> 2:       0.09846939
#> 3:       0.10400408


# Aggregate sf-data.table ====

if(requireNamespace("sf")) {
  x <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  x <- data.table::as.data.table(x)
  
  x$region <- ifelse(x$CNTY_ID <= 2000, 'high', 'low')
  d.aggr <- dt_aggr(
    x, 0L, "geometry", fun = sf::st_union, by = "region"
  )
  
  head(d.aggr)
}
#> Loading required namespace: sf
#> Reading layer `nc' from data source 
#>   `D:\Programs\R\R-4.6.1\library\sf\shape\nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#>    region                       geometry
#>    <char>             <sfc_MULTIPOLYGON>
#> 1:   high MULTIPOLYGON (((-75.78317 3...
#> 2:    low MULTIPOLYGON (((-76.46926 3...



#############################################################################

# dt_setcoe ====

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: int  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 
dt_setcoe(obj, is.numeric, v = as.numeric) # integers are now numeric
str(obj) # now those columns are double/numeric
#> Classes 'data.table' and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  1 2 3 4 5 6 7 8 9 10
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: num  11 12 13 14 15 16 17 18 19 20
#>  $ d: Factor w/ 10 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x000001c8d4855ef0> 


#############################################################################
# dt_setmutate ====

# add new columns based on other columns:
d <- data.table::as.data.table(iris)
mutations <- ~ n(
  Ratio.Length = Sepal.Length / Petal.Length,
  Ratio.Width = Sepal.Width / Petal.Width
)
dt_setmutate(d, mutations)
d
#>      Sepal.Length Sepal.Width Petal.Length Petal.Width   Species Ratio.Length
#>             <num>       <num>        <num>       <num>    <fctr>        <num>
#>   1:          5.1         3.5          1.4         0.2    setosa     3.642857
#>   2:          4.9         3.0          1.4         0.2    setosa     3.500000
#>   3:          4.7         3.2          1.3         0.2    setosa     3.615385
#>   4:          4.6         3.1          1.5         0.2    setosa     3.066667
#>   5:          5.0         3.6          1.4         0.2    setosa     3.571429
#>  ---                                                                         
#> 146:          6.7         3.0          5.2         2.3 virginica     1.288462
#> 147:          6.3         2.5          5.0         1.9 virginica     1.260000
#> 148:          6.5         3.0          5.2         2.0 virginica     1.250000
#> 149:          6.2         3.4          5.4         2.3 virginica     1.148148
#> 150:          5.9         3.0          5.1         1.8 virginica     1.156863
#>      Ratio.Width
#>            <num>
#>   1:   17.500000
#>   2:   15.000000
#>   3:   16.000000
#>   4:   15.500000
#>   5:   18.000000
#>  ---            
#> 146:    1.304348
#> 147:    1.315789
#> 148:    1.500000
#> 149:    1.478261
#> 150:    1.666667

# remove columns:
dt_setmutate(d, n(Ratio.Width = NULL)) # remove Ratio.Width
d
#>      Sepal.Length Sepal.Width Petal.Length Petal.Width   Species Ratio.Length
#>             <num>       <num>        <num>       <num>    <fctr>        <num>
#>   1:          5.1         3.5          1.4         0.2    setosa     3.642857
#>   2:          4.9         3.0          1.4         0.2    setosa     3.500000
#>   3:          4.7         3.2          1.3         0.2    setosa     3.615385
#>   4:          4.6         3.1          1.5         0.2    setosa     3.066667
#>   5:          5.0         3.6          1.4         0.2    setosa     3.571429
#>  ---                                                                         
#> 146:          6.7         3.0          5.2         2.3 virginica     1.288462
#> 147:          6.3         2.5          5.0         1.9 virginica     1.260000
#> 148:          6.5         3.0          5.2         2.0 virginica     1.250000
#> 149:          6.2         3.4          5.4         2.3 virginica     1.148148
#> 150:          5.9         3.0          5.1         1.8 virginica     1.156863

# transform existing columns:
mutations <- ~ n(Sepal.Length = Sepal.Length /100)
dt_setmutate(d, mutations)
d
#>      Sepal.Length Sepal.Width Petal.Length Petal.Width   Species Ratio.Length
#>             <num>       <num>        <num>       <num>    <fctr>        <num>
#>   1:        0.051         3.5          1.4         0.2    setosa     3.642857
#>   2:        0.049         3.0          1.4         0.2    setosa     3.500000
#>   3:        0.047         3.2          1.3         0.2    setosa     3.615385
#>   4:        0.046         3.1          1.5         0.2    setosa     3.066667
#>   5:        0.050         3.6          1.4         0.2    setosa     3.571429
#>  ---                                                                         
#> 146:        0.067         3.0          5.2         2.3 virginica     1.288462
#> 147:        0.063         2.5          5.0         1.9 virginica     1.260000
#> 148:        0.065         3.0          5.2         2.0 virginica     1.250000
#> 149:        0.062         3.4          5.4         2.3 virginica     1.148148
#> 150:        0.059         3.0          5.1         1.8 virginica     1.156863
```
