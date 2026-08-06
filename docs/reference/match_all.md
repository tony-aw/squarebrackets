# Match All, Order-Sensitive and Duplicates-Sensitive

Find all indices of vector `haystack` that are equal to vector
`needles`, taking into account the order of both vectors, and their
duplicate values.  
  
`match_all()` is essentially a much more efficient version of:

    lapply(needles, \(i) which(haystack == i))

Like `lapply(needles, \(i) which(haystack == i))`, `NA`s are ignored.  
  
`match_all()` internally calls
`collapse::`[fmatch](https://fastverse.org/collapse/reference/fmatch.html)
and
`collapse::`[gsplit](https://fastverse.org/collapse/reference/GRP.html).  
Core of the code is based on a suggestion by Sebastian Kranz (author of
the 'collapse' package).  
  

## Usage

``` r
match_all(needles, haystack, unlist = TRUE)
```

## Arguments

- needles, haystack:

  vectors of the same type.  
  `needles` cannot contain `NA`/`NaN`.  
  Long vectors are not supported.

- unlist:

  Boolean, indicating if the result should be a single unnamed integer
  vector (`TRUE`, default), or a named list of integer vectors
  (`FALSE`).  

## Value

An integer vector, or list of integer vectors.  
If a list, each element of the list corresponds to each value of
`needles`.  
When `needles` and/or `haystack` is empty, or when `haystack` is fully
`NA`, `match_all()` returns an empty integer vector (if
`unlist = TRUE`), or an empty list (if `unlist = FALSE`).  

## Examples

``` r
n <- 200
haystack <- sample(letters, n, TRUE)
needles <- sample(letters, n/2, TRUE)
indices1 <- match_all(needles, haystack)
head(indices1)
#> [1]  51 133 178 180 189  25
```
