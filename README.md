
<!-- README.md is generated from README.Rmd. Please edit that file -->

# squarebrackets

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

squarebrackets: Methods as an Alternative to the Square Brackets
Operators

 

## Introduction

The ‘squarebrackets’ package provides methods to be used in situation
where regular square brackets (`[`) are inconvenient. A few examples
will be given of situations where `[` is inconvenient, and how
‘squarebrackets’ helps in such situations.

 

Suppose you have an array `x`. In order to perform subset operations on
`x` with `[`, you need to know how many dimensions it has. I.e. if `x`
has 3 dimensions, one would use `x[i, j, k, drop = FALSE]` or
`x[i, j, k] <- value`. But what if you don’t know a-priori the number of
dimensions `x` has. How would you do this? It’s not impossible with `[`,
but still rather convoluted.

The ‘squarebrackets’ package solves this by providing methods that use
name-based arguments, instead of position based arguments.

 

The `data.frame`, `tibble`, `data.table`, and `tidytable` objects, which
all inherit from class “data.frame”, use slightly different rules
regarding the usage of `[`. Especially `data.table` has **very**
different rules. Constantly switching between these rules is annoying,
and makes your code look inconsistent.

The ‘squarebrackets’ package solves this, by using methods that are
consistent across the classes `data.frame`, `tibble`, `data.table`, and
`tidytable`, and their direct extensions (such as the `sf-data.table`
class).

 

‘R’ adheres to copy-on-modification semantics when replacing values
using `[<-`. Usually this is fine. But sometimes one would like explicit
control when to create a copy, and when to modify using
pass-by-reference semantics.

The ‘squarebrackets’ package provides 2 methods with explicit semantics:
`sb_mod()` for modification through deep copies, and `sb_set()` for
modification through pass-by-reference semantics.

 

Now, to get started see `?squarebrackets_help`

 

## Installing & Loading

One can install ‘squarebrackets’ from GitHub like so:

``` r
remotes::install_github("https://github.com/tony-aw/squarebrackets")
```

Special care has been taken to make sure the function names are clear,
and that the function names are unlikely to conflict with core R, the
recommended R packages, the rstudioapi package, or major packages from
the fastverse. So one can attach the package - thus exposing its
functions to the namespace - using:

``` r
library(squarebrackets)
```

Should the user wish to expose specific functions from ‘squarebrackets’
ONLY within a specific environment, like only within a specific
function, one can use the following:

``` r
tinycodet::import_LL("squarebrackets", selection = ... )
```

 

## Changelog (EXPERIMENTAL VERSIONS)

First GitHub upload - Package is very much experimental.

 
