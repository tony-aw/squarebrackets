
<!-- README.md is generated from README.Rmd. Please edit that file -->

# squarebrackets

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

squarebrackets: Subset Methods as Alternatives to the Square Brackets
Operators for Programming

## Description

Provides subset methods (supporting both atomic and recursive S3
classes) that may be more convenient alternatives to the `[` and `[<-`
operators, whilst maintaining similar performance.

Some nice properties of these methods include, but are not limited to,
the following:

1)  The `[` and `[<-` operators use different rule-sets for different
    data.frame-like types (data.frames, data.tables, tibbles,
    tidytables, etc.). The ‘squarebrackets’ methods use the same
    rule-sets for the different data.frame-like types.
2)  Performing dimensional subset operations on an array using `[` and
    `[<-`, requires a-priori knowledge on the number of dimensions the
    array has. The ‘squarebrackets’ methods work on any arbitrary
    dimensions without requiring such prior knowledge.
3)  When selecting names with the `[` and `[<-` operators, only the
    first occurrence of the names are selected in case of duplicate
    names. The ‘squarebrackets’ methods always perform on all names in
    case of duplicates, not just the first.
4)  The `[<-` operator only supports copy-on-modify semantics for most
    classes. The ‘squarebrackets’ methods provides explicit
    pass-by-reference and pass-by-value semantics, whilst still
    respecting things like binding-locks and mutability rules.
5)  ‘squarebrackets’ supports index-less sub-set operations, which is
    more memory efficient (and better for the environment) for
    `long vectors` than sub-set operations using the `[` and `[<-`
    operators.

 

## Get Started

To get started see the [Get
Started](https://tony-aw.github.io/squarebrackets/articles/squarebrackets.html)
page.

 

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

 

 
