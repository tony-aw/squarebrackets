
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

 

To get started see `?squarebrackets_help`

 

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

- 10 March 2024: First GitHub upload - Package is very much
  experimental.
- 12 March 2024: Changed the introduction help page a bit, added
  `dt_setadd()`, and added tests for all `dt_` - functions. There are
  slightly over 50,000 tests now.
- 15 March 2024: Added the `sb_setRename()` method, and added tests for
  this method also.
- 16 March 2024: Fixed a bug in the “rcpp_set_rowcol” source code.
  Tweaked the documentation here and there a bit. Improved the tests a
  bit.

 
