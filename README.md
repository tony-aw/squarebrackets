
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

 

Among programming languages, ‘R’ has perhaps one of the most flexible
and comprehensive sub-setting functionality, provided by the square
brackets operators (`[`, `[ <-`).

But in some occasions, the square brackets operators are less than
optimally convenient; examples:

- Performing subset operations on an array, when the number of
  dimensions of the array are not known a-priori, is quite convoluted.
- The `data.frame`, `tibble`, `data.table` and `tidytable` classes use
  different rule sets for the square brackets operators. So one needs to
  learn new rules for different data.frame classes, making your one’s
  code inconsistent.
- Selecting names for sub-setting, only the first occurrences of the
  names are selected for the sub-set. And when un-selecting/removing
  names for sub-setting, the syntax is very different from selecting
  names
- ‘R’ implicitly uses copy-on-modify semantics. The `[<-` operator does
  not support explicitly choosing between deep copy or pass-by-reference
  semantics.

The ‘squarebrackets’ package provides methods as an alternative to the
square brackets operators, that are more convenient in such occasions.

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
- 17 March 2024: Added more tests, and tweaked the documentation a bit.
- 19 March 2024: Some methods/functions did not support mutable_atomic
  type “complex”; this is now fixed. Added support for mutable_atomic
  type “raw”. Added tests for atomic type handling. Added the functions
  `ma_setv()`, and `couldb.mutable_atomic()`. Added the options “sb.rat”
  and “sb.chkdup”; argument `chkdup` is now also set to `FALSE` by
  default. Added more badges to the documentation.
- 20 March 2024: The user can now also specify `coe = TRUE` in
  `sb_mod.data.frame()`.
- 24 March 2024: Methods are now split between methods for non-recursive
  objects (`sb_`), and methods for recursive objects (`sb2_`).
- 26 March 2024: Replaced `seq_rec()` with `seq_rec2()`.
- 27 March 2024: Added `dt_setreorder()`, and added tests for this also.
  ‘abind’ now as a dependency, and ‘abind’ based code removed, as it is
  redundant.
- 29 March 2024: Added `sb2_before.array()` and `sb2_after.array()`, and
  added tests for these also. Added tests for data.frame-like coercion
  types. Tweaked the documentation here and there a bit.
- 30 March 2024: Removed the separate `NA` checks, as they are
  redundant. Fixed some linguistic mistakes in the documentation.
- 1 April 2024: Removed `sb_coe()` but kept `sb2_coe()`. Added `inv`
  argument to `sb_mod()`/`sb2_mod()`, `sb_set()`/`sb2_set()`, and
  `sb2_coe()`, and added tests for these. Added `idx1()` for
  Copy-On-Modification Substitution, and added tests for `idx1()`
  also.Fixed a bug in character subset ordering in the
  `sb`/`sb2`\_`mod`/`set`/`coe` - generic methods. Fixed a bug in the
  introduction message. Added even more tests. Added `idx1_dim()`, and
  added tests for these also.
- 5 April 2024: Replaced `idx1`/`idx0` with `idx()`.
- 18 May 2024: Added a few tests for the `idx()` method (need to add
  more). Fixed the export pattern expressions in the Namespace file.
  Adjusted the documentation.

 
