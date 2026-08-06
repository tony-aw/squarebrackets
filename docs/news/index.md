# Changelog

## squarebrackets 0.0.0.9011

- Overhauled the `_mod` methods: they now use R’ native semantics for
  atomic objects, and shallow pass-by-value semantics for recursive
  objects.
- Removed the `_icom` methods, as they’re no longer necessary.
- Added quick way to filter rows by condition in a data.frame using
  double-tilded formulas.
- Added more tests.

## squarebrackets 0.0.0.9010

- Giving non-existing names as indices now gives a proper error.
- Added the `dt_` functions.
- Added the
  [`ci_df()`](https://tony-aw.github.io/squarebrackets/reference/developer_ci.md)
  function.
- Using `use = -2:-1` or `use = 2:1` in the `tt_` will now result in an
  error, to ensure consistent syntax.

## squarebrackets 0.0.0.9009

- Added `arepl()`.
- Replaced the use of `do.call(...)` in the `ss_` methods with
  `eval(quote(...))`.
- Replaced the `strid_pv()` stride function with the more generic
  [`stride_v()`](https://tony-aw.github.io/squarebrackets/reference/stride_v.md)
  S3 method; Users can now create custom methods for it.
- Moved the `use` argument from the `long_` methods into the `stride_`
  functions/methods.
- Streamlined the internal code here and there.
- Fixed some documentation errors.
- **Bug Fix**: data.table gave an error when sub-setting it if one of
  its columns was a mutatomic object. This is because data.table does
  not directly call `[.mutatomic`. This issue is now fixed, by
  re-defining the mutatomic internal attributes. The mutatomic class is
  now compatible with data.table and other functions that internally do
  not call `[` directly.

## squarebrackets 0.0.0.9008

- Completely overhauled the methods for index-less sub-set operations on
  long vectors; They are now the `long_` methods, and use `stride`
  objects to perform index-less sub-set operations.
- Replaced the `sbt_` methods with the `tt_` methods (same methods,
  different names)

## squarebrackets 0.0.0.9007

- Completely overhauled the arguments regarding inverting indices in
  almost all methods.
- Removed imaginary numbers as a form of indices in favour of the
  **much** more flexible formula (`~...`) based indices.
- Simplified the input for the `slicev_` methods.
- Restructured the unit tests for the `slice_` and `slicev_` methods.
- Replaced `idx` method with the `_icom` methods.
- Removed `idx_r`.
- Replaced the `obs, vars` arguments with the `row, col` arguments in
  the `sbt_` methods;
- Fixed a few relatively small bugs

## squarebrackets 0.0.0.9006

- Overhauled the main methods: replaced {`ii_`, `ii2_`, `ss_`, `ss2_`}
  methods with { `ii_`, `ss_`, `sbt_` }
- Removed the `dt_*` functions.
- Removed the `idx_ord_*` functions.
- Renamed `sub2ind` and friends to `ss2ii` and so on.

## squarebrackets 0.0.0.9005

- `0L` can now be used as an alias for `NULL` in the indexing arguments.
- `NULL` or `0L` can now be used in a list for the `ss` argument, for
  more convenience.
- Added proper tests for the `obs, vars` arguments.
- Improved speed of the array-related methods.

## squarebrackets 0.0.0.9004

- Improved speed for the `_set` methods.
- Simplified the internal code used for matrices.
- **Bug fix** in the `ss, d` argument pair, where something like
  `1:2, 1:2` behave differently then `list(1:2), 1:2` for data.frames.

## squarebrackets 0.0.0.9003

- Improved speed for the `slice_` and `slicev_` methods.

## squarebrackets 0.0.0.9002

- Renamed the `i_` and `i2_` methods to `ii_` and `ii2_`, to avoid
  confusion.
- Renamed `ci_flat()` to
  [`ci_ii()`](https://tony-aw.github.io/squarebrackets/reference/developer_ci.md)
  and `ci_sub()` to
  [`ci_ss()`](https://tony-aw.github.io/squarebrackets/reference/developer_ci.md).

## squarebrackets 0.0.0.9001

- Placed the `i2_rec()` and `i2_recin()` in its own, separate set of
  methods: the `lst_` methods.
- Adjusted the documentation and tests in accordance with the above
  change.
- The `ss_`/`ss2_` methods will now give an error if dimensional sub-set
  operations are attempted on non-dimensional objects.

## squarebrackets 0.0.0.9000

- First “real” experimental version of ‘squarebrackets’ on GitHub.
