
# squarebrackets 0.0.0.9006
* Overhauled the main methods: replaced {`ii_`, `ii2_`, `ss_`, `ss2_`} methods with { `ii_`, `ss_`, `sbt_` }
* Removed the `dt_*` functions.
* Removed the `idx_ord_*` functions.
* Renamed `sub2ind` and friends to `ss2ii` and so on.



# squarebrackets 0.0.0.9005
* `0L` can now be used as an alias for `NULL` in the indexing arguments.
* `NULL` or `0L` can now be used in a list for the `ss` argument, for more convenience.
* Added proper tests for the `obs, vars` arguments.
* Improved speed of the array-related methods.


# squarebrackets 0.0.0.9004
* Improved speed for the `_set` methods.
* Simplified the internal code used for matrices.
* **Bug fix** in the `ss, d` argument pair, where something like `1:2, 1:2` behave differently then `list(1:2), 1:2` for data.frames.


# squarebrackets 0.0.0.9003
* Improved speed for the `slice_` and `slicev_` methods.


# squarebrackets 0.0.0.9002
* Renamed the `i_` and `i2_` methods to `ii_` and `ii2_`, to avoid confusion.
* Renamed `ci_flat()` to `ci_ii()` and `ci_sub()` to `ci_ss()`.


# squarebrackets 0.0.0.9001
* Placed the `i2_rec()` and `i2_recin()` in its own, separate set of methods: the `lst_` methods.
* Adjusted the documentation and tests in accordance with the above change.
* The `ss_`/`ss2_` methods will now give an error if dimensional sub-set operations are attempted on non-dimensional objects.


# squarebrackets 0.0.0.9000
* First "real" experimental version of 'squarebrackets' on GitHub.

