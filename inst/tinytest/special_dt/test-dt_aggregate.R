
enumerate <- 0


# errors ====
x <- data.table::data.table(
  a = 1:20, b = 21:40,
  by1 = as.factor(rep(letters[1:4], 5)), by2 = as.factor(rep(letters[5:8], 5))
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = sum, by = c("by1", "by2"), keyby = NA),
  pattern = "`keyby` must be `TRUE` or `FALSE`"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = sum, by = ~ by1 + by2),
  pattern = "`by` must be a character vector of one or more column names"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = sum, by = c("by1", "by1")),
  pattern = "`by` cannot have duplicate values"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = sum, by = "foo"),
  pattern = "`by` specifies an unknown column"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = ~ sum, by = "by1"),
  pattern = "`fun` must be a function or a list of functions"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = list(sum = "sum"), by = "by1"),
  pattern = "`fun` must be a function or a list of functions"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = list(), by = "by1"),
  pattern = "zero-length `fun` or `col` not supported"
)
expect_error(
  dt_aggr(x, 0L, integer(0L), fun = sum, by = "by1"),
  pattern = "zero-length `fun` or `col` not supported"
)
expect_error(
  dt_aggr(x, 0L, integer(0L), fun = list(), by = "by1"),
  pattern = "zero-length `fun` or `col` not supported"
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = list(sum, mean, var), by = "by1"),
  pattern = "`length(fun)` and `length(col)` are not multiple of each other",
  fixed = TRUE
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = list(sum, mean), by = "by1", newnames = "a"),
  pattern = "`newnames` must be the same length as the replicated lengths of `fun` and `col`",
  fixed = TRUE
)
expect_error(
  dt_aggr(x, 0L, c("a", "b"), fun = list(sum, mean), by = "by1", newnames = "a"),
  pattern = "`newnames` must be the same length as the replicated lengths of `fun` and `col`",
  fixed = TRUE
)

enumerate <- enumerate + 12L


# equivalence to base aggregate() function ====
x <- data.table::data.table(
  a = sample(as.raw(1:100)),
  b = sample(c(TRUE, FALSE, NA), 100, TRUE),
  c = sample(c(NA, 1:100), 100),
  d = c(NA, NaN, Inf, -Inf, rnorm(96)),
  e = c(NA, NaN, Inf, -Inf, rnorm(96)) + c(NA, NaN, Inf, -Inf, rnorm(96)) * -1i,
  f = sample(c(month.abb, NA), 100, TRUE),
  by1 = as.factor(rep_len(letters[1:4], 100)),
  by2 = as.factor(rep_len(letters[5:8], 100))
)
y <- x
expected <- aggregate(
  y[, c("c", "d")], y[, c("by1")], \(x)mean(x, na.rm = TRUE)
) |> data.table::as.data.table()
names(expected) <- c("by1", "mean(c)", "mean(d)")
out <- dt_aggr(
  x, 0L, c("c", "d"), fun = list(mean = \(x)mean(x, na.rm = TRUE)), by = "by1"
)
expect_equal(expected, out)

expected <- aggregate(
  y[, c("c", "d")], y[, c("by1", "by2")], \(x)mean(x, na.rm = TRUE)
) |> data.table::as.data.table()
names(expected) <- c("by1", "by2", "mean(c)", "mean(d)")
out <- dt_aggr(
  x, 0L, c("c", "d"), fun = list(mean = \(x)mean(x, na.rm = TRUE)), by = c("by1", "by2")
)
expect_equal(expected, out)


enumerate <- enumerate + 2L


# equivalence to data.table operations ====
x <- data.table::data.table(
  a = sample(as.raw(1:100)),
  b = sample(c(TRUE, FALSE, NA), 100, TRUE),
  c = sample(c(NA, 1:100), 100),
  d = c(NA, NaN, Inf, -Inf, rnorm(96)),
  e = c(NA, NaN, Inf, -Inf, rnorm(96)) + c(NA, NaN, Inf, -Inf, rnorm(96)) * -1i,
  f = sample(c(month.abb, NA), 100, TRUE),
  by1 = as.factor(rep_len(letters[1:4], 100)),
  by2 = as.factor(rep_len(letters[5:8], 100))
)
y <- x

fun <- list(
  mean = mean,
  var = var
) |> rep(2)
col <- c("c", "d") |> rep(each = 2)

# with rows:
expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1"),
  y[1:50, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), by = c("by1")]
)
expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1", keyby = TRUE),
  y[1:50, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), keyby = c("by1")]
)
expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = c("by1", "by2")),
  y[1:50, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), by = c("by1", "by2")]
)
expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = c("by1", "by2"), keyby = TRUE),
  y[1:50, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), keyby = c("by1", "by2")]
)


# without rows:
expect_equal(
  dt_aggr(x, 0L, col, fun = fun, by = "by1"),
  y[, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), by = c("by1")]
)
expect_equal(
  dt_aggr(x, 0L, col, fun = fun, by = "by1", keyby = TRUE),
  y[, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), keyby = c("by1")]
)
expect_equal(
  dt_aggr(x, 0L, col, fun = fun, by = c("by1", "by2")),
  y[, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), by = c("by1", "by2")]
)
expect_equal(
  dt_aggr(x, 0L, col, fun = fun, by = c("by1", "by2"), keyby = TRUE),
  y[, .(`mean(c)` = mean(c), `var(c)` = var(c), `mean(d)` = mean(d), `var(d)` = var(d)), keyby = c("by1", "by2")]
)


enumerate <- enumerate + 8L



# newnames - automatic recycling ====
x <- data.table::data.table(
  a = sample(as.raw(1:100)),
  b = sample(c(TRUE, FALSE, NA), 100, TRUE),
  c = sample(c(NA, 1:100), 100),
  d = c(NA, NaN, Inf, -Inf, rnorm(96)),
  e = c(NA, NaN, Inf, -Inf, rnorm(96)) + c(NA, NaN, Inf, -Inf, rnorm(96)) * -1i,
  f = sample(c(month.abb, NA), 100, TRUE),
  by1 = as.factor(rep_len(letters[1:4], 100)),
  by2 = as.factor(rep_len(letters[5:8], 100))
)
y <- x
fun <- list(
  mean = mean,
  var = var
)
col <- c("c", "d") |> rep(each = 2)

expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1"),
  dt_aggr(x, 1:50, col, fun = rep(fun, 2), by = "by1")
)

fun <- list(
  mean = mean,
  var = var
) |> rep(each = 2)
col <- c("c", "d")

expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1"),
  dt_aggr(x, 1:50, rep(col, 2), fun = fun, by = "by1")
)

enumerate <- enumerate + 2L



# newnames - manual specification as-if auto ====
x <- data.table::data.table(
  a = sample(as.raw(1:100)),
  b = sample(c(TRUE, FALSE, NA), 100, TRUE),
  c = sample(c(NA, 1:100), 100),
  d = c(NA, NaN, Inf, -Inf, rnorm(96)),
  e = c(NA, NaN, Inf, -Inf, rnorm(96)) + c(NA, NaN, Inf, -Inf, rnorm(96)) * -1i,
  f = sample(c(month.abb, NA), 100, TRUE),
  by1 = as.factor(rep_len(letters[1:4], 100)),
  by2 = as.factor(rep_len(letters[5:8], 100))
)
y <- x
fun <- list(
  mean = mean,
  var = var
)
col <- c("c", "d") |> rep(each = 2)

expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1"),
  dt_aggr(x, 1:50, col, fun = fun, by = "by1",
          newnames = c("mean(c)", "var(c)", "mean(d)", "var(d)"))
)

fun <- list(
  mean = mean,
  var = var
) |> rep(each = 2)
col <- c("c", "d")

expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1"),
  dt_aggr(x, 1:50, rep(col, 2), fun = fun, by = "by1",
          newnames = c("mean(c)", "mean(d)", "var(c)", "var(d)"))
)

enumerate <- enumerate + 2L



# newnames - custom manual ====
x <- data.table::data.table(
  a = sample(as.raw(1:100)),
  b = sample(c(TRUE, FALSE, NA), 100, TRUE),
  c = sample(c(NA, 1:100), 100),
  d = c(NA, NaN, Inf, -Inf, rnorm(96)),
  e = c(NA, NaN, Inf, -Inf, rnorm(96)) + c(NA, NaN, Inf, -Inf, rnorm(96)) * -1i,
  f = sample(c(month.abb, NA), 100, TRUE),
  by1 = as.factor(rep_len(letters[1:4], 100)),
  by2 = as.factor(rep_len(letters[5:8], 100))
)
y <- x
fun <- list(
  mean = mean,
  var = var
)
col <- c("c", "d") |> rep(each = 2)

newnames <- c("mean1", "var1", "mean2", "var1")

expect_equal(
  dt_aggr(x, 1:50, col, fun = fun, by = "by1",
          newnames = newnames) |> names(),
  c("by1", newnames)
)

enumerate <- enumerate + 1L
