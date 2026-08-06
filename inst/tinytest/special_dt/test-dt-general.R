
enumerate <- 0

# general errors ====
x <- list(a = 1:10, b = letters[1:10])
expect_error(
  dt_aggr(x, 0L, "a", by = "b", f = sum),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setcoe(x, "a", v = as.numeric),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setmutate(x, n(new = 1:1)),
  pattern = "`x` must be a data.table"
)
enumerate <- enumerate + 3L



# dt_setcoe ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
cols <- c("a", "c")
dt_setcoe(x, cols, v = \(x)x^2)
y[ , (cols):= lapply(.SD, \(x)x^2), .SD = cols]
expect_equal(
  x,
  y
)

x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
dt_setcoe(x, is.numeric, v = \(x)x^2)
cols <- c("a", "c")
y[ , (cols):= lapply(.SD, \(x)x^2), .SD = cols]
expect_equal(
  x,
  y
)

expect_error(
  dt_setcoe(x, is.numeric, v = ~ x^2)
)

enumerate <- enumerate + 3


# dt_setmutate, remove ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
cols <- c("a", "c")
dt_setmutate(x, list(a = NULL, c = NULL))
y[ , (cols):= NULL]
expect_equal(
  x,
  y
)

x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
nms <- names(x)[vapply(x, is.numeric, logical(1L))]
mutations <- vector("list", length(nms))
names(mutations) <- nms
dt_setmutate(x, mutations)
cols <- c("a", "c")
y[ , (cols):= NULL]
expect_equal(
  x,
  y
)

expect_warning(dt_setmutate(x, list(e = NULL)))

enumerate <- enumerate + 3L


# dt_setmutate, add ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
new <- data.table::data.table(e = 101:110, f = 111:120)
y <- cbind(x, new)
dt_setmutate(x, new)
expect_equal(
  x,
  y
)

x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
mutations <- ~ n(e = a/c, f = paste0(b, d))
y <- data.table::copy(x)
y[, c("e", "f") := list(a/c, paste0(b, d))]
dt_setmutate(x, mutations)
expect_equal(
  x,
  y
)

enumerate <- enumerate + 2L


# dt_setmutate, modify ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
mutations <- list(a = 11:20, b = month.abb[1:10])
y <- data.table::copy(x)
y[, c("a", "b") := list(11:20, month.abb[1:10])]
dt_setmutate(x, mutations)
expect_equal(
  x,
  y
)



x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
mutations <- ~ n(a = a/c, b = paste0(b, d))
y <- data.table::copy(x)
y[, c("a", "b") := list(a/c, paste0(b, d))]
dt_setmutate(x, mutations)
expect_equal(
  x,
  y
)

enumerate <- enumerate + 2L


