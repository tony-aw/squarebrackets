
enumerate <- 0

# general errors ====
x <- list(a = 1:10, b = letters[1:10])
expect_error(
  dt_aggregate(x, SDcols = "a", by = "b", f = sum),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setcoe(x, vars = "a", v = as.numeric),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setrm(x, vars = "a"),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setadd(x, data.table::data.table(e = 1:10)),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setreorder(x),
  pattern = "`x` must be a data.table"
)

x <- data.table::data.table(a = 1:10, b = letters[1:10])
new <- list(a = 1:10, b = letters[1:10])
expect_error(
  dt_setadd(x, new),
  pattern = "`new` must be a data.frame-like object"
)
new <- data.table::data.table(e = 1:10, e = letters[1:10])
expect_error(
  dt_setadd(x, new),
  pattern = "`new` does not have unique variable names for all columns; \n fix this before subsetting"
)
new <- data.table::data.table(b = 1:10, c = letters[1:10])
expect_error(
  dt_setadd(x, new),
  pattern = "column(s) already exist",
  fixed = TRUE
)
expect_error(
  dt_setadd(x, data.table::data.table()),
  pattern = "must give at least one new column"
)
enumerate <- enumerate + 12


# dt_aggregate ====
x <- data.table::data.table(
  a = 1:20, b = 21:40,
  by1 = as.factor(rep(letters[1:4], 5)), by2 = as.factor(rep(letters[5:8], 5))
)
expect_error(
  dt_aggregate(x, SDcols = c("a", "b"), by = c("by1", "by2"), f = sum, order_by = NA),
  pattern = "`order_by` must be `TRUE` or `FALSE`"
)
expect_error(
  dt_aggregate(x, SDcols = ~ a + b, by = c("by1", "by2"), f = sum),
  pattern = "`SDcols` and `by` must be atomic vectors"
)
expect_error(
  dt_aggregate(x, SDcols = c("a", "b"), by = ~ by1 + by2, f = sum),
  pattern = "`SDcols` and `by` must be atomic vectors"
)

y <- x
expect_equal(
  dt_aggregate(x, SDcols = c("a", "b"), by = c("by1", "by2"), f = sum),
  y[, lapply(.SD, sum), .SDcols = c("a", "b"), by = c("by1", "by2")]
)
expect_equal(
  dt_aggregate(x, SDcols = c("a", "b"), by = c("by1", "by2"), f = sum, order_by = FALSE),
  y[, lapply(.SD, sum), .SDcols = c("a", "b"), by = c("by1", "by2")]
)
expect_equal(
  dt_aggregate(x, SDcols = c("a", "b"), by = c("by1", "by2"), f = sum, order_by = TRUE),
  y[, lapply(.SD, sum), .SDcols = c("a", "b"), keyby = c("by1", "by2")]
)

enumerate <- enumerate + 6


# dt_setcoe ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
cols <- c("a", "c")
dt_setcoe(x, vars = cols, v = \(x)x^2)
y[ , (cols):= lapply(.SD, \(x)x^2), .SD = cols]
expect_equal(
  x,
  y
)

x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
dt_setcoe(x, vars = is.numeric, v = \(x)x^2)
cols <- c("a", "c")
y[ , (cols):= lapply(.SD, \(x)x^2), .SD = cols]
expect_equal(
  x,
  y
)

expect_error(
  dt_setcoe(x, vars = is.numeric, v = ~x^2)
)

enumerate <- enumerate + 3


# dt_setrm ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
cols <- c("a", "c")
dt_setrm(x, vars = cols)
y[ , (cols):= NULL]
expect_equal(
  x,
  y
)

x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
y <- data.table::copy(x)
dt_setrm(x, vars = is.numeric)
cols <- c("a", "c")
y[ , (cols):= NULL]
expect_equal(
  x,
  y
)


x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
expect_error(
  dt_setrm(x),
  pattern = "must specify at least one column"
)

enumerate <- enumerate + 4


# dt_setadd ====
x <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = letters[11:20])
new <- data.table::data.table(e = 101:110, f = 111:120)
y <- cbind(x, new)
dt_setadd(x, new)
expect_equal(
  x,
  y
)

enumerate <- enumerate + 1

# dt_setreorder - roworder ====
n <- 1e4
chrs <- sample(c(letters, LETTERS), n, TRUE)
x <- data.table::data.table(
  a = 1L:n, b = n:1L, c = as.double(1:n), d = as.double(n:1),
  e = sort(chrs), f = sort(chrs, TRUE)
)
x2 <- x
x3 <- data.table::copy(x)
dt_setreorder(x, roworder = nrow(x):1)
expect_equal(
  x,
  x3[nrow(x3):1,]
)
expect_equal(
  x, x2
)
expect_error(
  dt_setreorder(x, roworder = (n-1):1),
  pattern = "`roworder` must be a strict permutation/shuffle of 1:nrow(x)",
  fixed = TRUE
)

n <- 1e4
chrs <- sample(c(letters, LETTERS), n, TRUE)
x <- data.table::data.table(
  a = 1L:n, b = n:1L, c = as.double(1:n), d = as.double(n:1),
  e = sort(chrs), f = sort(chrs, TRUE)
)
names(x) <- stringi::stri_dup(names(x), 100)
x2 <- x
x3 <- data.table::copy(x)
dt_setreorder(x, roworder = nrow(x):1)
expect_equal(
  x,
  x3[nrow(x3):1,]
)
expect_equal(
  x, x2
)
expect_error(
  dt_setreorder(x, roworder = (n-1):1),
  pattern = "`roworder` must be a strict permutation/shuffle of 1:nrow(x)",
  fixed = TRUE
)
enumerate <- enumerate + 6


# dt_setreorder - varorder ====
n <- 1e4
chrs <- sample(c(letters, LETTERS), n, TRUE)
x <- data.table::data.table(
  a = 1L:n, b = n:1L, c = as.double(1:n), d = as.double(n:1),
  e = sort(chrs), f = sort(chrs, TRUE)
)
x2 <- x
newnms <- rev(names(x))
dt_setreorder(x, varorder = rev(names(x)))
expect_equal(
  names(x2),
  newnms
)

enumerate <- enumerate + 1

