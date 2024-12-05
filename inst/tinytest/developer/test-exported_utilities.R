
enumerate <- 0
x <- seq(1.1, 30.1, by = 1.0)
names(x) <- c(letters, rep("a", 3), NA)



# integers ====
expect_equal(
  indx_x(15L, x, names(x), length(x)),
  15
)
expect_equal(
  indx_wo(15L, x, names(x), length(x)),
  c(1:14, 16:30)
)
enumerate <- enumerate + 2L



# decimal ====

expect_equal(
  indx_x(15.0, x, names(x), length(x)),
  15
)
expect_equal(
  indx_wo(15.0, x, names(x), length(x)),
  c(1:14, 16:30)
)
enumerate <- enumerate + 2L



# logical ====

expect_equal(
  indx_x(c(rep(FALSE, 14), TRUE, rep(FALSE, 15)), x, names(x), length(x)),
  15
)
expect_equal(
  indx_wo(c(rep(FALSE, 14), TRUE, rep(FALSE, 15)), x, names(x), length(x)),
  c(1:14, 16:30)
)
enumerate <- enumerate + 2L



# names ====

expect_equal(
  indx_x("a", x, names(x), length(x)),
  c(1, 27, 28, 29)
)
expect_equal(
  indx_wo("a", x, names(x), length(x)),
  c(2:26, 30)
)

expect_equal(
  indx_x("c", x, names(x), length(x)),
  3
)
expect_equal(
  indx_wo("c", x, names(x), length(x)),
  c(1:2, 4:30)
)
enumerate <- enumerate + 4L



# complex ====

expect_equal(
  indx_x(1:2 * -1i, x, names(x), length(x)),
  length(x) - 1:2 + 1L
)
expect_equal(
  indx_x(2:3 * -1i, x, names(x), length(x)),
  length(x) - 2:3 + 1L
)

expect_equal(
  indx_x(1:2 * 1i, x, names(x), length(x)),
  1:2
)
expect_equal(
  indx_x(2:3 * 1i, x, names(x), length(x)),
  2:3
)

expect_equal(
  indx_wo(1:2 * -1i, x, names(x), length(x)),
  1:(length(x) - 2)
)
expect_equal(
  indx_wo(2:3 * -1i, x, names(x), length(x)),
  c(1:(length(x) - 3), length(x))
)

expect_equal(
  indx_wo(1:2 * 1i, x, names(x), length(x)),
  seq_along(x)[-1:-2]
)
expect_equal(
  indx_wo(2:3 * 1i, x, names(x), length(x)),
  seq_along(x)[-2:-3]
)
enumerate <- enumerate + 8L


# functions ====

expect_equal(
  indx_x(\(x)x>=5, x, names(x), length(x)),
  5:30
)
expect_equal(
  indx_wo(\(x)x>=5, x, names(x), length(x)),
  1:4
)
enumerate <- enumerate + 2L



# zero-length ====
expect_equal(
  indx_x(logical(0), x, names(x), length(x)),
  numeric(0)
)
expect_equal(
  indx_wo(logical(0), x, names(x), length(x)),
  seq_len(length(x))
)
enumerate <- enumerate + 2L



# NULL ====

expect_true(identical(indx_x(NULL, x, names(x), length(x)), base::quote(expr = )))
expect_true(identical(indx_wo(NULL, x, names(x), length(x)), base::quote(expr = )))
enumerate <- enumerate + 2L



