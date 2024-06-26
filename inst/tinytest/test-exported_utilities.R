

x <- seq(1.1, 30.1, by = 1.0)
names(x) <- c(letters, rep("a", 3), NA)
expect_equal(
  indx_x("a", x, names(x), length(x)),
  c(1, 27, 28, 29)
)
expect_equal(
  indx_rm("a", x, names(x), length(x)),
  c(2:26, 30)
)

expect_equal(
  indx_x("c", x, names(x), length(x)),
  3
)
expect_equal(
  indx_rm("c", x, names(x), length(x)),
  c(1:2, 4:30)
)

expect_equal(
  indx_x(15L, x, names(x), length(x)),
  15
)
expect_equal(
  indx_rm(15L, x, names(x), length(x)),
  c(1:14, 16:30)
)

expect_equal(
  indx_x(15.0, x, names(x), length(x)),
  15
)
expect_equal(
  indx_rm(15.0, x, names(x), length(x)),
  c(1:14, 16:30)
)

expect_equal(
  indx_x(c(rep(FALSE, 14), TRUE, rep(FALSE, 15)), x, names(x), length(x)),
  15
)
expect_equal(
  indx_rm(c(rep(FALSE, 14), TRUE, rep(FALSE, 15)), x, names(x), length(x)),
  c(1:14, 16:30)
)

expect_equal(
  indx_x(\(x)x>=5, x, names(x), length(x)),
  5:30
)
expect_equal(
  indx_rm(\(x)x>=5, x, names(x), length(x)),
  1:4
)

expect_equal(
  indx_x(logical(0), x, names(x), length(x)),
  numeric(0)
)
expect_equal(
  indx_rm(logical(0), x, names(x), length(x)),
  seq_len(length(x))
)


expect_equal(
  indx_x(1:2 - 1i, x, names(x), length(x)),
  length(x) - 1:2 + 1L
)
expect_equal(
  indx_x(2:3 - 1i, x, names(x), length(x)),
  length(x) - 2:3 + 1L
)

expect_equal(
  indx_x(1:2 + 1i, x, names(x), length(x)),
  1:2
)
expect_equal(
  indx_x(2:3 + 1i, x, names(x), length(x)),
  2:3
)

expect_true(identical(indx_x(NULL, x, names(x), length(x)), base::quote(expr = )))
expect_true(identical(indx_rm(NULL, x, names(x), length(x)), base::quote(expr = )))

enumerate <- 16
