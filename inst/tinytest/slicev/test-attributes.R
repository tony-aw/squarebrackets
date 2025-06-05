
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
enumerate <- 0L

# no attributes test ====
x <- sample(1:10)
y <- sample(1:10)
expect_equal(
  slicev_x(x, y = y, v = c(2, 9)),
  x[y >= 2 & y <= 9]
)
expect_equal(
  slicev_x(x, y = y, v = c(2, 9), sticky = FALSE),
  x[y >= 2 & y <= 9]
)
expect_equal(
  slicev_x(x, y = y, v = c(2, 9), sticky = TRUE),
  x[y >= 2 & y <= 9]
)


# use.names tests ====
x <- mutatomic(sample(1:10), names = letters[1:10])
y <- sample(1:10)

expect_equal(
  slicev_x(x, y = y, v = c(2, 9)),
  x[y >= 2 & y <= 9]
)

expect_equal(
  slicev_x(x, y = y, v = c(2, 9), use.names = FALSE),
  unname(x[y >= 2 & y <= 9])
)

# factor ====
x <- factor(sample(letters))
expect_equal(
  slicev_x(x, y = x, v = "b"),
  x[x == "b", drop = FALSE]
)
expect_equal(
  slicev_x(x, y = x, v = "b", sticky = FALSE),
  x[x == "b", drop = FALSE]
)
expect_equal(
  slicev_x(x, y = x, v = "b", sticky = TRUE),
  x[x == "b", drop = FALSE]
)


# roman ====
x <- as.roman(1:10)
expect_equal(
  slicev_x(x, y = y, v = c(2, 9)),
  x[y >= 2 & y <= 9]
)
expect_equal(
  slicev_x(x, y = y, v = c(2, 9), sticky = TRUE),
  x[y >= 2 & y <= 9]
)
expected <- x[y >= 2 & y <= 9]
attributes(expected) <- NULL
expect_equal(
  slicev_x(x, y = y, v = c(2, 9), sticky = FALSE),
  expected
)

enumerate <- enumerate + 11L

