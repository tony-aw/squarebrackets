x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    A = list(A  = "AA2A", B = "AA2B"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  )
)

expect_equal(
  sb_rec(x, 1:2),
  x[[1:2]]
)

x2 <- x
x2[[1:2]] <- "foo"
sb_rec(x, 1:2, "foo")
expect_equal(
  x,
  x2
)
