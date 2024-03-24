
enumerate <- 0

# sb2_rec ====
x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  )
)
expect_equal(sb2_rec(x, c(1,2,2)), "ABB")
expect_equal(sb2_rec(x, c(2,2,1)), "BBA")

expect_equal(
  sb2_rec(x, 1:2),
  x[[1:2]]
)

x2 <- x
x2[[1:2]] <- "foo"
sb2_rec(x, 1:2, "foo")
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 4

print(enumerate)


