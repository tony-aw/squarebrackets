

x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    A = list(A  = "AA2A", B = "AA2B"),
    B = list(A = "ABA", B = "ABB"),
    C = letters
  ),
  Y = list(
    Z = list(Z = "YZZ", Y = "YZY"),
    Y = list(Z = "YYZ", Y = "YYY"),
    X = "YX"
  )
)

# margin = 0 ====
y2 <- list(
  A.A.A = "AAA",
  A.A.B = "AAB",
  A.A.A = "AA2A",
  A.A.B = "AA2B",
  A.B.A = "ABA", 
  A.B.B = "ABB",
  A.C = letters,
  Y.Z.Z = "YZZ",
  Y.Z.Y = "YZY",
  Y.Y.Z = "YYZ",
  Y.Y.Y = "YYY",
  Y.X = "YX"
)

# use.names = TRUE
y <- lst_untree(x, margin = 0, use.names = TRUE)

expect_equal(
  y,y2
)
expect_equal(
  y[["Y.Z.Y"]],
  x[[c("Y", "Z", "Y")]]
)

# use.names = FALSE
y <- lst_untree(x, margin = 0, use.names = FALSE)
expect_equal(
  y, unname(y2)
)


# margin = 1L ====
# use.names = TRUE
y <- lst_untree(x, margin = 1, use.names = TRUE)
expect_equal(
  dim(y),
  c(length(x), sapply(x, lst_nlists) |> max())
)
expect_equal(
  rownames(y),
  names(x)
)

# use.names = FALSE
y <- lst_untree(x, margin = 1, use.names = FALSE)
expect_equal(
  dim(y),
  c(length(x), sapply(x, lst_nlists) |> max())
)


# margin = 2 ====
# use.names = TRUE
y <- lst_untree(x, margin = 2, use.names = TRUE)
expect_equal(
  dim(y),
  c(sapply(x, lst_nlists) |> max(), length(x))
)
expect_equal(
  colnames(y),
  names(x)
)

# use.names = FALSE
y <- lst_untree(x, margin = 2, use.names = TRUE)
expect_equal(
  dim(y),
  c(sapply(x, lst_nlists) |> max(), length(x))
)


# other ====
x <- lapply(1:10, \(x)list(sample(letters), sample(1:10)))
y <- lst_untree(x, margin = 0)
expect_true(
  is.list(y)
)

enumerate <- 10L
