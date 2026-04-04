
# NOT RUN IN CRAN

x <- sample(as.raw(0:255))
x <- rep_len(x, 2^31 + 10L)

# pv ====
expected <- x[.test_v2ind(1L, x, as.raw(128), 1L)]
out <- long_x(x, stride_pv(x, as.raw(128)))
expect_equal(
  expected, out
)

# seq ====
expected <- x[seq(2, 2^31 + 9L, 2)]
out <- long_x(x, stride_seq(2, 2^31 + 9L, 2L))

# ptrn ====
expected <- x[(2:(2^31 + 9))[c(TRUE, FALSE, FALSE, TRUE)]]
out <- long_x(x, stride_prn(2, 2^31 + 9,  c(TRUE, FALSE, FALSE, TRUE)))
expect_equal(
  expected, out
) |> errorfun()
