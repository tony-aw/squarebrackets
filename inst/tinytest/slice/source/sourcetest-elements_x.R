

list_fromto <- list(
  1, -1i, 2, -2i, 10, -10i, 100, -100i, 99, -99i, 98, -98i
)
list_by <- list(
  1, 2, 3, 100, -1, -2, -3, -100
)
n <- length(list_fromto) * length(list_fromto) * length(list_by)


# without names ====
x <- mutatomic(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      expected[[counter]] <- basetest(x, from, to, by)
      out[[counter]] <- slicetest(x, from, to, by)
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
) |> errorfun()


# with names, use.names = TRUE ====
x <- mutatomic(
  1:100, dim = c(10, 10),
  names = sample(letters, 100, TRUE),
  dimnames = n(letters[1:10], letters[10:1])
)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      expected[[counter]] <- basetest(x, from, to, by)
      out[[counter]] <- slicetest(x, from, to, by)
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
) |> errorfun()


# with names, use.names = FALSE ====
x <- mutatomic(
  1:100, dim = c(10, 10),
  names = sample(letters, 100, TRUE),
  dimnames = n(letters[1:10], letters[10:1])
)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      expected[[counter]] <- basetest(x, from, to, by) |> unname()
      out[[counter]] <- slicetest(x, from, to, by, use.names = FALSE)
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
) |> errorfun()


# check sticky ====
x <- as.roman(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      expected[[counter]] <- basetest(x, from, to, by) |> unname()
      out[[counter]] <- slicetest(x, from, to, by, use.names = FALSE)
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
) |> errorfun()


# atomic types checks ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  rnorm(100),
  sample(c(NA, NaN, -Inf, Inf, 0), 100, TRUE),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  as.complex(c(1:99, NA)),
  as.raw(0:99),
  rep(NA, 100)
)
x.data <- lapply(x.data, as.mutatomic)

expected <- out <- list(8)
for(i in seq_along(x.data)) {
  x <- data.table::copy(x.data[[i]])
  
  expected[[i]] <- basetest(x, 3, -3i, -3)
  out[[i]] <- slicetest(x, 3, -3i, -3)
  
}
expect_equal(
  expected, out
) |> errorfun()

enumerate <- enumerate + length(x.data)

