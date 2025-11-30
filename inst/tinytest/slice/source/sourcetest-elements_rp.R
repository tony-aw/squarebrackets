

list_fromto <- list(
  1, 100, 2, 99, 10, 90, 100, 1, 99, 2, 98, 3
)
list_by <- list(
  1, 2, 3, 100, -1, -2, -3, -100
)
n <- length(list_fromto) * length(list_fromto) * length(list_by)



# equal length replacement ====
x <- mutatomic(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      rp <- subset_fun(x, from, to, by)
      expected[[counter]] <- basetest(x, from, to, by, tf = \(x) as.integer(x^2))
      out[[counter]] <- slicetest(x, from, to, by, tf = \(x) x^2)
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
) |> errorfun()


# singular replacement ====

x <- mutatomic(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      rp <- 1L
      expected[[counter]] <- basetest(x, from, to, by, tf = \(x) x[1])
      out[[counter]] <- slicetest(x, from, to, by, tf = \(x) x[1])
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
)  |> errorfun()



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
  
  rp <- rev(subset_fun(x, 3, 98, -3))
  expected[[i]] <- basetest(x, 3, 98, -3, tf = rev)
  out[[i]] <- slicetest(x, 3, 98, -3, tf = rev)
  
}
expect_equal(
  expected, out
)  |> errorfun()

enumerate <- enumerate + length(x.data)

