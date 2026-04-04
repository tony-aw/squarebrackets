

list_fromto <- list(
  1, 100, 2, 99, 10, 90, 100, 1, 99, 2, 98, 3
)
list_patternlen <- list(
  2, 3, 4, 100
)
n <- length(list_fromto) * length(list_fromto) * length(list_patternlen)



# equal length replacement ====
x <- mutatomic(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_patternlen)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_patternlen[[iB]]
      pattern = rep(c(TRUE, FALSE), ceiling(by/2)) |> sample(by)
      if(length(pattern) <= length(from:to)) {
        
        rp <- subset_fun(x, from, to, pattern)
        expected[[counter]] <- basetest(x, from, to, pattern, tf = \(x) as.integer(x^2))
        out[[counter]] <- slicetest(x, from, to, pattern, tf = \(x) x^2)
        counter <- counter + 1L
        enumerate <- enumerate + 1L
      }
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
    for(iB in seq_along(list_patternlen)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_patternlen[[iB]]
      pattern = rep(c(TRUE, FALSE), ceiling(by/2)) |> sample(by)
      if(length(pattern) <= length(from:to)) {
        rp <- 1L
        expected[[counter]] <- basetest(x, from, to, pattern, tf = \(x) x[1])
        out[[counter]] <- slicetest(x, from, to, pattern, tf = \(x) x[1])
        counter <- counter + 1L
        enumerate <- enumerate + 1L
      }
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
  
  rp <- rev(subset_fun(x, 3, 98, c(TRUE, FALSE, FALSE, TRUE)))
  expected[[i]] <- basetest(x, 3, 98, c(TRUE, FALSE, FALSE, TRUE), tf = rev)
  out[[i]] <- slicetest(x, 3, 98, c(TRUE, FALSE, FALSE, TRUE), tf = rev)
  
}
expect_equal(
  expected, out
)  |> errorfun()

enumerate <- enumerate + length(x.data)

