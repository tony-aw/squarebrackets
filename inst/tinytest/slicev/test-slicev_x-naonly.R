
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# regular ====

gen <- function() c(rnorm(100 - 4), NA, NaN, Inf -Inf, 1)

x.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  gen() + gen() * -1i
  
  
)
y.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  gen() + gen() * -1i
)

for(i in seq_along(x.list)) {
  x <- x.list[[i]]
  y <- y.list[[i]]
  
  x2 <- x[90:20]
  y2 <- y[90:20]
  expected <- x2[is.na(y2)]
  out <- slicev_x(x, y = y, na = NA, r = TRUE, from = 11 * -1i, to = 20)
  expect_equal(
    expected, out
  ) |> errorfun()
  expected <- x2[!is.na(y2)]
  out <- slicev_x(x, y = y, na = NA, r = FALSE, from = 11 * -1i, to = 20)
  expect_equal(
    expected, out
  ) |> errorfun()
  enumerate <- enumerate + 2L
}


