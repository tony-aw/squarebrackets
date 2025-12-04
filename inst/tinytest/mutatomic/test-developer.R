

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

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

testfun1 <- function(x) {
  .internal_set_ma(x)
}



nms <- list(NULL, letters[1:10])
dms <- list(NULL, c(2, 5))
dmnms <- list(
  NULL,
  list(month.abb[1:2], NULL),
  list(NULL, month.name[1:5]),
  list(month.abb[1:2], month.name[1:5])
)

for(i in seq_along(x.data)) {
  x <- data.table::copy(x.data[[i]])
  expect_false(
    is.mutatomic(x)
  ) |> errorfun()
  
  testfun1(x)
  invisible(x)
  
  expect_true(
    is.mutatomic(x)
  ) |> errorfun()
}



# errors ====

x <- list(1:10)
expect_error(
  testfun1(x),
  pattern = "not atomic or not convertible"
)
expect_error(
  testfun1(as.factor(1:10)),
  pattern = "not atomic or not convertible"
)
x <- 1:10
enumerate <- enumerate + 2L

