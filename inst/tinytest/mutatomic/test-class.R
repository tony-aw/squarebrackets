
# set-up ====

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


# is.mutatomic ====

for(i in seq_along(x.data)) {
  x <- x.data[[i]]
  expect_false(
    is.mutatomic(x)
  ) |> errorfun()
  x <- as.mutatomic(x)
  expect_true(
    is.mutatomic(x)
  ) |> errorfun()
  enumerate <- enumerate + 2L
  
}

x <- factor(letters)
class(x) <- "mutatomic"
attr(x, 'typeof') <- typeof(x)
expect_false(
  is.mutatomic(x)
)

enumerate <- enumerate + 1L


# couldb.mutatomic ====
for(i in seq_along(x.data)) {
  x <- x.data[[i]]
  expect_true(
    couldb.mutatomic(x)
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}



# as.mutatomic vs mutatomic - vector ====

for(i in seq_along(x.data)) {
  x <- x.data[[i]]
  n <- length(x)
  expect_equal(
    as.mutatomic(x),
    mutatomic(x)
  ) |> errorfun()
  names(x) <- letters[1:n]
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, names = letters[1:n])
  ) |> errorfun()
  enumerate <- enumerate + 2L
  
}




# as.mutatomic vs mutatomic - matrix ====
for(i in seq_along(x.data)) {
  x <- matrix(x.data[[i]][1:20], ncol = 4, nrow = 5)
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, dim = c(5, 4))
  ) |> errorfun()
  dimnames(x) <- list(NULL, letters[1:4])
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, dim = c(5, 4), dimnames = list(NULL, letters[1:4]))
  ) |> errorfun()
  
  x <- matrix(x.data[[i]][1:20], ncol = 4)
  names(x) <- letters[1:20]
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, dim = c(5, 4), names = letters[1:20])
  ) |> errorfun()
  dimnames(x) <- list(NULL, letters[1:4])
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, dim = c(5, 4), names = letters[1:20], dimnames = list(NULL, letters[1:4]))
  ) |> errorfun()
  
  
  enumerate <- enumerate + 4L
  
}



# as.mutatomic vs mutatomic - array ====

for(i in seq_along(x.data)) {
  
  x <- array(x.data[[i]][1:27], dim = c(3,3,3))
  dimnames(x) <- list(NULL, NULL, letters[1:3])
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, dim = c(3,3,3), dimnames = list(NULL, NULL, letters[1:3]))
  ) |> errorfun()
  names(x) <- c(letters, NA)
  expect_equal(
    as.mutatomic(x),
    mutatomic(x, dim = c(3,3,3), dimnames = list(NULL, NULL, letters[1:3]), names = c(letters, NA))
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}



# as.mutatomic vs mutatomic vs materialize_atomic - ALTREP ====
x <- 1:1e6
is_altrep <- squarebrackets:::.C_is_altrep
expect_true(is_altrep(x))
expect_false(as.mutatomic(x) |> is_altrep())
expect_false(mutatomic(x) |> is_altrep())
enumerate <- enumerate + 2L


# materialize ====
x <- 1:10
expect_true(
  squarebrackets:::.C_is_altrep(x)
)
x <- as.mutatomic(x)
expect_false(
  squarebrackets:::.C_is_altrep(x)
)
expect_true(
  is.mutatomic(x)
)

x <- 1:10
expect_true(
  squarebrackets:::.C_is_altrep(x)
)
x <- mutatomic(x)
expect_false(
  squarebrackets:::.C_is_altrep(x)
)
expect_true(
  is.mutatomic(x)
)
enumerate <- enumerate + 6L



# errors ====

expect_error(
  mutatomic(list(1:10)),
  pattern = "non-atomic or non-convertible data given"
)
expect_error(
  mutatomic(as.factor(1:10)),
  pattern = "non-atomic or non-convertible data given"
)
expect_error(
  as.mutatomic(list(1:10)),
  "not atomic or not convertible"
)
expect_error(
  as.mutatomic(factor(letters)),
  "not atomic or not convertible"
)

enumerate <- enumerate + 4

