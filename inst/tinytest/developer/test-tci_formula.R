
enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

# vector ====
x <- 1:100
n <- length(x)
expect_equal(
  tci_formula(~ .M, 0L, length(x), names(x), sys.call()),
  0
)
expect_equal(
  tci_formula( ~ .Nms, 0L, length(x), names(x), sys.call()),
  NULL
)
names(x) <- sample(month.abb, 100, TRUE)
expect_equal(
  tci_formula(~ .Nms, 0L, length(x), names(x), sys.call()),
  names(x)
)
expect_equal(
  tci_formula(~ .N, 0L, length(x), names(x), sys.call()),
  n
)
expect_equal(
  tci_formula(~ .I, 0L, length(x), names(x), sys.call()),
  seq_len(n)
)
expect_equal(
  tci_formula(~ .bi(-1, 1), 0L, length(x), names(x), sys.call()),
  c(n, 1)
)
expect_equal(
  tci_formula(~ .bi(2, -2), 0L, length(x), names(x), sys.call()),
  c(2, n - 1)
)
expect_equal(
  tci_formula(~ .bi(-.I), 0L, length(x), names(x), sys.call()),
  rev(seq_len(n))
)
expect_equal(
  tci_formula(~ .ptrn(c(TRUE, FALSE)), 0L, length(x), names(x), sys.call()),
  (1:length(x))[c(TRUE, FALSE)]
)
expect_equal(
  tci_formula(~ .ptrn(c(TRUE, FALSE), 2, 99), 0L, length(x), names(x), sys.call()),
  (2:99)[c(TRUE, FALSE)]
)


enumerate <- enumerate + 10L


# array ====
x <- array(1:prod(10:8), 10:8)
dimnames(x) <- lapply(dim(x), \(i) sample(letters, i))

for(m in 1:3) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(~ .M, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .Nms, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  dimnames(x)[m] <- list(NULL)
  expect_equal(
    tci_formula(~ .Nms, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    NULL
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .N, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .I, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-1, 1), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(2, -2), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-.I), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE)), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE), 2, 7), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    (2:7)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  
  enumerate <- enumerate + 9L
}


# data.frame ====

x <- data.table::data.table(
  a = as.raw(0:99),
  b = sample(c(TRUE, FALSE, NA), 100L, TRUE),
  c = c(NA, 1:99),
  d = c(NA, NaN, Inf, -Inf, rnorm(96)),
  e = c(NA, NaN, Inf, -Inf, rnorm(96)) + c(NA, NaN, Inf, -Inf, rnorm(96)) * -1i,
  f = sample(c(month.name, NA), 100, TRUE)
)

for(m in 1:2) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(~ .M, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .Nms, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .N, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .I, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-1, 1), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(2, -2), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-.I), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE)), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE), 2, 4), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    (2:4)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  enumerate <- enumerate + 8L
}


# zerolen vector ====
x <- integer(0L)
n <- length(x)
expect_equal(
  tci_formula(~ .M, 0L, length(x), names(x), sys.call()),
  0
)
expect_equal(
  tci_formula(~ .Nms, 0L, length(x), names(x), sys.call()),
  NULL
)
expect_equal(
  tci_formula(~ .N, 0L, length(x), names(x), sys.call()),
  n
)
expect_equal(
  tci_formula(~ .I, 0L, length(x), names(x), sys.call()),
  seq_len(n)
)
expect_equal(
  tci_formula(~ .bi(-1, 1), 0L, length(x), names(x), sys.call()),
  c(n, 1) # yes, this is as expected
)
expect_equal(
  tci_formula(~ .bi(2, -2), 0L, length(x), names(x), sys.call()),
  c(2, n - 1)
)
expect_equal(
  tci_formula(~ .bi(-.I), 0L, length(x), names(x), sys.call()),
  integer(0L)
)

expect_equal(
  tci_formula(~ .ptrn(c(TRUE, FALSE)), 0L, length(x), names(x), sys.call()),
  integer(0L)
)
expect_equal(
  tci_formula(~ .ptrn(c(TRUE, FALSE), 1, 2), 0L, length(x), names(x), sys.call()),
  integer(0L)
)

enumerate <- enumerate + 9L


# zerolen array ====

x <- array(integer(0L), c(10, 0, 10))
dimnames(x) <- lapply(dim(x), \(i) sample(letters, i))

for(m in 1:3) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(~ .M, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .Nms, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  dimnames(x)[m] <- list(NULL)
  expect_equal(
    tci_formula(~ .Nms, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    NULL
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .N, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .I, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-1, 1), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(2, -2), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-.I), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE)), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    if(n == 0L) integer(0L) else (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE), 2, 7), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    if(n == 0L) integer(0L) else (2:7)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  
  enumerate <- enumerate + 9L
}



# zerolen data.frame ====

x <- data.table::data.table()

for(m in 1:2) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(~ .M, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .Nms, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .N, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .I, m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-1, 1), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(2, -2), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .bi(-.I), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE)), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    if(n == 0L) integer(0L) else (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(~ .ptrn(c(TRUE, FALSE), 2, 7), m, dim(x)[m], dimnames(x)[[m]], sys.call()),
    if(n == 0L) integer(0L) else (2:7)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  enumerate <- enumerate + 8L
}

