
enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

# vector ====
x <- 1:100
n <- length(x)
expect_equal(
  tci_formula(x, 0L, ~ .M, sys.call()),
  0
)
expect_equal(
  tci_formula(x, 0L, ~ .Nms, sys.call()),
  NULL
)
names(x) <- sample(month.abb, 100, TRUE)
expect_equal(
  tci_formula(x, 0L, ~ .Nms, sys.call()),
  names(x)
)
expect_equal(
  tci_formula(x, 0L, ~ .N, sys.call()),
  n
)
expect_equal(
  tci_formula(x, 0L, ~ .I, sys.call()),
  seq_len(n)
)
expect_equal(
  tci_formula(x, 0L, ~ .bi(-1, 1), sys.call()),
  c(n, 1)
)
expect_equal(
  tci_formula(x, 0L, ~ .bi(2, -2), sys.call()),
  c(2, n - 1)
)
expect_equal(
  tci_formula(x, 0L, ~ .bi(-.I), sys.call()),
  rev(seq_len(n))
)
expect_equal(
  tci_formula(x, 0L, ~ .ptrn(c(TRUE, FALSE))),
  (1:length(x))[c(TRUE, FALSE)]
)
expect_equal(
  tci_formula(x, 0L, ~ .ptrn(c(TRUE, FALSE), 2, 99)),
  (2:99)[c(TRUE, FALSE)]
)
expect_equal(
  tci_formula(x, 0L, ~ .x, sys.call()),
  x
)


enumerate <- enumerate + 10L


# array ====

x <- array(1:prod(10:8), 10:8)
dimnames(x) <- lapply(dim(x), \(i) sample(letters, i))

for(m in 1:3) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(x, m, ~ .M, sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .Nms, sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  dimnames(x)[m] <- list(NULL)
  expect_equal(
    tci_formula(x, m, ~ .Nms, sys.call()),
    NULL
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .N, sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .I, sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-1, 1), sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(2, -2), sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-.I), sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE))),
    (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE), 2, 7)),
    (2:7)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  expect_equal(
    tci_formula(x, m, ~ .x, sys.call()),
    x
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
    tci_formula(x, m, ~ .M, sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .Nms, sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .N, sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .I, sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-1, 1), sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(2, -2), sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-.I), sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE))),
    (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE), 2, 4)),
    (2:4)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  expect_equal(
    tci_formula(x, m, ~ .x, sys.call()),
    x
  ) |> errorfun()
  
  enumerate <- enumerate + 8L
}


# zerolen vector ====
x <- integer(0L)
n <- length(x)
expect_equal(
  tci_formula(x, 0L, ~ .M, sys.call()),
  0
)
expect_equal(
  tci_formula(x, 0L, ~ .Nms, sys.call()),
  NULL
)
expect_equal(
  tci_formula(x, 0L, ~ .N, sys.call()),
  n
)
expect_equal(
  tci_formula(x, 0L, ~ .I, sys.call()),
  seq_len(n)
)
expect_equal(
  tci_formula(x, 0L, ~ .bi(-1, 1), sys.call()),
  c(n, 1) # yes, this is as expected
)
expect_equal(
  tci_formula(x, 0L, ~ .bi(2, -2), sys.call()),
  c(2, n - 1)
)
expect_equal(
  tci_formula(x, 0L, ~ .bi(-.I), sys.call()),
  integer(0L)
)

expect_equal(
  tci_formula(x, 0L, ~ .ptrn(c(TRUE, FALSE)), sys.call()),
  integer(0L)
)
expect_equal(
  tci_formula(x, 0L, ~ .ptrn(c(TRUE, FALSE), 1, 2), sys.call()),
  integer(0L)
)

expect_equal(
  tci_formula(x, 0L, ~ .x, sys.call()),
  x
)

enumerate <- enumerate + 9L


# zerolen array ====

x <- array(integer(0L), c(10, 0, 10))
dimnames(x) <- lapply(dim(x), \(i) sample(letters, i))

for(m in 1:3) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(x, m, ~ .M, sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .Nms, sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  dimnames(x)[m] <- list(NULL)
  expect_equal(
    tci_formula(x, m, ~ .Nms, sys.call()),
    NULL
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .N, sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .I, sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-1, 1), sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(2, -2), sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-.I), sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE))),
    if(n == 0L) integer(0L) else (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE), 2, 7)),
    if(n == 0L) integer(0L) else (2:7)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  expect_equal(
    tci_formula(x, m, ~ .x, sys.call()),
    x
  ) |> errorfun()
  
  enumerate <- enumerate + 9L
}



# zerolen data.frame ====

x <- data.table::data.table()

for(m in 1:2) {
  n <- dim(x)[m]
  expect_equal(
    tci_formula(x, m, ~ .M, sys.call()),
    m
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .Nms, sys.call()),
    dimnames(x)[[m]]
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .N, sys.call()),
    n
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .I, sys.call()),
    seq_len(n)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-1, 1), sys.call()),
    c(n, 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(2, -2), sys.call()),
    c(2, n - 1)
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .bi(-.I), sys.call()),
    rev(seq_len(n))
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE))),
    if(n == 0L) integer(0L) else (1:n)[c(TRUE, FALSE)]
  ) |> errorfun()
  expect_equal(
    tci_formula(x, m, ~ .ptrn(c(TRUE, FALSE), 2, 7)),
    if(n == 0L) integer(0L) else (2:7)[c(TRUE, FALSE)]
  ) |> errorfun()
  
  expect_equal(
    tci_formula(x, m, ~ .x, sys.call()),
    x
  ) |> errorfun()
  
  enumerate <- enumerate + 8L
}

