
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

funlist <- list(
  \(x, ss, d = 1:ndim(x)) ci_ss(x, ss, d),
  \(x, ss, d = 1:ndim(x)) ss_x(x, ss, d),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, rp = -1),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, tf = \(x)x[1]),
  \(x, ss, d = 1:ndim(x)) {
    x <- as.mutatomic(x)
    ss_set(x, ss, d, rp = -1)
    return(x)
  },
  \(x, ss, d = 1:ndim(x)) {
    x <- as.mutatomic(x)
    ss_set(x, ss, d, tf = \(x) x[1])
    return(x)
  }
)


# multi-dim tests ====

for(f in funlist) {
  
  
  # check dims order equivalence ====
  x <- array(1:120, dim = 4:6)
  expect_equal(
    f(x, n(1:3, 1:4, 1:5), 1:3),
    f(x, n(1:5, 1:4, 1:3), 3:1)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:3, 1:4, 1:5), 1:3),
    f(x, n(1:4, 1:5, 1:3), c(2, 3, 1))
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:4, 1:5, 1:6), 1:4),
    pattern = "`use` out of range",
    fixed = TRUE
  ) |> errorfun()
  
  expect_equal(
    f(x, n(1:3, 1:4, 1:5), -1:-3),
    f(x, n(1:5, 1:4, 1:3), -3:-1)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:3, 1:4, 1:5), -1:-3),
    f(x, n(1:4, 1:5, 1:3), c(-2, -3, -1))
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:4, 1:5, 1:6), -1:-4),
    pattern = "`use` out of range",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 6L
  
  
  # atomic s tests ====
  x <- array(1:120, dim = 4:6)
  expect_equal(
    f(x, n(1:4), 1),
    f(x, 1:4, 1)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:4, 1:4), 1:2),
    f(x, 1:4, 1:2)
  ) |> errorfun()
  enumerate <- enumerate + 2L
  
  
  # check NULL/0L equivalence ====
  x <- matrix(1:20, ncol = 4)
  expect_equal(
    f(x, list(1:3, 0)),
    f(x, list(1:3, NULL))
  ) |> errorfun()
  expect_equal(
    f(x, list(0, 1:3)),
    f(x, list(NULL, 1:3))
  ) |> errorfun()
  expect_equal(
    f(x, list(1:3, 0), -Inf),
    f(x, list(1:3, NULL), -Inf)
  ) |> errorfun()
  expect_equal(
    f(x, list(0, 1:3), -Inf),
    f(x, list(NULL, 1:3), -Inf)
  ) |> errorfun()
  
  expect_equal(
    f(x, list(0)),
    f(x, list(NULL))
  ) |> errorfun()
  expect_equal(
    f(x, list(0), -Inf),
    f(x, list(NULL), -Inf)
  ) |> errorfun()
  
  expect_equal(
    f(x, 0),
    f(x, NULL)
  ) |> errorfun()
  expect_equal(
    f(x, 0, -Inf),
    f(x, NULL, -Inf)
  ) |> errorfun()
  
  enumerate <- enumerate + 8L
  
  # general error tests ====
  x <- matrix(1:20, ncol = 4)
  expect_error(
    f(x, 1:3, "a")
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:3, 1:3), 1:2),
    pattern = "`length(s)` must be 1 or equal to `length(use)`",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    f(x, 1:3, 1:10),
    pattern = "`use` out of range"
  ) |> errorfun()
  expect_error(
    f(x, 1:3, -1:-10),
    pattern = "`use` out of range"
  ) |> errorfun()
  expect_error(
    f(x, 1:3, -1:1),
    pattern = "`use` out of range"
  ) |> errorfun()
  enumerate <- enumerate + 5L
  
}


# 1d array tests ====

for(f in funlist) {
  
  x <- array(1:100)
  
  # atomic s tests ====
  expect_equal(
    f(x, n(1:4), 1),
    f(x, 1:4, 1)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:4), -1),
    f(x, 1:4, -1)
  ) |> errorfun()
  enumerate <- enumerate + 2L
  
  
  # check NULL/0L equivalence ====
  expect_equal(
    f(x, list(0)),
    f(x, list(NULL))
  ) |> errorfun()
  expect_equal(
    f(x, list(0), -1),
    f(x, list(NULL), -1)
  ) |> errorfun()
  
  expect_equal(
    f(x, 0),
    f(x, NULL)
  ) |> errorfun()
  expect_equal(
    f(x, 0, -1),
    f(x, NULL, -1)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
  # general error tests ====
  expect_error(
    f(x, 1:3, "a")
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:3), 1),
    pattern = "`length(s)` must be 1 or equal to `length(use)`",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    f(x, 1:3, 1:2),
    pattern = "`use` out of range"
  ) |> errorfun()
  expect_error(
    f(x, 1:3, -1:-2),
    pattern = "`use` out of range"
  ) |> errorfun()
  expect_error(
    f(x, 1:3, 1:-1),
    pattern = "`use` out of range"
  ) |> errorfun()
  
  enumerate <- enumerate + 5L
  
}



