
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# atomic ====

funlist <- list(
  \(x, ss, d, inv = FALSE) ss_x(x, ss, d),
  \(x, ss, d, inv = FALSE) ss_wo(x, ss, d),
  \(x, ss, d, inv = FALSE) ss_mod(x, ss, d, inv = inv, chkdup = FALSE, rp = -1)
)

for(f in funlist) {
  
  x <- array(1:120, dim = 4:6)
  
  # check dims order equivalence ====
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
    pattern = "`d` out of range",
    fixed = TRUE
  ) |> errorfun()
  
  expect_equal(
    f(x, n(1:3, 1:4, 1:5), 1:3, inv = TRUE),
    f(x, n(1:5, 1:4, 1:3), 3:1, inv = TRUE)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:3, 1:4, 1:5), 1:3, inv = TRUE),
    f(x, n(1:4, 1:5, 1:3), c(2, 3, 1), inv = TRUE)
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:4, 1:5, 1:6), 1:4, inv = TRUE),
    pattern = "`d` out of range",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 6L
  
  
  # atomic s tests ====
  expect_equal(
    f(x, n(1:4), 1),
    f(x, 1:4, 1)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:4, 1:4), 1:2),
    f(x, 1:4, 1:2)
  ) |> errorfun()
  enumerate <- enumerate + 2L
  
  
  # general error tests ====
  expect_error(
    f(x, 1:3, "a"),
    pattern = "`d` must be a integer vector"
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:3, 1:3), 1:2),
    pattern = "`length(s)` must equal `length(d)`",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    f(x, 1:3, 1:10),
    pattern = "`d` out of range"
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
  
}




# recursive ====

funlist <- list(
  \(x, ss, d, inv = FALSE) ss_x(x, ss, d),
  \(x, ss, d, inv = FALSE) ss_wo(x, ss, d),
  \(x, ss, d, inv = FALSE) ss_mod(x, ss, d, inv = inv, chkdup = FALSE, rp = list("a"))
)

for(f in funlist) {
  
  x <- c(letters, LETTERS) |> as.list()
  dim(x) <- c(26, 2)
  
  # check dims order equivalence ====
  expect_equal(
    f(x, n(1:3, 1:2), 1:2),
    f(x, n(1:2, 1:3), 2:1)
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:4, 1:5, 1:6), 1:4),
    pattern = "`d` out of range",
    fixed = TRUE
  ) |> errorfun()
  
  expect_equal(
    f(x, n(1:3, 1:2), 1:2, inv = TRUE),
    f(x, n(1:2, 1:3), 2:1, inv = TRUE)
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:4, 1:5, 1:6), 1:4, inv = TRUE),
    pattern = "`d` out of range",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 6L
  
  
  # atomic s tests ====
  expect_equal(
    f(x, n(1:2), 1),
    f(x, 1:2, 1)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:2, 1:2), 1:2),
    f(x, 1:2, 1:2)
  ) |> errorfun()
  enumerate <- enumerate + 2L
  
  
  # general error tests ====
  expect_error(
    f(x, 1:3, "a"),
    pattern = "`d` must be a integer vector"
  ) |> errorfun()
  expect_error(
    f(x, n(1:3, 1:3, 1:3), 1:2),
    pattern = "`length(s)` must equal `length(d)`",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    f(x, 1:3, 1:10),
    pattern = "`d` out of range"
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
  
}









