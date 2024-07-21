
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0

# m = 0L ====

for(i in 1:10) {
  x <- sample(1:20)
  r <- setNames(seq_along(x), names(x))
  grp <- factor(sample(letters[1:20]))
  expect_equal(
    idx_by(x, 0L, head, grp) |> as.integer(),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE) |> as.integer()
  ) |> errorfun()
  x <- letters[1:20]
  r <- setNames(seq_along(x), names(x))
  expect_equal(
    idx_by(x, 0, head, grp),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE)
  ) |> errorfun()
  enumerate <- enumerate + 2
}


# m = 1L ====

for(i in 1:10) {
  x <- matrix(sample(1:25), ncol = 5)
  r <- seq_len(nrow(x))
  grp <- factor(sample(letters[1:5]))
  expect_equal(
    idx_by(x, 1L, head, grp) |> as.integer(),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE) |> as.integer()
  ) |> errorfun()
  
  x <- matrix(sample(letters[1:25]), ncol = 5)
  r <- seq_len(nrow(x))
  expect_equal(
    idx_by(x, 1L, head, grp),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE)
  ) |> errorfun()
  enumerate <- enumerate + 2
  
  x <- matrix(sample(1:25), ncol = 5) |> as.data.frame()
  r <- seq_len(nrow(x))
  grp <- factor(sample(letters[1:5]))
  expect_equal(
    idx_by(x, 1L, head, grp) |> as.integer(),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE) |> as.integer()
  ) |> errorfun()
  
  x <- matrix(sample(letters[1:25]), ncol = 5) |> as.data.frame()
  r <- seq_len(nrow(x))
  expect_equal(
    idx_by(x, 1L, head, grp),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE)
  ) |> errorfun()
  enumerate <- enumerate + 2
}


# m = 2L ====

for(i in 1:10) {
  x <- matrix(sample(1:25), ncol = 5)
  r <- seq_len(ncol(x))
  grp <- factor(sample(letters[1:5]))
  expect_equal(
    idx_by(x, 2L, head, grp) |> as.integer(),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE) |> as.integer()
  ) |> errorfun()
  
  x <- matrix(sample(letters[1:25]), ncol = 5)
  r <- seq_len(ncol(x))
  expect_equal(
    idx_by(x, 2L, head, grp),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE)
  ) |> errorfun()
  enumerate <- enumerate + 2
  
  
  x <- matrix(sample(1:25), ncol = 5) |> as.data.frame()
  r <- seq_len(ncol(x))
  grp <- factor(sample(letters[1:5]))
  expect_equal(
    idx_by(x, 2L, head, grp) |> as.integer(),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE) |> as.integer()
  ) |> errorfun()
  
  x <- matrix(sample(letters[1:25]), ncol = 5) |> as.data.frame()
  r <- seq_len(ncol(x))
  expect_equal(
    idx_by(x, 2L, head, grp),
    tapply(r, grp, head, simplify = FALSE) |> unlist(use.names = TRUE)
  ) |> errorfun()
  enumerate <- enumerate + 2
}
