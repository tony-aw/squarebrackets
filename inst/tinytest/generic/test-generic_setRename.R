# set-up ====
source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0


# vector ====
nms <- letters
x <- mutable_atomic(1:26, names = nms)
y <- x

expect_error(
  sb_setRename(x, "a"),
  pattern = "improper `newnames` given"
)
expect_error(
  sb_setRename(x, rev(letters[1:10])),
  pattern = "improper `newnames` given"
)
expect_error(
  sb_setRename(x, 1:26),
  pattern = "improper `newnames` given"
)
expect_error(
  sb_setRename(x, ~ letters),
  pattern = "improper `newnames` given"
)

sb_setRename(x, rev(nms))
expect_equal(
  names(x),
  rev(nms)
)
expect_equal(
  x, y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 7


# matrix/array  - dimnames ====
x <- mutable_atomic(
  1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters)
)
y <- x
nms <- data.table::copy(dimnames(x))

rownms <- list("a", c("a", "b"), 1:10, ~letters[1:10])
colnms <- list("a", c("a", "b"), 1:26, ~letters)
for(i in seq_along(rownms)) {
  for(j in seq_along(colnms)) {
    expect_error(
      sb_setRename(x, newdimnames = list(rownms[[i]], colnms[[j]])),
      pattern = "improper `newdimnames` given"
    ) |> errorfun()
    enumerate <- enumerate + 1
  }
}

sb_setRename(
  x,
  newdimnames = lapply(dimnames(x), rev)
)
expect_equal(
  dimnames(x),
  lapply(nms, rev)
)
expect_equal(
  x, y
)
expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 3


# matrix/array  - names ====
x <- mutable_atomic(
  1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters)
)
y <- x
nms <- sample(letters, 260, TRUE)
expect_error(
  sb_setRename(x, newnames = rev(letters[1:10])),
  pattern = "improper `newnames` given"
)
expect_error(
  sb_setRename(x, newnames = 1:26),
  pattern = "improper `newnames` given"
)
expect_error(
  sb_setRename(x, newnames = ~ letters),
  pattern = "improper `newnames` given"
)

sb_setRename(x, newnames = nms)
expect_equal(
  names(x),
  nms
)
expect_equal(
  x, y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 6


# matrix/array  - names + dimnames====
x <- mutable_atomic(
  1:260, dim = c(10, 26),
  names = sample(letters, 260, TRUE), dimnames = n(letters[1:10], letters)
)
y <- x
nms <- data.table::copy(names(x))
dimnms <- data.table::copy(dimnames(x))
sb_setRename(
  x,
  newnames = rev(nms),
  newdimnames = lapply(dimnames(x), rev)
)
expect_equal(
  names(x),
  rev(nms)
)
expect_equal(
  dimnames(x),
  lapply(dimnms, rev)
)
expect_equal(
  x, y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 4


# unname vector ====

x <- mutable_atomic(1:26, names = letters)
y <- x
sb_setRename(x, NULL)
expect_equal(
  names(x),
  NULL
)
expect_equal(
  x, y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 3



# unname matrix ====

x <- mutable_atomic(1:20, dim = c(5,4), names = letters[1:20])
y <- x
sb_setRename(x, newnames = NULL)
expect_equal(
  names(x),
  NULL
)
expect_equal(
  x, y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 3


# fully undimname matrix ====
x <- mutable_atomic(1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters))
y <- x
sb_setRename(x, newdimnames = NULL)
expect_equal(
  dimnames(x),
  NULL
)
expect_equal(
  x,y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 3

# partially undimname matrix ====

x <- mutable_atomic(1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters))
y <- x
sb_setRename(x, newdimnames = n(NULL, letters))
expect_equal(
  dimnames(x),
  n(NULL, letters)
)
expect_equal(
  x,y
)

x <- mutable_atomic(1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters))
y <- x
sb_setRename(x, newdimnames = n(letters[1:10], NULL))
expect_equal(
  dimnames(x),
  n(letters[1:10], NULL)
)
expect_equal(
  x,y
)

expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)

enumerate <- enumerate + 5


