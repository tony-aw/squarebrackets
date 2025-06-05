# set-up ====
source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0


# set flatnames ====
nms <- letters
x <- mutatomic(1:26, names = nms)
y <- x

expect_error(
  sb_setFlatnames(x, newnames = "a"),
  pattern = "`newnames` of wrong length"
)
expect_error(
  sb_setFlatnames(x, newnames = rev(letters[1:10])),
  pattern = "`newnames` of wrong length"
)
expect_error(
  sb_setFlatnames(x, newnames = 1:26),
  pattern = "`newnames` must be a character vector"
)
expect_error(
  sb_setFlatnames(x, newnames = ~ letters),
  pattern = "`newnames` must be a character vector"
)

sb_setFlatnames(x, newnames = rev(nms))
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


# partially set flatnames ====
nms <- letters
x <- mutatomic(1:26, names = nms)
y <- x

expect_error(
  sb_setFlatnames(x, i = 1:2, newnames = "a"),
  pattern = "`newnames` of wrong length"
)
expect_error(
  sb_setFlatnames(x, i = 1:27, newnames = c(letters[1:10], NA)),
)
expect_error(
  sb_setFlatnames(x, i = 1:26, newnames = 1:26),
  pattern = "`newnames` must be a character vector"
)
expect_error(
  sb_setFlatnames(x, i = 1, newnames = ~ letters),
  pattern = "`newnames` must be a character vector"
)

iList <- list(
  c(rep(TRUE, 10), rep(FALSE, 16)),
  1:10,
  26:17 * -1i,
  letters[1:10]
)

result <- c(rev(nms[1:10]), nms[11:26])
for(i in seq_along(iList)) {
  sb_setFlatnames(x, i = 1:10, newnames = rev(nms[1:10]))
  
  expect_equal(
    names(x),
    result
  ) |> errorfun()
  
  expect_equal(
    x, y
  ) |> errorfun()
  
  expect_equal( # check that the original base::letters has NOT been modified
    letters,
    c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
      "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  ) |> errorfun()
  
  enumerate <- enumerate + 3
  
}



# set dimnames ====
x <- mutatomic(
  1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters)
)
y <- x
nms <- data.table::copy(dimnames(x))

rownms <- list("a", c("a", "b"), 1:10, ~letters[1:10])
colnms <- list("a", c("a", "b"), 1:26, ~letters)
for(i in seq_along(rownms)) {
  for(j in seq_along(colnms)) {
    expect_error(
      sb_setDimnames(x, 1:2, newdimnames = list(rownms[[i]], colnms[[j]])),
      pattern = "improper `newdimnames` given"
    ) |> errorfun()
    enumerate <- enumerate + 1
  }
}
expect_error(
  sb_setDimnames(x, 1:2, letters),
  pattern = "`newdimnames` must be a list of the same length as `m`"
)

sb_setDimnames(
  x,
  1:2,
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


enumerate <- enumerate + 6



# partially set dimnames ====
x <- y <- mutatomic(
  1:(26*4), dim = c(26, 4), dimnames = n(letters, letters[1:4])
)
print(x)

expect_error(
  sb_setDimnames(x, 1:3, n(rev(rownames(x)), letters[1:4], "a")),
  pattern = "improper `m` given"
)

sb_setDimnames(
  x,
  1L,
  n(rev(rownames(x)))
)
expect_equal(
  rownames(x),
  rev(letters)
)
expect_equal(
  x, y
)
expect_equal( # check that the original base::letters has NOT been modified
  letters,
  c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
)



# un-flatname ====

x <- mutatomic(1:26, names = letters)
y <- x
sb_setFlatnames(x, newnames =  NULL)
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




# un-dimname ====
x <- mutatomic(1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters))
y <- x
sb_setDimnames(x, newdimnames = NULL)
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



# partially un-dimname ====

x <- mutatomic(1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters))
y <- x
sb_setDimnames(x, 1L, newdimnames = n(NULL))
expect_equal(
  dimnames(x),
  n(NULL, letters)
)
expect_equal(
  x,y
)

x <- mutatomic(1:260, dim = c(10, 26), dimnames = n(letters[1:10], letters))
y <- x
sb_setDimnames(x, 2L, newdimnames = n(NULL))
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


