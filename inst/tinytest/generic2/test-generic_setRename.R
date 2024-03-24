# set-up ====
source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0


# data.table ====
x <- matrix(1:260, ncol=26)
colnames(x) <- letters
x <- data.table::as.data.table(x)
names(x) <- letters
y <- x

sb2_setRename(x, old = names(x), new = rev(names(x)))
expect_equal(
  names(x),
  rev(letters)
)
sb2_setRename(x, new = letters)
expect_equal(
  names(x),
  letters
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

