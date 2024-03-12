
# set-up ====
source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0

a <- as.mutable_atomic(1:10)
refs <- paste0("ref_", letters)
for(i in refs) {
  assign(i, a, envir = environment())
}
`1234567890!@#$%^&*()` <- a

mynms <- expand.grid("random_", letters, letters) |> as.matrix()
mynms <- apply(mynms, 1, \(x)paste(x, collapse = ""))
length(mynms)
for(i in mynms) {
  assign(i, rpois(1, 10), envir = environment())
}


# sb_set gives error when modifying locked objects ====
sb_currentBindings(a)
sb_currentBindings(a, action = "lockbindings")
expect_true(all(sb_currentBindings(a, action = "checklock")))


expect_error(
  sb_set(a, i = 1, rp = -1),
  pattern = "object is locked"
)
expect_error(
  sb_set.default(a, i = 1, rp = -1),
  pattern = "object is locked"
)
mymat <- as.mutable_atomic(matrix(1:10, ncol = 2))
lockBinding("mymat", env = environment())
expect_error(
  sb_set.matrix(mymat, i = 1, rp = -1)
)
myarr <- as.mutable_atomic(array(1:27, dim = c(3,3,3)))
lockBinding("myarr", env = environment())
expect_error(
  sb_set.array(myarr, i = 1, rp = -1),
  pattern = "object is locked"
)

mydt <- data.table::data.table(a = 1:10, b = letters[1:10])
lockBinding("mydt", env = environment())
expect_error(
  sb_set.data.table(mydt, col = "a", tf = \(x)x^2),
  pattern = "object is locked"
)
rm(list = c("mymat", "myarr", "mydt"), envir = environment())
enumerate <- enumerate + 7


# clean-up ====
rm(list = c(refs, mynms, "1234567890!@#$%^&*()", "i", "a", "refs", "mynms"),
   envir = environment())
