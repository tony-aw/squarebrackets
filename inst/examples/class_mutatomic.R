
x <- mutatomic(
  1:20, dim = c(5, 4), dimnames = list(letters[1:5], letters[1:4])
)
x

x <- matrix(1:10, ncol = 2)
x <- as.mutatomic(x)
is.mutatomic(x)
print(x)
x[, 1]
x[] <- as.double(x)
print(x)
is.mutatomic(x)
