
# bind_array ====

# here, atomic and recursive matrices are mixed,
# resulting in a recursive matrix

# creating the arrays
x <- c(
  lapply(1:3, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:3, \(x)sample(1:10)),
  lapply(1:3, \(x)rnorm(10)),
  lapply(1:3, \(x)sample(letters))
)
x <- matrix(x, 4, 3, byrow = TRUE)
dimnames(x) <- n(letters[1:4], LETTERS[1:3])
print(x)

y <- matrix(1:12, 4, 3)
print(y)

# binding the arrays
arg.list <- list(x = x, y = y)
bind_array(arg.list, along = 0L) # binds on new dimension before first
bind_array(arg.list, along = 1L) # binds on first dimension
bind_array(arg.list, along = 2L)
bind_array(arg.list, along = 3L) # bind on new dimension after last



################################################################################

# bind_mat ====

# here, atomic and recursive matrices are mixed,
# resulting in a recursive matrix

x <- c(
  lapply(1:3, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:3, \(x)sample(1:10)),
  lapply(1:3, \(x)rnorm(10)),
  lapply(1:3, \(x)sample(letters))
)
x <- matrix(x, 4, 3, byrow = TRUE)
dimnames(x) <- n(letters[1:4], LETTERS[1:3])
print(x)

y <- matrix(1:12, 4, 3)
print(y)

bind_mat(n(x = x, y = y), 2L)



################################################################################

# bind_dt ====


x <- data.frame(a = 1:12, b = month.abb) # data.frame
y <- data.table::data.table(a = 1:12, b = month.abb) # data.table

bind_dt(n(x = x, y = y), 2L) # column bind

bind_dt(n(x = x, y = y), 1L) # row bind

