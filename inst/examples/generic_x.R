

# atomic objects ====

obj <- matrix(1:16, ncol = 4)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss_x(obj, s = n(1:3), d = 1:ndim(obj))
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
i_x(obj, i = \(x) x > 5)
# above is equivalent to obj[obj > 5]
ss_x(obj, s = n(c("a", "a")), d = 2L)
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(1:64, c(4,4,3))
print(obj)
ss_x(obj, s = n(1:3, 1:2), d = c(1,3))
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
i_x(obj, i = \(x)x > 5)
# above is equivalent to obj[obj > 5]


#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
i2_x(obj, 1) # obj[1]
i2_x(obj, 1, red = TRUE) # obj[[1]]
i2_x(obj, 1:2) # obj[1:2]
i2_x(obj, is.numeric) # obj[sapply(obj, is.numeric)]
# for recursive subsets, see lst_rec()


obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss2_x(obj, s = n(1:3), d = 1:ndim(obj))
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
i2_x(obj, i = is.numeric)
# above is equivalent to obj[sapply(obj, is.numeric)]
ss2_x(obj, s = n(c("a", "a")), d = 2L)
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(as.list(1:64), c(4,4,3))
print(obj)
ss2_x(obj, s = n(1:3, 1:2), d = c(1,3))
# above is equivalent to obj[1:3, , 1:2, drop = FALSE]
i2_x(obj, i = \(x)x > 5)
# above is equivalent to obj[sapply(obj, \(x) x > 5)]

#############################################################################

# data.frame-like objects ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
print(obj)
ss2_x(obj, n(1:3)) # obj[1:3, 1:3, drop = FALSE]
ss2_x(obj, obs = ~ (a > 5) & (c < 19), vars = is.numeric)


