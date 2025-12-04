
# atomic objects ====

obj <- matrix(1:16, ncol = 4)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss_wo(obj, n(1:3), 1:ndim(obj))
# above is equivalent to  obj[-1:-3, -1:-3, drop = FALSE]
ii_wo(obj, i = \(x) x > 5)
# above is equivalent to  obj[!obj > 5]
ss_wo(obj, n("a"), 2L)
# above is equivalent to  obj[, which(!colnames(obj) %in% "a")]

obj <- array(1:64, c(4,4,3))
print(obj)
ss_wo(obj, n(1, c(1, 3)), c(1, 3))
# above is equivalent to obj[-1, , c(-1, -3), drop = FALSE]
ii_wo(obj, i = \(x)x > 5)
# above is equivalent to obj[!obj > 5]



#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
ii_wo(obj, "a")
# above is equivalent to obj[which(!names(obj) %in% "a")]
ii_wo(obj, 1) # obj[-1]
ii_wo(obj, 1:2)
# above is equivalent to obj[seq_len(length(obj))[-1:-2]]
obj <- list(a = 1:10, b = letters[1:11], c = letters)
ii_wo(obj, is.numeric)
# above is equivalent to obj[!sapply(obj, is.numeric)] # this time singular brackets?
# for recusive indexing, see lst_rec()


obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss_wo(obj, n(1:3), 1:ndim(obj))
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
ii_wo(obj, i = is.numeric)
# above is equivalent to obj[sapply(obj, is.numeric)]
ss_wo(obj, n(c("a", "a")), 2L)
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(as.list(1:64), c(4,4,3))
print(obj)
ss_wo(obj, n(1, c(1, 3)), c(1, 3))
# above is equivalent to obj[-1, , c(-1, -3), drop = FALSE]
ii_wo(obj, i = \(x)x>5)
# above is equivalent to obj[!sapply(obj, \(x) x > 5)]



#############################################################################

# data.frame-like objects ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
print(obj)
sbt_wo(obj, 1:3, 1:3)
# above is equivalent to obj[-1:-3, -1:-3, drop = FALSE]
sbt_wo(obj, ~ (a > 5) & (c < 19), is.numeric)



