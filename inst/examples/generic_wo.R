
# atomic objects ====

obj <- matrix(1:16, ncol = 4)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
sb_wo(obj, n(1:3), 1:ndims(obj))
# above is equivalent to  obj[-1:-3, -1:-3, drop = FALSE]
sb_wo(obj, i = \(x) x > 5)
# above is equivalent to  obj[!obj > 5]
sb_wo(obj, n("a"), 2L)
# above is equivalent to  obj[, which(!colnames(obj) %in% "a")]

obj <- array(1:64, c(4,4,3))
print(obj)
sb_wo(obj, n(1, c(1, 3)), c(1, 3))
# above is equivalent to obj[-1, , c(-1, -3), drop = FALSE]
sb_wo(obj, i = \(x)x > 5)
# above is equivalent to obj[!obj > 5]



#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
sb2_wo(obj, "a")
# above is equivalent to obj[which(!names(obj) %in% "a")]
sb2_wo(obj, 1) # obj[-1]
sb2_wo(obj, 1:2)
# above is equivalent to obj[seq_len(length(obj))[-1:-2]]
sb2_wo(obj, is.numeric, red = TRUE)
# above is equivalent to obj[[!sapply(obj, is.numeric)]] IF this returns a single element
obj <- list(a = 1:10, b = letters[1:11], c = letters)
sb2_wo(obj, is.numeric)
# above is equivalent to obj[!sapply(obj, is.numeric)] # this time singular brackets?
# for recusive indexing, see sb2_rec()


obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
sb2_wo(obj, n(1:3), 1:ndims(obj))
# above is equivalent to obj[1:3, 1:3, drop = FALSE]
sb2_wo(obj, i = is.numeric)
# above is equivalent to obj[sapply(obj, is.numeric)]
sb2_wo(obj, n(c("a", "a")), 2L)
# above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]

obj <- array(as.list(1:64), c(4,4,3))
print(obj)
sb2_wo(obj, n(1, c(1, 3)), c(1, 3))
# above is equivalent to obj[-1, , c(-1, -3), drop = FALSE]
sb2_wo(obj, i = \(x)x>5)
# above is equivalent to obj[!sapply(obj, \(x) x > 5)]



#############################################################################

# data.frame-like objects ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
print(obj)
sb2_wo(obj, n(1:3))
# above is equivalent to obj[-1:-3, -1:-3, drop = FALSE]
sb2_wo(obj, obs = ~ (a > 5) & (c < 19), vars = is.numeric)



