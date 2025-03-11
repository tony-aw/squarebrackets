
# atomic objects ====

obj <- matrix(1:16, ncol = 4)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
rp <- -1:-9
sb_mod(obj, n(1:3), 1:ndim(obj), rp = rp)
# above is equivalent to  obj[1:3, 1:3] <- -1:-9; obj
sb_mod(obj, i = \(x)x<=5, rp = -1:-5)
# above is equivalent to  obj[obj <= 5] <- -1:-5; obj
sb_mod(obj, n("a"), 2L, rp = -1:-8)
# above is equivalent to  obj[, which(colnames(obj) %in% "a")] <- -1:-8; obj
sb_mod(obj, n(1:3), 1:ndim(obj), tf = \(x) -x)
# above is equivalent to  obj[1:3, 1:3] <- (-1 * obj[1:3, 1:3]); obj
sb_mod(obj, i = \(x)x <= 5, tf = \(x) -x)
# above is equivalent to  obj[obj <= 5] <- (-1 * obj[obj <= 5]); obj

obj <- array(1:64, c(4,4,3))
print(obj)
sb_mod(obj, n(1:3, 1:2), c(1,3), rp = -1:-24)
# above is equivalent to obj[1:3, , 1:2] <- -1:-24
sb_mod(obj, i = \(x)x <= 5, rp = -1:-5)
# above is equivalent to obj[obj <= 5] <- -1:-5

#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
sb2_mod(obj, "a", rp = list(1L))
# above is equivalent to  obj[["a"]] <- 1L; obj
sb2_mod(obj, is.numeric, rp = list(-1:-10, -11:-20))
# above is equivalent to  obj[which(sapply(obj, is.numeric))] <- list(-1:-10, -11:-20); obj

obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
sb2_mod(obj, n(1:3), 1:ndim(obj),rp = n(-1))
# above is equivalent to obj[1:3, 1:3] <- list(-1)
sb2_mod(obj, i = is.numeric, rp = n(-1))
# above is equivalent to obj[sapply(obj, is.numeric)] <- list(-1)
sb2_mod(obj, n("a"), 2L, rp = n(-1))
# above is equivalent to
# obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()] <- list(-1)


obj <- array(as.list(1:64), c(4,4,3))
print(obj)
sb2_mod(obj, n(1:3, 1:2), c(1,3), rp = as.list(-1:-24))
# above is equivalent to obj[1:3, , 1:2] <- as.list(-1:-24)
sb2_mod(obj, i = \(x) x <= 5, rp = as.list(-1:-5))
# above is equivalent to obj[sapply(onj, \(x) x <= 5)] <- as.list(-1:-5)


#############################################################################

# data.frame-like objects  - whole columns ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
sb2_mod(
  obj, vars = is.numeric,
  tf = sqrt
)

#############################################################################

# data.frame-like objects  - partial columns ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)

sb2_mod(
  obj, obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt
) 
sb2_mod(
  obj, obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt
) 
sb2_mod(
  obj, obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt
) 




