
# atomic objects ====

gen_mat <- function() {
  obj <- matrix(1:16, ncol = 4)
  colnames(obj) <- c("a", "b", "c", "a")
  return(obj)
}

obj <- obj2 <- gen_mat()
print(obj)

ss_mod(obj, n(1:3), 1:ndim(obj), rp = -1:-9)
print(obj2)
# above is like x[1:3, 1:3] <- -1:-9

obj <- obj2 <- gen_mat()
obj

ss_mod(obj, n("a"), 2L, rp = cbind(-1:-4, -5:-8))
print(obj2)
# above is like x[, "a"] <- cbind(-1:-4, -5:-8)

obj <- obj2 <- gen_mat()
obj

ss_mod(obj, n(1:3), 1:ndim(obj), tf = \(x) -x)
print(obj2)
# above is like x[1:3, 1:3] <- -1 * x[1:3, 1:3]

obj <- obj2 <- gen_mat()
obj

ss_mod(obj, "a", 2L, tf = \(x) -x)
obj2
# above is like x[, "a"] <- -1 * x[, "a"]


gen_array <- function() {
  as.mutatomic(array(1:64, c(4,4,3)))
}
obj <- obj2 <- gen_array()
obj

ss_mod(obj, n(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
print(obj2)
# above is like x[1:3, , 1:2] <- -1:-12



#############################################################################


# lists ====

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
print(obj)
ii_mod(obj, "a", rp = list(1L))
print(obj)
# above is equivalent to  obj[["a"]] <- 1L; obj

obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
ii_mod(obj, is.numeric, rp = list(-1:-10, -11:-20))
print(obj)
# above is equivalent to  obj[which(sapply(obj, is.numeric))] <- list(-1:-10, -11:-20); obj

obj <- rbind(
  lapply(1:4, \(x)sample(c(TRUE, FALSE, NA))),
  lapply(1:4, \(x)sample(1:10)),
  lapply(1:4, \(x)rnorm(10)),
  lapply(1:4, \(x)sample(letters))
)
colnames(obj) <- c("a", "b", "c", "a")
print(obj)
ss_mod(obj, n(1:3), 1:ndim(obj),rp = n(-1))
print(obj)
# above is equivalent to obj[1:3, 1:3] <- list(-1)
ii_mod(obj, is.numeric, rp = n(-1))
print(obj)
# above is equivalent to obj[sapply(obj, is.numeric)] <- list(-1)
ss_mod(obj, n("a"), 2L, rp = n(-1))
print(obj)
# above is equivalent to
# obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()] <- list(-1)


obj <- array(as.list(1:64), c(4,4,3))
print(obj)
ss_mod(obj, n(1:3, 1:2), c(1,3), rp = as.list(-1:-24))
print(obj)
# above is equivalent to obj[1:3, , 1:2] <- as.list(-1:-24)

obj <- array(as.list(1:64), c(4,4,3))
ii_mod(obj, i = \(x) x <= 5, rp = as.list(-1:-5))
print(obj)
# above is equivalent to obj[sapply(onj, \(x) x <= 5)] <- as.list(-1:-5)


#############################################################################

# data.frame-like objects  - whole columns ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
tt_mod(
  obj, col = is.numeric,
  tf = sqrt
)

#############################################################################

# data.frame-like objects  - partial columns ====

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)

tt_mod(
  obj, with(obj,  (a > 2) & (c < 17)), is.numeric,
  tf = sqrt
) 
tt_mod(
  obj, with(obj,  (a > 2) & (c < 17)), is.numeric,
  tf = sqrt
) 
tt_mod(
  obj, with(obj,  (a > 2) & (c < 17)), is.numeric,
  tf = sqrt
) 




