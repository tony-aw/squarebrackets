

# mutatomic objects ====

gen_mat <- function() {
  obj <- as.mutatomic(matrix(1:16, ncol = 4))
  colnames(obj) <- c("a", "b", "c", "a")
  return(obj)
}

obj <- obj2 <- gen_mat()
print(obj)

ss_set(obj, n(1:3), 1:ndim(obj), rp = -1:-9)
print(obj2)
# above is like x[1:3, 1:3] <- -1:-9, but using pass-by-reference

obj <- obj2 <- gen_mat()
obj

ii_set(obj, i = \(x) x <= 5, rp = -1:-5)
print(obj2)
# above is like x[x <= 5] <- -1:-5, but using pass-by-reference

obj <- obj2 <- gen_mat()
obj

ss_set(obj, n("a"), 2L, rp = cbind(-1:-4, -5:-8))
print(obj2)
# above is like x[, "a"] <- cbind(-1:-4, -5:-8), but using pass-by-reference

obj <- obj2 <- gen_mat()
obj

ss_set(obj, n(1:3), 1:ndim(obj), tf = \(x) -x)
print(obj2)
# above is like x[1:3, 1:3] <- -1 * x[1:3, 1:3], but using pass-by-reference

obj <- obj2 <- gen_mat()
obj

ii_set(obj, i = \(x) x <= 5, tf = \(x) -x)
print(obj2)
# above is like x[x <= 5] <- -1 * x[x <= 5], but using pass-by-reference

obj <- obj2 <- gen_mat()
obj

ss_set(obj, n("a"), 2L, tf = \(x) -x)
obj2
# above is like x[, "a"] <- -1 * x[, "a"], but using pass-by-reference


gen_array <- function() {
  as.mutatomic(array(1:64, c(4,4,3)))
}
obj <- obj2 <- gen_array()
obj

ss_set(obj, n(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
print(obj2)
# above is like x[1:3, , 1:2] <- -1:-12, but using pass-by-reference


obj <- obj2 <- gen_array()
obj
ii_set(obj, i = \(x)x <= 5, rp = -1:-5)
print(obj2)
# above is like x[x <= 5] <- -1:-5, but using pass-by-reference



#############################################################################

# data.table ====

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
ss2_set(
  obj, obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
)
print(obj)

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
dt_setcoe(obj, vars = is.numeric, v = as.numeric)
str(obj)
ss2_set(obj,
  obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt # SAFE: coercion performed by dt_setcoe(); so no warnings
) 
print(obj)

obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
ss2_set(
  obj, vars = is.numeric,
  tf = sqrt # SAFE: row=NULL & obs = NULL, so coercion performed
)
str(obj)

