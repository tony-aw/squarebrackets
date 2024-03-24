
# Coercion examples - lists ====
x <- list(factor(letters), factor(letters))
print(x)
sb2_mod(x, 1, rp = list(1)) # first element fully changed.

x <- list(1:10, 1:10)
print(x)
sb2_rec(x, 1, rp = "a") # coerces first element to character
print(x)


#############################################################################


# Coercion examples - data.frame-like - whole columns ====

obj <- data.frame(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
sb2_mod(
  obj, vars = is.numeric,
  tf = sqrt # SAFE: row=NULL & filter = NULL, so coercion performed
)

#############################################################################


# Coercion examples - data.frame-like - partial columns ====

# sb_mod():
obj <- data.frame(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)

sb2_mod(
  obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
)
sb2_mod(
  obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  coe = as.double, tf = sqrt # SAFE: coercion performed
)



