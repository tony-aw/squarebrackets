
# Coercion examples - mutatomic ====

x <- as.mutatomic(1:16)
ii_set(x, i = 1:6, rp = 8.5) # 8.5 coerced to 8, because `x` is of type `integer`
print(x)

#############################################################################

# Coercion examples - data.table - whole columns ====

# sbt_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
sbt_mod(
  obj, col = is.numeric,
  tf = sqrt # SAFE: obs = NULL, so coercion performed
)

# sbt_set():
sbt_set(
  obj, col = is.numeric,
  tf = sqrt # SAFE: obs = NULL, so coercion performed
)
str(obj)

#############################################################################


# Coercion examples - data.table - partial columns ====

# sbt_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)

sbt_mod(
  obj, idx_obs(obj, ~ (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # SAFE: coercion performed
)

# sbt_set():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
sbt_set(
  obj, idx_obs(obj, ~ (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt
  # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
)
print(obj)

obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj)
sbt_set(obj, col = is.numeric, tf = as.numeric) # first coerce type by whole columns
str(obj)
sbt_set(
  obj,
  idx_obs(obj, ~ (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # SAFE: coercion performed by dt_setcoe(); so no warnings
) 
print(obj)


#############################################################################

# View of List ====

x <- list(
 a = data.table::data.table(cola = 1:10, colb = letters[1:10]),
 b = data.table::data.table(cola = 11:20, colb = letters[11:20])
)
print(x)
myref <- x$a
address(myref) == address(x$a) # they are the same
sbt_set(myref, col = "cola", tf = \(x)x^2)
print(x) # notice x has been changed

