
# Coercion examples - mutatomic ====

x <- as.mutatomic(1:16)
ii_set(x, i = 1:6, rp = 8.5) # 8.5 coerced to 8, because `x` is of type `integer`
print(x)

#############################################################################

# Coercion examples - data.table - whole columns ====

# tt_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
tt_mod(
  obj, col = is.numeric,
  tf = sqrt # SAFE: obs = NULL, so coercion performed
)

# tt_set():
tt_set(
  obj, col = is.numeric,
  tf = sqrt # SAFE: obs = NULL, so coercion performed
)
str(obj)

#############################################################################


# Coercion examples - data.table - partial columns ====

# tt_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)

tt_mod(
  obj, with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt # SAFE: coercion performed
)

# tt_set():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
tt_set(
  obj, with(obj,  (a >= 2) & (c <= 17)), is.numeric,
  tf = sqrt
  # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
)
print(obj)

obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj)
tt_set(obj, col = is.numeric, tf = as.numeric) # first coerce type by whole columns
str(obj)
tt_set(
  obj,
  with(obj,  (a >= 2) & (c <= 17)), is.numeric,
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
tt_set(myref, col = "cola", tf = \(x)x^2)
print(x) # notice x has been changed

