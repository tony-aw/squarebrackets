
# Coercion examples - mutatomic ====

x <- as.mutatomic(1:16)
i_set(x, i = 1:6, rp = 8.5) # 8.5 coerced to 8, because `x` is of type `integer`
print(x)

#############################################################################

# Coercion examples - data.table - whole columns ====

# ss2_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
ss2_mod(
  obj, vars = is.numeric,
  tf = sqrt # SAFE: row=NULL & obs = NULL, so coercion performed
)

# ss2_set():
ss2_set(
  obj, vars = is.numeric,
  tf = sqrt # SAFE: row=NULL & obs = NULL, so coercion performed
)
str(obj)

#############################################################################


# Coercion examples - data.table - partial columns ====

# ss2_mod():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)

ss2_mod(
  obj, obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt # SAFE: coercion performed
)

# ss2_set():
obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
ss2_set(
  obj, obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
  tf = sqrt
  # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
)
print(obj)

obj <- data.table::data.table(
  a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
)
str(obj)
dt_setcoe(obj, vars = is.numeric, v = as.numeric)
str(obj)
ss2_set(obj,
  obs = ~ (a >= 2) & (c <= 17), vars = is.numeric,
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
ss2_set(myref, vars = "cola", tf = \(x)x^2)
print(x) # notice x has been changed


