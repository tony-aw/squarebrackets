 
# mutable atomic vector ====
x <- y <- mutatomic(1:10, names = letters[1:10])
print(x)
sb_setFlatnames(x, newnames = rev(letters[1:10]))
print(y)

x <- y <- mutatomic(1:10, names = letters[1:10])
print(x)
sb_setFlatnames(x, 1L, "XXX")
print(y)

################################################################################


# mutable atomic matrix ====
x <- mutatomic(
  1:20, dim = c(5, 4), dimnames = n(letters[1:5], letters[1:4])
)
print(x)
sb_setDimnames(
  x,
  1:2,
  lapply(dimnames(x), rev)
)
print(x)



################################################################################



# data.table ====

x <- data.table::data.table(
  a = 1:20,
  b = letters[1:20]
)
print(x)
sb_setVarnames(x, old = names(x), new = rev(names(x)))
print(x)

