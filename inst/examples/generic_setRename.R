 
# mutable atomic vector ====
x <- y <- mutable_atomic(1:10, names = letters[1:10])
sb_setRename(x, rev(letters[1:10]))
print(x)

################################################################################


# mutable atomic matrix ====
x <- mutable_atomic(
  1:20, dim = c(5, 4), dimnames = n(letters[1:5], letters[1:4])
)
print(x)
sb_setRename(
  x,
  newdimnames = lapply(dimnames(x), rev)
)
print(x)


x <- mutable_atomic(
   1:20, letters[1:20], dim = c(5, 4), dimnames = n(letters[1:5], letters[1:4])
)
print(x)
sb_setRename(
  x, newnames = rev(names(x)),
  newdimnames = lapply(dimnames(x), rev)
)
print(x)


################################################################################



# data.table ====

x <- data.table::data.table(
  a = 1:20,
  b = letters[1:20]
)
print(x)
sb_setRename(x, old = names(x), new = rev(names(x)))
print(x)

