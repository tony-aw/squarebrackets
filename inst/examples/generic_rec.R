

lst <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    A = list(A  = "AA2A", B = "AA2B"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  )
)

#############################################################################

# access recursive subsets ====

sb_rec(lst, c(1,2,2)) # this gives "AA2B"
sb_rec(lst, c("A", "B", "B")) # this gives "ABB"
sb_rec(lst, c(2,2,1)) # this gives "BBA"
sb_rec(lst, c("B", "B", "A")) # this gives "BBA"


#############################################################################

# replace with R's default in-place semantics ====

# replace "AAB" using R's default in-place semantics:
sb_rec(
  lst, c("A", "A", "B"),
  rp = "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
)
print(lst)


#############################################################################

# Modify View of List By Reference ====

x <- list(
 a = data.table::data.table(cola = 1:10, colb = letters[1:10]),
 b = data.table::data.table(cola = 11:20, colb = letters[11:20])
)
print(x)
mypointer <- sb_rec(x, "a")
address(mypointer) == address(x$a) # they are the same
sb_set(mypointer, col = "cola", tf = \(x)x^2)
print(x) # notice x has been changed

