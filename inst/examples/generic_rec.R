

lst <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    A = list(A  = "AA2A", B = "AA2B"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  ),
  C = list(
    A = 1:10,
    B = 11:20
  )
)

#############################################################################

# access recursive subsets ====

sb2_rec(lst, c(1,2,2)) # this gives "AA2B"
sb2_rec(lst, c("A", "B", "B")) # this gives "ABB"
sb2_rec(lst, c(2,2,1)) # this gives "BBA"
sb2_rec(lst, c("B", "B", "A")) # this gives "BBA"


#############################################################################

# replace recursive subset with R's default in-place semantics ====

# replace "AAB" using R's default in-place semantics:
sb2_recin(
  lst, c("A", "A", "B"),
  rp = "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
)
print(lst)



#############################################################################

# transform recursive subsets with R's default in-place semantics ====

sb2_recin(lst, c("C", "A"), tf = \(x)x^2) # transforms lst$C$A

print(lst)


#############################################################################

# add/remove new recursive subsets with R's default in-place semantics ====

sb2_recin(lst, c("C", "D"), rp = "NEW VALUE") # adds lst$C$D
print(lst)

sb2_recin(lst, c("C", "A"), rp = NULL) # removes lst$C$A
print(lst) # notice lst$C$A is GONE


#############################################################################

# Modify View of List By Reference ====

x <- list(
 a = data.table::data.table(cola = 1:10, colb = letters[1:10]),
 b = data.table::data.table(cola = 11:20, colb = letters[11:20])
)
print(x)
myref <- sb2_rec(x, "a")
address(myref) == address(x$a) # they are the same
sb2_set(myref, vars = "cola", tf = \(x)x^2)
print(x) # notice x has been changed


