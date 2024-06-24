
# show-casing how the list-elements are arranged and named ====

x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    A = list(A  = "AA2A", B = "AA2B"),
    B = list(A = "ABA", B = "ABB"),
    C = letters
  ),
  Y = list(
    Z = list(Z = "YZZ", Y = "YZY"),
    Y = list(Z = "YYZ", Y = "YYY"),
    X = "YX"
  )
)


# un-tree column-wise:
sapply(x, lst_nlists) |> max() # number of rows `y` will have
y <- lst_untree(x, margin = 2, use.names = TRUE)
dim(y)
print(y)
y[["Y.Z.Y"]] # you can still use names for selecting/replacing
sb2_x(y, n(1:3, 1:2), 1:2) # vectorized selection of multiple recursive elements


# un-tree row-wise:
sapply(x, lst_nlists) |> max() # number of columns `y` will have
y <- lst_untree(x, margin = 1, use.names = TRUE)
dim(y)
print(y)
y[["Y.Z.Y"]] # you can still use names for selecting/replacing
sb2_x(y, n(1:2, 1:3), 1:2)  # vectorized selection of multiple recursive elements


# simple flattened list:
y <- lst_untree(x, margin = 0, use.names = TRUE)
print(y)
y[["Y.Z.Y"]]
x[[c("Y", "Z", "Y")]] # equivalent in the original list


################################################################################

# showcasing that only list-elements are recursively flattened ====
# i.e. atomic vectors in recursive subsets remain atomic

x <- lapply(1:10, \(x)list(sample(letters), sample(1:10)))

sapply(x, lst_nlists) |> max()
y <- lst_untree(x, margin = 1)
dim(y)
print(y)

lst_untree(x, margin = 1)


################################################################################

# showcasing vectorized sub-setting ====
x <- lapply(1:10, \(x) list(
  list(sample(letters[1:10]), sample(LETTERS[1:10])),
  list(sample(month.abb), sample(month.name)),
  list(sample(1:10), rnorm(10))
))
y <- lst_untree(x, 1)

# getting the first recursive elements in the second level/depth in base R:
for(i in seq_along(x)) {
  x[[c(i, c(1,1))]]
}

# the same, but vectorized using the untree'd list:
y[seq_len(nrow(y)), 1]

