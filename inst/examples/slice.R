

x <- mutable_atomic(1:1e7)

# extract:
slice_x(x, 1, 10)

# reverse:
slice_x(x, -1i, 1) |> head()

# remove:
slice_rm(x, 1, -11i) # all elements except the last 10

# replace every other element:
x <- mutable_atomic(1:1e7)
slice_set(x, 2, -1i, 2, rp = -1)
head(x)

# replace all elements except the first element:
x <- mutable_atomic(1:1e7)
slice_set(x, 1, 1, inv = TRUE, rp = -1)
head(x)

