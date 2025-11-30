

x <- mutatomic(1:1e7)

# extract:
slice_x(x, 1, 10)

# reverse:
slice_x(x, length(x), 1) |> head()

# remove:
slice_wo(x, 1, length(x) - 10) # all elements except the last 10

# replace every other element:
x <- mutatomic(1:1e7)
slice_set(x, 2, length(x), 2, rp = -1)
head(x)

# replace all elements except the first element:
x <- mutatomic(1:1e7)
slice_set(x, 1, 1, inv = TRUE, rp = -1)
head(x)

