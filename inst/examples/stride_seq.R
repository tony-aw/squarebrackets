

x <- mutatomic(1:1e7)

# extract elements 2 to 9
long_x(x, stride_seq(2, 9, 1))

# reverse:
long_x(x, stride_seq(9, 2, 1))

# remove elements 1 to length(x) - 10:
long_x(x, stride_seq(1, length(x) - 10, 1), -1)

# replace every other element:
x <- mutatomic(1:1e7)
long_set(x, stride_seq(2, length(x), 2), rp = -1)
head(x)

# replace all elements except the first element:
x <- mutatomic(1:1e7)
long_set(x, stride_seq(1, 1, 1), use = -1, rp = -1)
head(x)

