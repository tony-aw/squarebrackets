
# extract all elements of x with the name "a":
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic(1:1e6, names = nms)
head(x)
stride <-  stride_v(names(x), v = "a")
long_x(x, stride) |> head()


# find all x smaller than or equal to 5, and replace with `-1000`:
stride <- stride_v(x, v = c(-Inf, 5))
long_set(x, stride, rp = -1000L)
head(x, n = 10)




x <- mutatomic(1:1e7)

# extract elements 2 to 9
long_x(x, ~ 2:9:1:1)

# reverse:
long_x(x, ~ 9:2:1:1)

# remove ll elements except the last 10:
long_x(x, ~ 1:(.N - 10):1:-1)

# replace every other element:
x <- mutatomic(1:1e7)
long_set(x, ~ 2:.N:2:1, rp = -1)
head(x)

# replace all elements except the first element:
x <- mutatomic(1:1e7)
long_set(x, ~1:1:1:-1, rp = -1)
head(x)


# extract pattern c(TRUE, FALSE, FALSE, TRUE) from first 20 elements:
x <- mutatomic(1:1e7)
long_x(x, ~ 1:20:c(TRUE, FALSE, FALSE, TRUE):1)

