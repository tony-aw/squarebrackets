
# basic idea ====
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic::mutatomic(1:1e6, names = nms)
head(x)

# memory efficient form of sum(x <= 10):
countv(x, v = c(-Inf, 10))

# extract all elements of x with the name "a":
slicev_x(x, y = names(x), v = "a") |> head()

# find all x smaller than or equal to 5, and replace with `-1000`:
slicev_set(x, y = x, v = c(-Inf, 5), rp = -1000L)
head(x, n = 10)


################################################################################
# Numeric range ====
#
x <- mutatomic::mutatomic(1:1e6)
head(x)
slicev_x(x, v= c(-Inf, 5)) # x[x <= 5]


################################################################################
# Character ====
#
x <- stringi::stri_rand_shuffle(rep("hello", 1e5))
head(x)
slicev_x(x, v = "hello") |> head() # find "hello"

# find 2 possible misspellings of "hello":
slicev_x(x, v = c("holle", "helol")) |> head()




