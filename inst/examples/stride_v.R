
# basic idea ====
nms <- c(letters, LETTERS, month.abb, month.name) |> rep_len(1e6)
x <- mutatomic(1:1e6, names = nms)
head(x)

# extract all elements of x with the name "a":
stride <-  stride_v(names(x), v = "a")
long_x(x, stride) |> head()

# find all x smaller than or equal to 5, and replace with `-1000`:
stride <- stride_v(x, v = c(-Inf, 5))
long_set(x, stride, rp = -1000L)
head(x, n = 10)


################################################################################
# Numeric range ====
#
x <- mutatomic(1:1e6)
head(x)
stride <- stride_v(x, v = c(-Inf, 5))
long_x(x, stride) # x[x <= 5]


################################################################################
# Character ====
#
if(require(stringi)) {
  x <- stringi::stri_rand_shuffle(rep("hello", 1e5))
  head(x)
  stride <- stride_v(x, v = "hello")
  long_x(x, stride) |> head() # find "hello"
  
  # find 2 possible misspellings of "hello":
  stride <- stride_v(x, v = c("holle", "helol"))
  long_x(x, stride) |> head()
  
}




