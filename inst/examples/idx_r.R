
x <- data.frame(
  a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = -1:-10
)
print(x)
ind1 <- idx_r(x, 1, 2, 2-1i) # rows 2:(n-1)
ind2 <- idx_r(x, 2, "d", 2) # columns d:2
sb2_x(x, ind1, ind2)
