
x <- data.frame(
  a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = -1:-10
)
print(x)
ind1 <- idx_r(x, 1, 2, 2* -1i) # rows 2:(nrow(x)-1)
sbt_x(x, ind1) # extract the row range

x <- array(1:125, c(5,5,5))
d <- 1:3
s <- idx_r(x, d, 2, 2* -1i) # 2:(n-1) for every dimension
ss_x(x, s = s, d = d) # same as x[ 2:4, 2:4, 2:4, drop = FALSE]

x <- letters
x[idx_r(x, 0, 2, 2* -1i)]

