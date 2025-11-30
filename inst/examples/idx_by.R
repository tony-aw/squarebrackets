

# vectors ====
(a <- 1:20)
(grp <- factor(rep(letters[1:5], each = 4)))

# get the last element of `a` for each group in `grp`:
s <- list(idx_by(a, 0L, last, grp))
ss_x(cbind(a, grp), s, 1L)


# data.frame ====
x <- data.frame(
  a = sample(1:20),
  b = letters[1:20],
  group = factor(rep(letters[1:5], each = 4))
)
print(x)
# get the first row for each group in data.frame `x`:
row <- idx_by(x, 1, first, x$group)
sbt_x(x, row)
# get the first row for each group for which a > 10:
x2 <- sbt_x(x, idx_obs(x, ~ a > 10))
row <- na.omit(idx_by(x2, 1, first, x2$group))
sbt_x(x2, row)
 

