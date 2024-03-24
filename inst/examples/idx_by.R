

# vectors ====
(a <- 1:20)
(grp <- factor(rep(letters[1:5], each = 4)))

# get the last element of `a` for each group in `grp`:
i <- idx_by(last, seq_along(a), grp)
sb_x(cbind(a, grp), row = i)


# data.frame ====
x <- data.frame(
  a = sample(1:20),
  b = letters[1:20],
  group = factor(rep(letters[1:5], each = 4))
)
print(x)
# get the first row for each group in data.frame `x`:
row <- idx_by(first, 1:nrow(x), x$group)
sb2_x(x, row)
# get the first row for each group for which a > 10:
x2 <- sb2_x(x, filter = ~ a > 10)
row <- na.omit(idx_by(first, 1:nrow(x2), x2$group))
sb2_x(x2, row)
 

