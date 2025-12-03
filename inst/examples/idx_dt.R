

obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
print(obj)
sbt_x(obj, 1:3, 1:3)
sbt_x(obj, with(obj,  (a > 5) & (c < 19)), idx_vars(obj, a ~ c))


