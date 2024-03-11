
x <- data.frame(
  a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = -1:-10
)
ind <- seq_names(colnames(x), "b", "d")
sb_x(x, col = ind)
