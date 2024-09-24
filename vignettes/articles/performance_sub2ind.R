# speed tests

library(squarebrackets)
library(ggplot2)
library(data.table)
loadNamespace("bench")
library(future.apply)


x.dim <- c(500, 500, 100)
x.len <- prod(x.dim)
x <- array(1:x.len, dim = x.dim)
sub <- n(1:450, 1:450, 1:90)
ind <- sub2ind(sub, x.dim, checks = FALSE)
bm.sub2ind <- bench::mark(
  base = x[1:450, 1:450, 1:90] |> as.vector(),
  sub2ind = sub2ind(sub, dim(x), checks = FALSE),
  min_iterations = 500
)
bm.sub2ind$result <- NULL
summary(bm.sub2ind)
ggplot2::autoplot(bm.sub2ind)
save(bm.sub2ind, file = "bm.sub2ind.RData")

