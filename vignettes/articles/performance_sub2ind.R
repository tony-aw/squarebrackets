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
ind <- ss2ii(sub, x.dim, checks = FALSE)
bm.ss2ii <- bench::mark(
  base = x[1:450, 1:450, 1:90] |> as.vector(),
  ss2ii = ss2ii(sub, dim(x), checks = FALSE),
  min_iterations = 500
)
bm.ss2ii$result <- NULL
summary(bm.ss2ii)
ggplot2::autoplot(bm.ss2ii)
save(bm.ss2ii, file = "bm.ss2ii.RData")

