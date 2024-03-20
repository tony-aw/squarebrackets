# speed tests

library(subsets)
library(ggplot2)
library(data.table)
loadNamespace("bench")
library(future.apply)


x <- mutable_atomic(1:1e6)
bm.setv <- bench::mark(
  "base `x[which(x==v)] <- rp`" = x[which(x==10)] <- -10,
  "squarebrackets::ma_setv" = ma_setv(x, 10, -10, NA.safety = FALSE),
  "collapse::setv" = collapse::setv(x, 10, -10),
  min_iterations = 500,
  check = FALSE
)
bm.setv
ggplot2::autoplot(bm.setv)
save(bm.setv, file = "bm.setv.RData")


n <- 2000
x <- mutable_atomic(seq_len(n^2), dim = c(n,n))
bm.setapply <- bench::mark(
  sb = setapply(x, 1, rev),
  base = apply(x, 1, rev),
  min_iterations = 500,
  check = FALSE
)
bm.setapply
ggplot2::autoplot(bm.setapply)
save(bm.setv, file = "bm.setapply.RData")
