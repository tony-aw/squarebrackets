# speed tests

library(squarebrackets)
library(ggplot2)
library(data.table)
loadNamespace("bench")
library(future.apply)


# atomic ====

n <- 3162 # approx sqrt(1e7)
x.mat <- matrix(seq_len(n*n), ncol = n)
x.mat2 <- as.mutable_atomic(x.mat)
colnames(x.mat) <- sample(c(letters, LETTERS, NA), n, TRUE)
sel.rows <- 1:1000
sel.cols <- 1:1000
basefun <- function(x, rows, cols, tf) {
  x[rows, cols] <- tf(x[rows, cols])
  return(x)
}
base_plus_idx <- function(x, rows, cols, tf) {
  x[idx.array(x, n(rows, cols), 1:2)] <- tf(x[idx.array(x, n(rows, cols), 1:2)])
  return(x)
}
tf <- function(x) { return(-1 * x) }
bm.sb_tf.matrix <- bench::mark(
  "base [<-" =  basefun(x.mat, sel.rows, sel.cols, tf = tf),
  "idx + [<-" = base_plus_idx(x.mat, sel.rows, sel.cols, tf = tf),
  "sb_set" = sb_set.matrix(x.mat2, sel.rows, sel.cols, tf = tf),
  "sb_mod" = sb_mod.matrix(x.mat, sel.rows, sel.cols, tf = tf),
  check = FALSE,
  min_iterations = 500
)
bm.sb_tf.matrix
summary(bm.sb_tf.matrix)
autoplot(bm.sb_tf.matrix) + ggtitle("matrix")
save(bm.sb_tf.matrix, file = "bm.sb_tf.matrix.RData")


x.dims <- c(1900, 1900, 3) # leads to approx 1e7 elements
x.3d <- array(1:prod(x.dims), x.dims)
x.3d2 <- as.mutable_atomic(x.3d)
sel.rows <- 1:900
sel.lyrs <- c(TRUE, FALSE, TRUE)
basefun <- function(x, rows, lyrs, tf) {
  x[rows, , lyrs] <- tf(x[rows, , lyrs])
  return(x)
}
tf <- function(x) { return(-1 * x) }
bm.sb_tf.3d <- bench::mark(
  "base [<-" = basefun(x.3d, sel.rows, sel.lyrs, tf = tf ),
  "sb_set" =  sb_set.array(x.3d2, n(sel.rows, sel.lyrs), c(1,3), tf = tf),
  "sb_mod" = sb_mod.array(x.3d, rcl = n(sel.rows, NULL, sel.lyrs), tf = tf),
  check = FALSE,
  min_iterations = 500
)
summary(bm.sb_tf.3d)
autoplot(bm.sb_tf.3d) + ggtitle("3d")
save(bm.sb_tf.3d, file = "bm.sb_tf.3d.RData")


# data.frame-like ====

n <- 1e5
ncol <- 200 # times 2
chrmat <- matrix(
  sample(letters, n*ncol, replace = TRUE), ncol = ncol
)
intmat <- matrix(
  seq.int(n*ncol), ncol = ncol
)
x <- cbind(chrmat, intmat) |> as.data.frame()
colnames(x) <- make.names(colnames(x), unique = TRUE)
x2 <- data.table::as.data.table(x)
rm(list = c("chrmat", "intmat"))

sel.rows <- 1:1000
basefun <- function(x, rows, tf) {
  x[rows, sapply(x, is.numeric)] <- lapply(x[rows, sapply(x, is.numeric)], tf)
  return(x)
}
bm.sb_tf.df <- bench::mark(
  "base [<-" = basefun(x, sel.rows, tf = \(x) -1 * x),
  "sb_set" = sb2_set.data.table(
    x2, rows = sel.rows, vars = is.numeric, tf = \(x) -1 * x
  ),
  "sb_mod" = sb2_mod.data.frame(
    x, rows = sel.rows, vars = is.numeric, tf = \(x) -1 * x, coe = TRUE
  ),
  check = FALSE,
  min_iterations = 500
)
summary(bm.sb_tf.df)
autoplot(bm.sb_tf.df) + ggtitle("data.frame-like")
save(bm.sb_tf.df, file = "bm.sb_tf.df.RData")
