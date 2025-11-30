# speed tests

library(squarebrackets)
library(ggplot2)
library(data.table)
loadNamespace("bench")
library(future.apply)


# atomic ====

n <- 3162 # approx sqrt(1e7)
x.mat <- matrix(seq_len(n*n), ncol = n)
x.mat2 <- as.mutatomic(x.mat)
colnames(x.mat) <- sample(c(letters, LETTERS, NA), n, TRUE)
sel.rows <- 1:1000
sel.cols <- 1:1000
basefun <- function(x, rows, cols, tf) {
  x[rows, cols] <- tf(x[rows, cols])
  return(x)
}
base_plus_idx <- function(x, rows, cols, tf) {
  x[ss_icom(x, n(rows, cols), 1:2)] <- tf(x[ss_icom(x, n(rows, cols), 1:2)])
  return(x)
}
tf <- function(x) { return(-1 * x) }
gc()
bm.sb_tf.matrix <- bench::mark(
  "base [<-" =  basefun(x.mat, sel.rows, sel.cols, tf = tf),
  "idx + [<-" = base_plus_idx(x.mat, sel.rows, sel.cols, tf = tf),
  "ss_set" = ss_set(x.mat2, n(sel.rows, sel.cols), tf = tf),
  "ss_mod" = ss_mod(x.mat, n(sel.rows, sel.cols), tf = tf),
  check = FALSE,
  min_iterations = 500
)
summary(bm.sb_tf.matrix)
autoplot(bm.sb_tf.matrix) + ggtitle("matrix")
save(bm.sb_tf.matrix, file = "bm.sb_tf.matrix.RData")


x.dims <- c(1900, 1900, 3) # leads to approx 1e7 elements
x.3d <- array(1:prod(x.dims), x.dims)
x.3d2 <- as.mutatomic(x.3d)
sel.rows <- 1:900
sel.lyrs <- c(TRUE, FALSE, TRUE)
basefun <- function(x, rows, lyrs, tf) {
  x[rows, , lyrs] <- tf(x[rows, , lyrs])
  return(x)
}
base_plus_idx <- function(x, rows, lyrs, tf) {
  x[ss_icom(x, n(rows, lyrs), c(1, 3))] <- tf(x[ss_icom(x, n(rows, lyrs), c(1, 3))])
  return(x)
}
tf <- function(x) { return(-1L * x) }
gc()
bm.sb_tf.3d <- bench::mark(
  "base [<-" = basefun(x.3d, sel.rows, sel.lyrs, tf = tf ),
  "idx + [<-" = base_plus_idx(x.3d, sel.rows, sel.lyrs, tf = tf),
  "ss_set" =  ss_set(x.3d2, n(sel.rows, sel.lyrs), c(1,3), tf = tf),
  "ss_mod" = ss_mod(x.3d, n(sel.rows, sel.lyrs), c(1, 3), tf = tf),
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
df <- cbind(chrmat, intmat) |> as.data.frame()
colnames(df) <- make.names(colnames(df), unique = TRUE)
dt <- data.table::as.data.table(df)
rm(list = c("chrmat", "intmat"))

sel.rows <- 1:1000
basefun <- function(x, rows, tf) {
  x[rows, sapply(x, is.numeric)] <- lapply(x[rows, sapply(x, is.numeric)], tf)
  return(x)
}
gc()
bm.sb_tf.df <- bench::mark(
  "base [<-" = basefun(df, sel.rows, tf = \(x) -1 * x),
  "sbt_set" = sbt_set(
    dt, obs = sel.rows, col = is.numeric, tf = \(x) -1 * x
  ),
  "sbt_mod" = sbt_mod(
    df, obs = sel.rows, col = is.numeric, tf = \(x) -1 * x
  ),
  check = FALSE,
  min_iterations = 500
)
summary(bm.sb_tf.df)
autoplot(bm.sb_tf.df) + ggtitle("data.frame-like")
save(bm.sb_tf.df, file = "bm.sb_tf.df.RData")
