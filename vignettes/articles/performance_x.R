# speed tests

library(squarebrackets)
library(ggplot2)
library(data.table)

# atomic ====
x <- 1:1e7
gc()
bm.ii_x.default <- bench::mark(
  ii_x(x, i = 1:1e6),
  x[1:1e6],
  min_iterations = 500
)
bm.ii_x.default$result <- NULL
summary(bm.ii_x.default)
ggplot2::autoplot(bm.ii_x.default)
save(bm.ii_x.default, file = "bm.ii_x.default.RData")


n <- 5e3
x.mat <- matrix(seq_len(n*n), ncol = n)
colnames(x.mat) <- sample(c(letters, NA), n, TRUE)
sel.rows <- 1:100
sel.cols <- rep(sample(letters[1:13]), 10)
foo <- cbind(
  match_all = colnames(x.mat)[match_all(sel.cols, colnames(x.mat))], 
  lapply = colnames(x.mat)[lapply(sel.cols, \(i) which(colnames(x.mat) == i)) |> unlist()]
)
all(apply(foo, 1, \(x)x[1] == x[2]))
gc()
bm.sb_x.matrix <- bench::mark(
  "squarebrackets" = ss_x(x.mat, n(sel.rows, sel.cols)),
  "base R" = x.mat[sel.rows, lapply(sel.cols, \(i) which(colnames(x.mat) == i)) |> unlist(), drop = FALSE],
  min_iterations = 500
)
bm.sb_x.matrix$result <- NULL
summary(bm.sb_x.matrix)
autoplot(bm.sb_x.matrix) + ggtitle("matrix")
save(bm.sb_x.matrix, file = "bm.sb_x.matrix.RData")


x.dims <- c(5000, 2000, 4)
x.3d <- array(1:prod(x.dims), x.dims)
sel.rows <- 1:900
sel.lyrs <- c(TRUE, FALSE, TRUE, FALSE)
gc()
bm.sb_x.3d <- bench::mark(
  "squarebrackets" =  ss_x(x.3d, n(sel.rows, sel.lyrs), c(1,3)),
  "base R + abind" = abind::asub(x.3d, idx = list(sel.rows, sel.lyrs), d = c(1,3)),
  min_iterations = 500
)
bm.sb_x.3d$result <- NULL
summary(bm.sb_x.3d)
autoplot(bm.sb_x.3d) + ggtitle("3d")
save(bm.sb_x.3d, file = "bm.sb_x.3d.RData")


# data.frame-like ====

n <- 1e5
ncol <- 200
chrmat <- matrix(
  sample(letters, n*ncol, replace = TRUE), ncol = ncol
)
intmat <- matrix(
  seq.int(n*ncol), ncol = ncol
)
x <- cbind(chrmat, intmat) |> as.data.frame()
rm(list = c("chrmat", "intmat"))
colnames(x) <- make.names(colnames(x), unique = TRUE)
sel.cols <- rep(sample(names(x), 10), 4)
sel.rows <- 1:1000
gc()
bm.sb_x.df <- bench::mark(
  "squarebrackets" = tt_x.data.frame(x, sel.rows, sel.cols),
  "base R" = x[sel.rows, sel.cols, drop = FALSE],
  min_iterations = 500
)
summary(bm.sb_x.df)
autoplot(bm.sb_x.df) + ggtitle("data.frame")
save(bm.sb_x.df, file = "bm.sb_x.df.RData")

x <- as.data.table(x)
tempfun <- function(x, i, j) {
  x <- collapse::ss(x, i, j, check = TRUE)
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}
gc()
bm.sb_x.dt <- bench::mark(
  "squarebrackets" = tt_x(x, sel.rows, sel.cols),
  "data.table + collapse" = tempfun(x, sel.rows, sel.cols),
  min_iterations = 500
)
summary(bm.sb_x.dt)
autoplot(bm.sb_x.dt) + ggtitle("data.table")
save(bm.sb_x.dt, file = "bm.sb_x.dt.RData")


# long ====
x <- sample(1:10, 2e6, TRUE)
ptrn <- c(TRUE, FALSE, FALSE, TRUE)

bm.long_x <- bench::mark(
  "pv in squarebrackets" = long_x(x, stride_pv(x, 5)),
  "pv in base R" = x[x == 5],
  "seq in squarebrackets" = long_x(x, ~ 1:(.N - 10):2),
  "seq in base R" = x[seq(1, length(x) - 10, 2)],
  "ptrn in squarebrackets" = long_x(x, ~ 1:(.N - 10):ptrn),
  "ptrn in base R" = x[ (1:(length(x) - 10))[ptrn] ],
  check = FALSE,
  min_iterations = 100
)
summary(bm.long_x)
autoplot(bm.long_x)
save(bm.long_x, file = "bm.long_x.RData")
