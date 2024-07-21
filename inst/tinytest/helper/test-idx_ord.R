
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0

samples <- list(
  rep(c(TRUE, FALSE), 25),
  as.integer(1:25),
  as.double(1:25),
  letters,
  as.factor(letters)
)

for(i_na.last in c(TRUE, FALSE)) {
  for(j_decr in c(TRUE, FALSE)) {
    for(k_method in c("auto", "shell", "radix")) {
      for(l_sample in 1:length(samples)) {
        for(m_rep in 1:10) {
          x <- sample(samples[[l_sample]], size = 25)
          expect_equal(
            idx_ord_v(x, i_na.last, j_decr, k_method),
            order(x, na.last = i_na.last, decreasing = j_decr, method = k_method)
          ) |> errorfun()
          
          x <- matrix(x, ncol = 5)
          expect_equal(
            idx_ord_m(x, 1, i_na.last, j_decr, k_method),
            order(x[1,], x[2,], x[3,], x[4,], x[5,], na.last = i_na.last, decreasing = j_decr, method = k_method)
          ) |> errorfun()
          expect_equal(
            idx_ord_m(x, 2, i_na.last, j_decr, k_method),
            order(x[, 1], x[, 2], x[, 3], x[, 4], x[, 5], na.last = i_na.last, decreasing = j_decr, method = k_method)
          ) |> errorfun()
          
          x <- as.data.frame(x)
          expect_equal(
            idx_ord_df(x, i_na.last, j_decr, k_method),
            order(x[, 1], x[, 2], x[, 3], x[, 4], x[, 5], na.last = i_na.last, decreasing = j_decr, method = k_method)
          ) |> errorfun()
          
          enumerate <- enumerate + 3
          
        }
      }
    }
  }
}