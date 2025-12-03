
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# vector ====
x <- as.mutatomic(1:100)
names(x) <- sample(c(month.name, month.abb), 100, TRUE)

funlist <- list(
  \(x, ii = NULL, use = 1) ii_x(x, ii, use),
  \(x, ii = NULL, use = 1) ii_mod(x, ii, use, chkdup = FALSE, rp = -1),
  \(x, ii = NULL, use = 1) ii_mod(x, ii, use, chkdup = FALSE, rp = -1:-100),
  \(x, ii = NULL, use = 1) ii_mod(x, ii, use, chkdup = FALSE, tf = mean),
  \(x, ii = NULL, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, rp = -1)
    return(x)
  },
  \(x, ii = NULL, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, rp = -1:-100)
    return(x)
  },
  \(x, ii = NULL, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, tf = mean)
    return(x)
  }
)

for(f in funlist) {
  # main:
  expect_equal(
    f(x),
    f(x, 1:100)
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
}
