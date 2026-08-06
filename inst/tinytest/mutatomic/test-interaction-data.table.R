
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

x <- mutatomic(1:10)
d <- data.table::data.table(x)
expect_true(is.mutatomic(d$x))

expect_silent(d[1:5])
d <- d[1:5]
expect_true(is.mutatomic(d$x))

enumerate <- enumerate + 3L