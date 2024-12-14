
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

x <- sample(1:10)
expect_message(
  countv(x, v = 1L, na = NA),
  pattern = "`na = NA`, so argument `v` will be ignored"
)
