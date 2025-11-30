
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# 1d array ====
x <- as.mutatomic(array(integer(0L)))
expect_equal(
  ndim(x), 1L
)

funlist <- list(
  \(x, ss, d = 1:ndim(x)) ss_x(x, ss, d),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, rp = -1),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, rp = integer(0L)),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, tf = \(x)x[1]),
  \(x, ss, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, d, rp = -1)
    return(x)
  },
  \(x, ss, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, d, rp = integer(0L))
    return(x)
  },
  \(x, ss, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, d, tf = \(x) x[1])
    return(x)
  }
)

indices <- n(logical(0L), integer(0L), character(0L), 0L, NULL)

for(f in funlist) {
  for(iUse in c(1, -1, Inf, -Inf)) {
    for(iIndx in seq_along(indices)) {
      # main:
      expect_equal(
        f(x, n(indices[[iIndx]])),
        x
      ) |> errorfun()
      expect_equal(
        f(x, n(indices[[iIndx]]), iUse),
        x
      ) |> errorfun()
      expect_equal(
        f(x, indices[[iIndx]]),
        x
      ) |> errorfun()
      expect_equal(
        f(x, indices[[iIndx]], iUse),
        x
      ) |> errorfun()
      enumerate <- enumerate + 4L
      
      # general error tests ====
      expect_error(
        f(x, 1:3, "a")
      ) |> errorfun()
      expect_error(
        f(x, 1:3, 1:10),
        pattern = "`use` out of range"
      ) |> errorfun()
      expect_error(
        f(x, 1:3, -1:-10),
        pattern = "`use` out of range"
      ) |> errorfun()
      expect_error(
        f(x, 1:3, -1:1),
        pattern = "`use` out of range"
      ) |> errorfun()
      enumerate <- enumerate + 5L
    }
  }
}


# 3d array tests ====
