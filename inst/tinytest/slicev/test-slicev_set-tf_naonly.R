
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# regular ====
x.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(letters, NA), 100, TRUE)
)
y.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(letters, NA), 100, TRUE)
)
tf.list <- list(
  \(x) return(FALSE),
  \(x) return(-1000L),
  \(x) return(-10.5),
  \(x) return("XXX")
)

for(i in seq_along(x.list)) {
  out <- x.list[[i]] |> mutatomic::as.mutatomic()
  y <- y.list[[i]]
  tf <- tf.list[[i]]
  
  ind <- which(is.na(y))
  ind <- ind[ind >= 20 & ind <= 90]
  
  expected <- data.table::copy(out)
  expected[ind] <- tf(expected[ind])
  slicev_set(
    out, y = y, na = NA, r = TRUE, from = 11 * -1i, to = 20,
    tf = tf
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  
  out <- x.list[[i]] |> mutatomic::as.mutatomic()
  y <- y.list[[i]]
  
  ind <- which(!is.na(y))
  ind <- ind[ind >= 20 & ind <= 90]
  
  expected <- data.table::copy(out)
  expected[ind] <- tf(expected[ind])
  slicev_set(
    out, y = y, na = NA, r = FALSE, from = 11 * -1i, to = 20,
    tf = tf
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  
  enumerate <- enumerate + 2L
}


