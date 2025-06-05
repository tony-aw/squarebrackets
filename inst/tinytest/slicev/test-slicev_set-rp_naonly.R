
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# regular ====
gen <- function() c(rnorm(100 - 4), NA, NaN, Inf -Inf, 1)

x.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  gen() + gen() * -1i
)
y.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  gen() + gen() * -1i
)
rp.list <- list(
  TRUE,
  -1L,
  -1.5,
  "a",
  1-1i
)

for(i in seq_along(x.list)) {
  out <- x.list[[i]] |> as.mutatomic()
  y <- y.list[[i]]
  rp <- rp.list[[i]]
  
  ind <- which(is.na(y))
  ind <- ind[ind >= 20 & ind <= 90]
  
  expected <- data.table::copy(out)
  expected[ind] <- rp
  slicev_set(
    out, y = y, na = NA, r = TRUE, from = 11 * -1i, to = 20,
    rp = rp
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  
  out <- x.list[[i]] |> as.mutatomic()
  y <- y.list[[i]]
  rp <- rp.list[[i]]
  
  ind <- which(!is.na(y))
  ind <- ind[ind >= 20 & ind <= 90]
  
  expected <- data.table::copy(out)
  expected[ind] <- rp
  slicev_set(
    out, y = y, na = NA, r = FALSE, from = 11 * -1i, to = 20,
    rp = rp
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  
  enumerate <- enumerate + 2L
}



