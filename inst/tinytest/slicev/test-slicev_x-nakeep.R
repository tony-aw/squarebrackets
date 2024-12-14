
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# single ====

gen <- function() c(rnorm(100 - 4), NA, NaN, Inf -Inf, 1)

x.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  gen() + gen() * -1i,
  as.raw(sample(1:100))
  
  
)
y.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  gen() + gen() * -1i,
  as.raw(sample(1:100))
)
v.list <- list(
  FALSE,
  2L,
  2.5,
  "a",
  1 - 1i,
  as.raw(1L)
)

for(i in seq_along(x.list)) {
  x <- x.list[[i]]
  y <- y.list[[i]]
  v <- v.list[[i]]
  ind <- which(ifelse(is.na(y), TRUE, y == v))
  ind <- ind[ind >= 20 & ind <= 90]
  expected <- rev(x[ind])
  out <- slicev_x(
    x, y = y, v = v, na = TRUE, r = TRUE, from = 11 * -1i, to = 20
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  x <- x.list[[i]]
  y <- y.list[[i]]
  v <- v.list[[i]]
  ind <- which(ifelse(is.na(y), TRUE, y != v))
  ind <- ind[ind >= 20 & ind <= 90]
  expected <- rev(x[ind])
  out <- slicev_x(
    x, y = y, v = v, na = TRUE, r = FALSE, from = 11 * -1i, to = 20
  )
  expect_equal(
    expected, out
  ) |> errorfun()
  
  
  enumerate <- enumerate + 2L
}


# numeric range ===
x.list <- list(
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE)
)
y.list <- list(
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE)
)
min.list <- list(
  -Inf,
  2.1,
  -Inf,
  2.1
)
max.list <- list(
  Inf,
  10,
  10,
  Inf
)
for(i in seq_along(x.list)) {
  for(j in seq_along(min.list)) {
    x <- x.list[[i]]
    y <- y.list[[i]]
    v <- c(min.list[[j]], max.list[[j]])
    ind <- which(ifelse(is.na(y), TRUE, y >= v[1] & y <= v[2]))
    ind <- ind[ind >= 20 & ind <= 90]
    expected <- rev(x[ind])
    out <- slicev_x(
      x, y = y, v = v, na = TRUE, r = TRUE, from = 11 * -1i, to = 20
    )
    expect_equal(
      expected, out
    ) |> errorfun()
    
    x <- x.list[[i]]
    y <- y.list[[i]]
    v <- c(min.list[[j]], max.list[[j]])
    ind <- which(ifelse(is.na(y), TRUE, y < v[1] | y > v[2]))
    ind <- ind[ind >= 20 & ind <= 90]
    expected <- rev(x[ind])
    out <- slicev_x(
      x, y = y, v = v, na = TRUE, r = FALSE, from = 11 * -1i, to = 20
    )
    expect_equal(
      expected, out
    ) |> errorfun()
    
    
    enumerate <- enumerate + 2L
  }
  
}


# string, multiple ====
x <- sample(c(letters, NA), 100, TRUE)
y <- sample(c(letters, NA), 100, TRUE)
x2 <- x[90:20]
y2 <- y[90:20]
expected <- x2[ifelse(is.na(y2), TRUE, y2 %in% letters[2:25])]
out <- slicev_x(x, y = y, v = letters[2:25], na = TRUE, r = TRUE, from = 11 * -1i, to = 20)
expect_equal(
  expected, out
)
expected <- x2[ifelse(is.na(y2), TRUE, !y2 %in% letters[2:25])]
out <- slicev_x(x, y = y, v = letters[2:25], na = TRUE, r = FALSE, from = 11 * -1i, to = 20)
expect_equal(
  expected, out
)
enumerate <- enumerate + 2L

