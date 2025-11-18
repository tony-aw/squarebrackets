
source(file.path(getwd(), "source", "functions4testing.R"))

enumerate <- 0

# 4D array ==== 
d <- c(10, 10, 10, 10)
len <- prod(d)
temp.fun <- function(...) {
  return(x[...])
}

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), d)
  ind1 <- sample(1:10, 3, FALSE)
  ind2 <- sample(1:10, 3, FALSE)
  ind3 <- sample(1:10, 3, FALSE)
  ind4 <- sample(1:10, 3, FALSE)
  subs <- list(ind1, ind2, ind3, ind4)
  coords <- ss2coord(subs, d)
  ind <- coord2ii(coords, d)
  x.coord <- numeric(length(ind))
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3], coords[i,4]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  coords <- ii2coord(ind, d)
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3], coords[i,4]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  expect_equal(
    coord2ss(coords), subs
  ) |> errorfun()
  enumerate <- enumerate + 3
}


# 3D array ==== 
d <- c(10, 10, 10)
len <- prod(d)
temp.fun <- function(...) {
  return(x[...])
}

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), d)
  ind1 <- sample(1:10, 3, FALSE)
  ind2 <- sample(1:10, 3, FALSE)
  ind3 <- sample(1:10, 3, FALSE)
  subs <- list(ind1, ind2, ind3)
  expect_equal(
    coord2ii(ss2coord(subs, d), d),
    ss2ii(subs, d)
  ) |> errorfun()
  coords <- coords <- ss2coord(subs, d)
  ind <- coord2ii(coords, d)
  x.coord <- numeric(length(ind))
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  coords <- ii2coord(ind, d)
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  expect_equal(
    coord2ss(coords), subs
  ) |> errorfun()
  enumerate <- enumerate + 4
}



# matrix ==== 
d <- c(10, 10)
len <- prod(d)
temp.fun <- function(...) {
  return(x[...])
}

for(i in 1:10) {
  x <- matrix(sample(seq_len(len*10), len, FALSE), ncol = 10)
  ind1 <- sample(1:10, 3, FALSE)
  ind2 <- sample(1:10, 3, FALSE)
  subs <- list(ind1, ind2)
  expect_equal(
    coord2ii(ss2coord(subs, d), d),
    ss2ii(subs, d)
  ) |> errorfun()
  coords <- coords <- ss2coord(subs, d)
  ind <- coord2ii(coords, d)
  x.coord <- numeric(length(ind))
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  coords <- ii2coord(ind, d)
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  expect_equal(
    coord2ss(coords), subs
  ) |> errorfun()
  enumerate <- enumerate + 4
}



# error checks ====
d <- c(1000, 1000, 4, 4)
len <- prod(d)
x <- array(1:len, d)
coords <- rbind(c(4:1), 1:4)
subs <- list(4:1, 1:4)
expect_error(
  coord2ii(coords, numeric(0)),
  pattern = "`length(x.dim) == 0`",
  fixed = TRUE
)
expect_error(
  coord2ii(matrix("1", ncol=4), character(4)),
  pattern = "`x.dim` and `coord` must both be numeric",
  fixed = TRUE
)
expect_error(
  coord2ii(coords, c(1, d)),
  pattern = "`ncol(coord) != length(x.dim)`",
  fixed = TRUE
)
expect_error(
  ss2coord(subs, rep(5, 5)),
  pattern = "`length(sub) != length(x.dim)`",
  fixed = TRUE
)
enumerate <- enumerate + 5


