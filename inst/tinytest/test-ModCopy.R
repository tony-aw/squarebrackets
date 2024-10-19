
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())



# data.frame-like - columns ====
cols <- c(1:2, 11:12)
convertfuns <- list(
  as.data.frame,
  data.table::as.data.table
)
for(i in seq_along(convertfuns)) {
  numX <- rnorm(100*10) |> matrix(ncol = 10)
  colnames(numX) <- letters[1:10]
  charX <- stringi::stri_rand_strings(100*10, 10) |> matrix(ncol = 10)
  colnames(charX) <- letters[11:20]
  x <- y <- convertfuns[[i]] (data.frame(numX, charX))
  
  z <- sb2_mod(x, col = cols, tf = \(x)x[1])
  
  az <- sapply(sb2_x(z, col = cols), data.table::address)
  ax <- sapply(sb2_x(x, col = cols), data.table::address)
  ay <- sapply(sb2_x(y, col = cols), data.table::address)
  expect_false(any(ax == az)) |> errorfun()
  expect_false(any(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- sb2_x(z, col = cols)
  ax <- sb2_x(x, col = cols)
  ay <- sb2_x(y, col = cols)
  expect_false(identical(ax, az)) |> errorfun()
  expect_false(identical(ay, az)) |> errorfun()
  expect_true(identical(ax, ay)) |> errorfun()
  
  az <- sapply(sb2_rm(z, col = cols), data.table::address)
  ax <- sapply(sb2_rm(x, col = cols), data.table::address)
  ay <- sapply(sb2_rm(y, col = cols), data.table::address)
  expect_true(all(ax == az)) |> errorfun()
  expect_true(all(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- sb2_rm(z, col = cols)
  ax <- sb2_rm(x, col = cols)
  ay <- sb2_rm(y, col = cols)
  expect_true(identical(ax, az)) |> errorfun()
  expect_true(identical(ay, az)) |> errorfun()
  expect_true(identical(ax, ay)) |> errorfun()
  
  enumerate <- enumerate + 12
}




# data.frame-like - rows & columns ====
convertfuns <- list(
  as.data.frame,
  data.table::as.data.table
)
rows <- c(1:10, 90:100)
cols <- c(1:2, 11:12)
for(i in seq_along(convertfuns)) {
  numX <- rnorm(100*10) |> matrix(ncol = 10)
  colnames(numX) <- letters[1:10]
  charX <- stringi::stri_rand_strings(100*10, 10) |> matrix(ncol = 10)
  colnames(charX) <- letters[11:20]
  x <- y <- convertfuns[[i]] (data.frame(numX, charX))

  z <- sb2_mod(x, row = rows, col = cols, tf = \(x)x[1])
  
  # note: selecting cols is like selecting list elements
  # (i.e. result is a list)
  # whereas selecting rows is like selecting recursive list elements
  # (i.e. result is the object pointed to)
  
  az <- sapply(sb2_x(z, col = cols), data.table::address)
  ax <- sapply(sb2_x(x, col = cols), data.table::address)
  ay <- sapply(sb2_x(y, col = cols), data.table::address)
  expect_false(any(ax == az)) |> errorfun()
  expect_false(any(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- sb2_x(z, col = cols)
  ax <- sb2_x(x, col = cols)
  ay <- sb2_x(y, col = cols)
  expect_false(identical(ax, az)) |> errorfun()
  expect_false(identical(ay, az)) |> errorfun()
  expect_true(identical(ax, ay)) |> errorfun()
  
  az <- sapply(sb2_rm(z, col = cols), data.table::address)
  ax <- sapply(sb2_rm(x, col = cols), data.table::address)
  ay <- sapply(sb2_rm(y, col = cols), data.table::address)
  expect_true(all(ax == az)) |> errorfun()
  expect_true(all(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- sb2_rm(z, col = cols)
  ax <- sb2_rm(x, col = cols)
  ay <- sb2_rm(y, col = cols)
  expect_true(identical(ax, az)) |> errorfun()
  expect_true(identical(ay, az)) |> errorfun()
  expect_true(identical(ax, ay)) |> errorfun()
  
  enumerate <- enumerate + 12
}


# list ====
x <- y <- list(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE),
  b = rpois(10, 5),
  c = rnorm(10),
  c = letters,
  d = factor(letters[1:20])
)

i <- c(1, 3)
z <- sb2_mod(x, i = i, tf = \(x)x[1])

az <- sapply(sb2_x(z, i = i), data.table::address)
ax <- sapply(sb2_x(x, i = i), data.table::address)
ay <- sapply(sb2_x(y, i = i), data.table::address)
expect_false(any(ax == az))
expect_false(any(ay == az))
expect_true(all(ax == ay))

az <- sb2_x(z, i = i)
ax <- sb2_x(x, i = i)
ay <- sb2_x(y, i = i)
expect_false(identical(ax, az))
expect_false(identical(ay, az))
expect_true(identical(ax, ay))

az <- sapply(sb2_rm(z, i = i), data.table::address)
ax <- sapply(sb2_rm(x, i = i), data.table::address)
ay <- sapply(sb2_rm(y, i = i), data.table::address)
expect_true(all(ax == az))
expect_true(all(ay == az))
expect_true(all(ax == ay))

az <- sb2_rm(z, i = i)
ax <- sb2_rm(x, i = i)
ay <- sb2_rm(y, i = i)
expect_true(identical(ax, az))
expect_true(identical(ay, az))
expect_true(identical(ax, ay))

enumerate <- enumerate + 12

