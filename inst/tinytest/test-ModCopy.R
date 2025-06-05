
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
  
  z <- ss2_mod(x, vars = cols, tf = \(x)x[1])
  
  az <- sapply(ss2_x(z, vars = cols), data.table::address)
  ax <- sapply(ss2_x(x, vars = cols), data.table::address)
  ay <- sapply(ss2_x(y, vars = cols), data.table::address)
  expect_false(any(ax == az)) |> errorfun()
  expect_false(any(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- ss2_x(z, vars = cols)
  ax <- ss2_x(x, vars = cols)
  ay <- ss2_x(y, vars = cols)
  expect_false(identical(ax, az)) |> errorfun()
  expect_false(identical(ay, az)) |> errorfun()
  expect_true(identical(ax, ay)) |> errorfun()
  
  az <- sapply(ss2_wo(z, vars = cols), data.table::address)
  ax <- sapply(ss2_wo(x, vars = cols), data.table::address)
  ay <- sapply(ss2_wo(y, vars = cols), data.table::address)
  expect_true(all(ax == az)) |> errorfun()
  expect_true(all(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- ss2_wo(z, vars = cols)
  ax <- ss2_wo(x, vars = cols)
  ay <- ss2_wo(y, vars = cols)
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

  z <- ss2_mod(x, obs = rows, vars = cols, tf = \(x)x[1])
  
  # note: selecting cols is like selecting list elements
  # (i.e. result is a list)
  # whereas selecting rows is like selecting recursive list elements
  # (i.e. result is the object pointed to)
  
  az <- sapply(ss2_x(z, vars = cols), data.table::address)
  ax <- sapply(ss2_x(x, vars = cols), data.table::address)
  ay <- sapply(ss2_x(y, vars = cols), data.table::address)
  expect_false(any(ax == az)) |> errorfun()
  expect_false(any(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- ss2_x(z, vars = cols)
  ax <- ss2_x(x, vars = cols)
  ay <- ss2_x(y, vars = cols)
  expect_false(identical(ax, az)) |> errorfun()
  expect_false(identical(ay, az)) |> errorfun()
  expect_true(identical(ax, ay)) |> errorfun()
  
  az <- sapply(ss2_wo(z, vars = cols), data.table::address)
  ax <- sapply(ss2_wo(x, vars = cols), data.table::address)
  ay <- sapply(ss2_wo(y, vars = cols), data.table::address)
  expect_true(all(ax == az)) |> errorfun()
  expect_true(all(ay == az)) |> errorfun()
  expect_true(all(ax == ay)) |> errorfun()
  
  az <- ss2_wo(z, vars = cols)
  ax <- ss2_wo(x, vars = cols)
  ay <- ss2_wo(y, vars = cols)
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
z <- i2_mod(x, i = i, tf = \(x)x[1])

az <- sapply(i2_x(z, i = i), data.table::address)
ax <- sapply(i2_x(x, i = i), data.table::address)
ay <- sapply(i2_x(y, i = i), data.table::address)
expect_false(any(ax == az))
expect_false(any(ay == az))
expect_true(all(ax == ay))

az <- i2_x(z, i = i)
ax <- i2_x(x, i = i)
ay <- i2_x(y, i = i)
expect_false(identical(ax, az))
expect_false(identical(ay, az))
expect_true(identical(ax, ay))

az <- sapply(i2_wo(z, i = i), data.table::address)
ax <- sapply(i2_wo(x, i = i), data.table::address)
ay <- sapply(i2_wo(y, i = i), data.table::address)
expect_true(all(ax == az))
expect_true(all(ay == az))
expect_true(all(ax == ay))

az <- i2_wo(z, i = i)
ax <- i2_wo(x, i = i)
ay <- i2_wo(y, i = i)
expect_true(identical(ax, az))
expect_true(identical(ay, az))
expect_true(identical(ax, ay))

enumerate <- enumerate + 12

