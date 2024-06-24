
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, sub, dims, tf) {
  x <- data.table::copy(x)
  sb_set.array(x, sub, dims, tf = tf)
  return(x)
}
tempfun1 <- function(x, sub, tf) {
  ind <- sub2ind(sub, dim(x))
  x[ind] <- tf(x[ind])
  return(x)
}

tf.funs <- list(
  collapse::fmode.default,
  function(x)sum(x, na.rm = TRUE) |> as.integer(),
  function(x)mean.default(x, na.rm = TRUE),
  function(x){sum(x=="a", na.rm = TRUE) |> as.character()},
  function(x)mean.default(x, na.rm = TRUE),
  function(x){as.integer(x) |> mean() |> as.integer() |> as.raw() }
)

generate_data <- function(x.len) {
  list(
    sample(c(TRUE, FALSE, NA), x.len, TRUE),
    as.integer(sample(c(1:x.len - 1, NA))),
    sample(c(rnorm(x.len - 4), NA, NaN, Inf, -Inf), x.len),
    sample(c(stringi::stri_rand_strings(x.len - 1, 26), NA)),
    as.complex(sample(c(rnorm(x.len - 1), NA))),
    as.raw(sample(1:100, x.len, TRUE))
  )
}

n <- 5

# 6D array ====
x.dim <- rep(n, 6)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(array(x.data[[j]], x.dim))
    ind1 <- sample(1:n, 4, FALSE)
    ind2 <- sample(1:n, 4, FALSE)
    ind3 <- sample(1:n, 4, FALSE)
    ind4 <- sample(1:n, 4, FALSE)
    ind5 <- sample(1:n, 4, FALSE)
    ind6 <- sample(1:n, 4, FALSE)
    sub <- list(ind1, ind2, ind3, ind4, ind5, ind6)
    dims <- 1:length(x.dim)
    
    expect_equal(
      tempfun1(x, sub, tf.funs[[j]]), tempfun2(x, sub, dims, tf.funs[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set.array(x, sub, dims, tf = tf.funs[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
  }
  
}
enumerate <- enumerate + 12


# 5D array ====
x.dim <- rep(n, 5)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(array(x.data[[j]], x.dim))
    ind1 <- sample(1:n, 4, FALSE)
    ind2 <- sample(1:n, 4, FALSE)
    ind3 <- sample(1:n, 4, FALSE)
    ind4 <- sample(1:n, 4, FALSE)
    ind5 <- sample(1:n, 4, FALSE)
    sub <- list(ind1, ind2, ind3, ind4, ind5)
    dims <- 1:length(x.dim)
    
    expect_equal(
      tempfun1(x, sub, tf.funs[[j]]), tempfun2(x, sub, dims, tf.funs[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set.array(x, sub, dims, tf = tf.funs[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
  }
  
}
enumerate <- enumerate + 12


# 4D array ====
x.dim <- rep(n, 4)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(array(x.data[[j]], x.dim))
    ind1 <- sample(1:n, 4, FALSE)
    ind2 <- sample(1:n, 4, FALSE)
    ind3 <- sample(1:n, 4, FALSE)
    ind4 <- sample(1:n, 4, FALSE)
    sub <- list(ind1, ind2, ind3, ind4)
    dims <- 1:length(x.dim)
    
    expect_equal(
      tempfun1(x, sub, tf.funs[[j]]), tempfun2(x, sub, dims, tf.funs[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set.array(x, sub, dims, tf = tf.funs[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
  }
  
}
enumerate <- enumerate + 12



# 3D array ====
x.dim <- rep(n, 3)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(array(x.data[[j]], x.dim))
    ind1 <- sample(1:n, 4, FALSE)
    ind2 <- sample(1:n, 4, FALSE)
    ind3 <- sample(1:n, 4, FALSE)
    sub <- list(ind1, ind2, ind3)
    dims <- 1:length(x.dim)
    
    expect_equal(
      tempfun1(x, sub, tf.funs[[j]]), tempfun2(x, sub, dims, tf.funs[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set.array(x, sub, dims, tf = tf.funs[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
  }
  
}
enumerate <- enumerate + 12



# 2D array ====
x.dim <- rep(n, 2)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(array(x.data[[j]], x.dim))
    ind1 <- sample(1:n, 4, FALSE)
    ind2 <- sample(1:n, 4, FALSE)
    sub <- list(ind1, ind2)
    dims <- 1:length(x.dim)
    
    expect_equal(
      tempfun1(x, sub, tf.funs[[j]]), tempfun2(x, sub, dims, tf.funs[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set.array(x, sub, dims, tf = tf.funs[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
  }
  
}
enumerate <- enumerate + 12


