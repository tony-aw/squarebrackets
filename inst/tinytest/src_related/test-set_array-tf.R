
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, sub, dims, tf) {
  x <- data.table::copy(x)
  sb_set.array(x, sub, dims, tf = tf)
  return(x)
}
tempfun1 <- function(x, sub, tf) {
  ind <- idx.array(x, sub, dims)
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
    sample(c(rnorm(x.len), NA, NaN, Inf, -Inf), x.len),
    sample(c(stringi::stri_rand_strings(x.len, 26), NA)),
    as.complex(sample(c(rnorm(x.len - 1), NA))),
    as.raw(sample(1:100, x.len, TRUE))
  )
}


for(iSample in 1:10) {
  for(iDim in 2:7) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      expect_equal(
        tempfun1(x, sub, tf.funs[[iType]]), tempfun2(x, sub, dims, tf.funs[[iType]])
      ) |> errorfun() # test indexing & atomic type recognition
      
      x2 <- x
      sb_set.array(x, sub, dims, tf = tf.funs[[iType]])
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
    }
  }
}
