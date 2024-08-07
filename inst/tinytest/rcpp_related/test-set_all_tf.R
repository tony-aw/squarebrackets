
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, tf) {
  x <- data.table::copy(x)
  sb_set.array(x, tf = tf)
  return(x)
}
tempfun1 <- function(x, tf) {
  x[] <- tf(x)
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
    
    expect_equal(
      tempfun1(x, tf.funs[[j]]), tempfun2(x, tf.funs[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set.array(x, tf = tf.funs[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
  }
  
}
enumerate <- enumerate + 12


