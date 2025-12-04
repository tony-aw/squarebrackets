
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, s, d, tf) {
  x <- data.table::copy(x)
  ss_set.default(x, s, d, tf = tf)
  return(x)
}
tempfun1 <- function(x, s, tf) {
  ind <- ss_icom(x, s, d)
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


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:9) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutatomic(array(x.data[[iType]], x.dim))
      s <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      d <- 1:length(x.dim)
      
      expected[[i]] <- tempfun1(x, s, tf.funs[[iType]])
      out[[i]] <- tempfun2(x, s, d, tf.funs[[iType]])
      
      x <- data.table::copy(x)
      x2 <- x
      ss_set.default(x, s, d, tf = tf.funs[[iType]])
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)

