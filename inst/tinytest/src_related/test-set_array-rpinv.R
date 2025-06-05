
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, s, d, rp) {
  x <- data.table::copy(x)
  ss_set.default(x, s, d, inv = TRUE, rp = rp)
  return(x)
}
tempfun1 <- function(x, s, rp) {
  ind <- idx.array(x, s, d, inv = TRUE)
  x[ind] <- rp
  return(x)
}

rp.lst <- list(
  NA,
  -1000L,
  -Inf,
  "NA; NaN; Inf; -Inf",
  as.complex(-1000),
  as.raw(0)
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
      
      expected[[i]] <- tempfun1(x, s, rp.lst[[iType]])
      out[[i]] <- tempfun2(x, s, d, rp.lst[[iType]])
      
      x <- data.table::copy(x)
      x2 <- x
      ss_set.default(x, s, d, inv = TRUE, rp = rp.lst[[iType]])
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)

