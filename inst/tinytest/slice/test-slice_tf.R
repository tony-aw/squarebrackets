
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


tempfun1 <- function(x, from = NULL, to = NULL, by = 1L, tf) {
  myslice <- cp_seq(x, 0L, from, to, by)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  ind <- seq(start, end, by)
  x[ind] <- tf(x[ind])
  return(x)
}

tempfun2 <- function(x, from = NULL, to = NULL, by = 1L, tf) {
  x <- data.table::copy(x)
  x2 <- x
  slice_set(x, from, to, by, tf = tf)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}


list_fromto <- list(
  1, -1i, 2, -2i, 10, -10i, 100, -100i, 99, -99i, 98, -98i
)
list_by <- list(
  1, 2, 3, 100, -1, -2, -3, -100
)
n <- length(list_fromto) * length(list_fromto) * length(list_by)



# equal length replacement ====
x <- mutatomic::mutatomic(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      expected[[counter]] <- tempfun1(x, from, to, by, \(x) as.integer(x^2))
      out[[counter]] <- tempfun2(x, from, to, by, \(x) x^2)
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
)


# singular replacement ====

x <- mutatomic::mutatomic(1:100)

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_by)) {
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_by[[iB]]
      expected[[counter]] <- tempfun1(x, from, to, by, \(x) x[1])
      out[[counter]] <- tempfun2(x, from, to, by, \(x) x[1])
      counter <- counter + 1L
      enumerate <- enumerate + 1L
    }
  }
}

expect_equal(
  expected, out
)



# atomic types checks ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  rnorm(100),
  sample(c(NA, NaN, -Inf, Inf, 0), 100, TRUE),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  as.complex(c(1:99, NA)),
  as.raw(0:99),
  rep(NA, 100)
)
x.data <- lapply(x.data, mutatomic::as.mutatomic)

expected <- out <- list(8)
for(i in seq_along(x.data)) {
  x <- data.table::copy(x.data[[i]])
  
  expected[[i]] <- tempfun1(x, 3, -3i, -3, rev)
  out[[i]] <- tempfun2(x, 3, -3i, -3, tf = rev)
  
}
expect_equal(
  expected, out
)

enumerate <- enumerate + length(x.data)

