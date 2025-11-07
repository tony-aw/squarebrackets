
# set-up ====

enumerate <- 0L

x <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 12L, TRUE),
  b = 1:12,
  c = rnorm(12),
  d = month.abb,
  e = as.factor(letters[1:12])
)

indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  out <- list(
    logical(0),
    rep(TRUE, dim.n),
    rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1,
    1:3,
    3:1,
    c(2, 3, 1),
    1 * -1i,
    1:3 * -1i,
    3:1 * -1i,
    c(2, 3, 1) * -1i,
    1 * 1i,
    1:3 * 1i,
    3:1 * 1i,
    c(2, 3, 1) * 1i
  )
  return(out)
}

indx_named <- function(x, dim.i) {
  if(dim.i == 1L) {
    out <- indx_general(x, dim.i)
  }
  if(dim.i == 2L) {
    out <- c(
      indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a"))
    )
  }
  
  return(out)
}

.dt_make_args <- squarebrackets:::.dt_make_args


# main tests ====
# these tests check if specifying indices for data.frame-like objects
# using the s,d argument pair
# is equivalent to the same using the obs,vars argument pair

rows <- indx_named(x, 1L)
cols <- indx_named(x, 2L)

list_sd <- list_ov <- vector("list", length(rows) * length(cols) * 2)
i <- 1L
for(iRow in 1:length(rows)) {
  for(iCol in 1:length(cols)) {
    for(iInv in c(TRUE, FALSE)) {
      
      s <- n(rows[[iRow]], cols[[iCol]])
      d <- 1:2
      
      list_sd[[i]] <- .dt_make_args(
        x, s = s, d = d, obs = NULL, vars = NULL,
        inv = iInv, chkdup = FALSE, sys.call()
      )
      list_ov[[i]] <- .dt_make_args(
        x, s = NULL, d = integer(0L), obs = rows[[iRow]], vars = cols[[iCol]],
        iInv, FALSE, sys.call()
      )
      
      i <- i + 1L
      
      
    }
    
    
    
  }
}

expect_equal(list_sd, list_ov)

enumerate <- enumerate + i


# formula obs tests ====

expected <- which(x$b > 5L)
out <- .dt_make_args(
  x, s = NULL, d = NULL, obs = ~ b > 5, vars = NULL,
  inv = FALSE, chkdup = FALSE, sys.call()
)
out <- out[[1L]]
expect_equal(
  out, expected
)

expected <- which(x$b <= 5L)
out <- .dt_make_args(
  x, s = NULL, d = NULL, obs = ~ b > 5, vars = NULL,
  inv = TRUE, chkdup = FALSE, sys.call()
)
out <- out[[1L]]
expect_equal(
  out, expected
)

enumerate <- enumerate + 2L



# formula vars tests ====
expected <- 2:4
out <-  .dt_make_args(
  x, s = NULL, d = NULL, obs = NULL, vars = b ~ d,
  inv = FALSE, chkdup = FALSE, sys.call()
)
out <- out[[2L]]
expect_equal(
  out, expected
)

expected <- c(1, 5)
out <-  .dt_make_args(
  x, s = NULL, d = NULL, obs = NULL, vars = b ~ d,
  inv = TRUE, chkdup = FALSE, sys.call()
)
out <- out[[2L]]
expect_equal(
  out, expected
)

enumerate <- enumerate + 2L



# function vars tests ====
expected <- 2:3
out <-  .dt_make_args(
  x, s = NULL, d = NULL, obs = NULL, vars = is.numeric,
  inv = FALSE, chkdup = FALSE, sys.call()
)
out <- out[[2L]]
expect_equal(
  out, expected
)

expected <- c(1, 4:5)
out <-  .dt_make_args(
  x, s = NULL, d = NULL, obs = NULL, vars = is.numeric,
  inv = TRUE, chkdup = FALSE, sys.call()
)
out <- out[[2L]]
expect_equal(
  out, expected
)

enumerate <- enumerate + 2L


