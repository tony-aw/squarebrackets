
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# seq_rec2 ====

expect_equal( # Fibonacci numbers
  seq_rec2(),
  c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
)
expect_equal( # Fibonacci numbers
  seq_rec2(rev = TRUE),
  c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
)

expect_equal( # Lucas numbers
  seq_rec2(inits = c(2,1)),
  c(2, 1, 3, 4, 7, 11, 18, 29, 47, 76)
)
expect_equal( # Lucas numbers
  seq_rec2(inits = c(2,1), rev = TRUE),
  c(2, 1, 3, 4, 7, 11, 18, 29, 47, 76)
)

expect_equal( # Pell numbers
  seq_rec2(m = c(2L, 1L)),
  c(0, 1, 2, 5, 12, 29, 70, 169, 408, 985)
)
expect_equal( # Pell numbers
  seq_rec2(m = c(1L, 2L), rev = TRUE),
  c(0, 1, 2, 5, 12, 29, 70, 169, 408, 985)
)

expect_equal( # https://oeis.org/A077957
  seq_rec2(inits = c(1, 0), m = c(0L, 2L)),
  c(1, 0, 2, 0, 4, 0, 8, 0, 16, 0)
)
expect_equal( # https://oeis.org/A077957
  seq_rec2(inits = c(1, 0), m = c(2L, 0L), rev = TRUE),
  c(1, 0, 2, 0, 4, 0, 8, 0, 16, 0)
)

expect_equal( # Jacobsthal numbers
  seq_rec2(m = c(1L, 2L)),
  c(0, 1, 1, 3, 5, 11, 21, 43, 85, 171)
)
expect_equal( # Jacobsthal numbers
  seq_rec2(m = c(2L, 1L), rev = TRUE),
  c(0, 1, 1, 3, 5, 11, 21, 43, 85, 171)
)

enumerate <- enumerate + 10


tempfun <- function(
    inits = c(0L, 1L),
    n = 10L,
    s = c(0L, 0L),
    m = c(1L, 1L),
    inop = `+`,
    form = 1L,
    rev = FALSE
) {
  x <- numeric(n)
  x[1:2] <- inits
  
  if(!rev) {
    prev1 <- 1L
    prev2 <- 2L
  }
  if(rev) {
    prev1 <- 2L
    prev2 <- 1L
  }
  
  if(form == 1) {
    for (i in 3:n){
      x[i] = inop((s[1] + m[1] * x[i-prev1]), (s[2] + m[2] * x[i-prev2]));
    }
  }
  if(form == 2) {
    for (i in 3:n){
      x[i] = inop((m[1] * (x[i-prev1] + s[1])), (m[2] * (x[i-prev2] + s[2])));
    }
  }
  
  return(x)
  
}

# try all manner of combinations
slist <- list(c(0L, 0L), c(1L, 1L), c(0L, 1L), c(1L, 0L))
mlist <- list(c(0L, 0L), c(1L, 1L), c(0L, 1L), c(1L, 0L), c(1L, 2L), c(2L, 1L))
nlist <- c(10L, 11L)
initslist <- list(c(0L, 1L), c(10L, 11L))
inops_nms <- list("+", "-", "*", "/")
inops_funs <- list(`+`, `-`, `*`, `/`)
formlist <- list(1L, 2L)
revlist <- c(TRUE, FALSE)

for(s in seq_along(slist)) {
  for(m in seq_along(mlist)) {
    for(n in nlist) {
      for(inop in seq_along(inops_funs)) {
        for(form in formlist) {
          for(rev in revlist) {
            for(i in seq_along(initslist)) {
              expect_equal(
                tempfun(initslist[[i]], n, slist[[s]], mlist[[m]], inops_funs[[inop]], form, rev),
                seq_rec2(initslist[[i]], n, slist[[s]], mlist[[m]], inops_nms[[inop]], form, rev)
              ) |> errorfun()
              enumerate <- enumerate + 1
              print(seq_rec2(initslist[[i]], n, slist[[s]], mlist[[m]], inops_nms[[inop]], form, rev))
            }
          }
        }
      }
    }
  }
}

expect_equal(
  tempfun(),
  seq_rec2()
)
expect_equal(
  tempfun(s = c(1L, 1L)),
  seq_rec2(s = c(1L, 1L))
)

enumerate <- enumerate + 2


# .rcpp_seq_mlen ====
expect_equal(
  squarebrackets:::.rcpp_seq_mlen(1:10),
  lapply(1:10, seq_len)
)

enumerate <- enumerate + 1


# seq_names ====
expect_error(
  seq_names(character(0), "a", "b"),
  pattern = "no names given"
)
expect_error(
  seq_names(c("", letters), "a", "b"),
  pattern = "empty names not allowed"
)
expect_error(
  seq_names(letters, "aa", "b"),
  pattern = "`start` not in `names`",
  fixed = TRUE
)
expect_error(
  seq_names(letters, "a", "bb"),
  pattern = "`end` not in `names`",
  fixed = TRUE
)
expect_error(
  seq_names(letters, "a", "y", inv = NA),
  pattern = "`inv` must be `TRUE` or `FALSE`",
  fixed = TRUE
)
expect_equal(
  seq_names(rev(letters), "a", "y"),
  26:2
)
expect_equal(
  seq_names(letters, "y", "a"),
  25:1
)
enumerate <- enumerate + 7


