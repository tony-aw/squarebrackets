
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L
.cp_val <- squarebrackets:::.cp_val



# corrections for numeric ====
y <- as.integer(1:10)
v <- as.integer(1:2)
lst <- .cp_val(y, v, TRUE, sys.call())
expect_true(is.double(lst$v))
enumerate <- enumerate + 1L


# corrections for factor ====
x <- factor(letters)
v <- "b"
lst <- .cp_val(x, v, TRUE, sys.call())
expect_equal(
  lst$v, 2L
)
v <- factor("b", levels = levels(x))
lst <- .cp_val(x, v, TRUE, sys.call())
expect_equal(
  lst$v, 2L
)
enumerate <- enumerate + 2L


# no corrections ====
# logical
y <- c(TRUE, FALSE, NA)
v <- TRUE
r <- TRUE
lst <- .cp_val(y, v, r, sys.call())
expect_equal(
  v, lst$v
)
expect_equal(
  r, lst$r
)

# string
y <- sample(letters)
v <- c("a", "b")
r <- TRUE
lst <- .cp_val(y, v, r, sys.call())
expect_equal(
  v, lst$v
)
expect_equal(
  r, lst$r
)

# ra
y <- as.raw(1:10)
v <- as.raw(2L)
r <- TRUE
lst <- .cp_val(y, v, r, sys.call())
expect_equal(
  v, lst$v
)
expect_equal(
  r, lst$r
)

# complex
y <- 1:10 + 1:10 *-1i
v <- 1-1i
r <- TRUE
lst <- .cp_val(y, v, r, sys.call())
expect_equal(
  v, lst$v
)
expect_equal(
  r, lst$r
)

enumerate <- enumerate + 7L




