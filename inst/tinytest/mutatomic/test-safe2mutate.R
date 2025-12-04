

testfun <- function(x) {
  stopifnot_ma_safe2mutate(substitute(x), parent.frame(n = 1), sys.call())
}


x <- as.list(1:10)
expect_error(
  testfun(x),
  pattern = "is not atomic"
)


x <- 1:16

expect_error(
  testfun(x),
  pattern = "not a 'mutatomic' object"
)

mylist <- list(
  a = mutatomic(1:10)
)

expect_error(
  testfun(mylist$a),
  pattern = "only objects that exist as variables can be modified by reference"
)

`mylist$a` <- mutatomic(1:10)

expect_silent(
  testfun(`mylist$a`)
)


lockBinding("x", environment())

expect_error(
  testfun(x),
  pattern = "cannot change value of locked binding for"
)

rm(list = "x")

enumerate <- 3L

