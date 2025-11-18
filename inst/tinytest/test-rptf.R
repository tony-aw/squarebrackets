
enumerate <- 0

# vector ====
x <- mutatomic(1:27, dim = c(3,3,3))
expect_error(
  ii_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 8L


# matrix ====
x <- mutatomic(1:20, dim = c(5,4))
expect_error(
  sbt_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sbt_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 4L



# 3d array ====
x <- mutatomic(1:27, dim = c(3,3,3))
expect_error(
  ss_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ii_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 4L



# data.frame ====
x <- data.frame(a = 1:10, b = letters[1:10])
expect_error(
  sbt_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sbt_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 2L


# data.table ====
x <- data.table::data.table(a = 1:10, b = letters[1:10])
expect_error(
  sbt_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sbt_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 2L

