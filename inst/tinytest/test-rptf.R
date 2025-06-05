
enumerate <- 0

# atomic vector ====
x <- mutatomic::mutatomic(1:10)
expect_error(
  i_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_set(x, inv = TRUE),
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


# atomic matrix ====
x <- mutatomic::mutatomic(1:20, dim = c(5,4))
expect_error(
  ss_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# atomic 3d array ====
x <- mutatomic::mutatomic(1:27, dim = c(3,3,3))
expect_error(
  ss_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 4L


# recursive vector ====
x <- as.list(1:10)
expect_error(
  i2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 4L


# recursive matrix ====
x <- matrix(as.list(1:20, ncol = 4))
expect_error(
  i2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 4L


# recursive 3d array ====
x <- array(as.list(1:27), dim = c(3,3,3))
expect_error(
  i2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  i2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 4L


# data.frame ====
x <- data.frame(a = 1:10, b = letters[1:10])
expect_error(
  ss2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 2L


# data.table ====
x <- data.table::data.table(a = 1:10, b = letters[1:10])
expect_error(
  ss2_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  ss2_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
enumerate <- enumerate + 2L
