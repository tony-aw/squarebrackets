
# atomic vector ====
x <- mutable_atomic(1:10)
expect_error(
  sb_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# atomic matrix ====
x <- mutable_atomic(1:20, dim = c(5,4))
expect_error(
  sb_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# atomic 3d array ====
x <- mutable_atomic(1:27, dim = c(3,3,3))
expect_error(
  sb_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# recursive vector ====
x <- as.list(1:10)
expect_error(
  sb2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# recursive matrix ====
x <- matrix(as.list(1:20, ncol = 4))
expect_error(
  sb2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# recursive 3d array ====
x <- array(as.list(1:27), dim = c(3,3,3))
expect_error(
  sb2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# data.frame ====
x <- data.frame(a = 1:10, b = letters[1:10])
expect_error(
  sb2_mod(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb2_mod(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)


# data.table ====
x <- data.table::data.table(a = 1:10, b = letters[1:10])
expect_error(
  sb2_set(x),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)
expect_error(
  sb2_set(x, inv = TRUE),
  pattern = "must specify either `rp` or `tf`",
  fixed = TRUE
)

enumerate <- 22
