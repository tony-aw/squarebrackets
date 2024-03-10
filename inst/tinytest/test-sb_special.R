
enumerate <- 0

# sb_str ====
expect_equal(
  sb_str("hello", 5:1),
  "olleh"
)
expect_equal(
  sb_str("hello", c(1:5, 5)),
  "helloo"
)
expect_equal(
  sb_str("hello", 2:5),
  "ello"
)
expect_equal(
  sb_str("hello", 1:4, "world", 1:4),
  "worlo"
)
expect_equal(
  sb_str("hello", 1:4, "world", 2:5),
  "orldo"
)
expect_equal(
  sb_str("hello", 5:2, "world", 1:4),
  "hlrow"
)
expect_error(
  sb_str("hello", 0),
  pattern = "`ind` must be a vector of strictly positive integers"
)
expect_error(
  sb_str("hello", 1:4, "world", 1:5),
  pattern = "`ind` and `rp.ind` must be of same size"
)
expect_error(
  sb_str("hello", -1:-4, "world", -1:-4),
  pattern = "both indices must be strictly positive integers"
)
expect_error(
  sb_str("hello", 1:4, "world", -1:-4),
  pattern = "both indices must be strictly positive integers"
)
expect_error(
  sb_str("hello", -1:-4, "world", 1:4),
  pattern = "both indices must be strictly positive integers"
)

enumerate <- enumerate + 11
