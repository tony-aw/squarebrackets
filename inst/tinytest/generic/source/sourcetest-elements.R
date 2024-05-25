
general_names <- combn(letters, 2) |> apply(2, paste, collapse = "")


indx_general <- list(
  integer(0),
  1, 1:3, 3:1, c(2, 3, 1),
  c(rep(TRUE, 24), rep(FALSE, 24)),
  rep(TRUE, 48), rep(FALSE, 48),
  c(TRUE, rep(FALSE, 47)), c(FALSE, rep(TRUE, 47)),
  function(x) x>5
)
if(test_allow_duplicates) {
  indx_general <- c(indx_general, list(c(1, 1, 1)))
}

indx_named <- c(indx_general, "ab", list(c("ab", "ac")), list(c("ac", "ab")))
if(test_allow_duplicates) {
  indx_named <- c(indx_named, list(c("ab", "ab")))
}

# uniquely named vector ====
x <- -sample.int(48)
names(x) <- general_names[1:48]
temp.fun(x, indx_named)


# unnamed vector ====
x <- -sample.int(48)
temp.fun(x, indx_general)


# non-uniquely named vector ====
x <- -sample.int(48)
names(x) <- c(general_names[1:45], "ab", "ab", NA)
temp.fun(x, indx_named)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, i = c("ab", "ab", "ab")),
    rep(x[which(names(x) %in% "ab")], 3)
  ) |> errorfun()
}


# uniquely named matrix====
x <- -sample.int(48) |> matrix(ncol=2)
names(x) <- general_names[1:48]
temp.fun(x, indx_named)


# unnamed matrix ====
x <- -sample.int(48) |> matrix(ncol=2)
temp.fun(x, indx_general)


# non-uniquely named matrix ====
x <- -sample.int(48) |> matrix(ncol = 2)
names(x) <- c(general_names[1:45], "ab", "ab", NA)
temp.fun(x, indx_named)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, i = c("ab", "ab", "ab")),
    rep(x[which(names(x) %in% "ab")], 3)
  ) |> errorfun()
}


# uniquely named array ====
x <- -sample.int(48) |> array(dim = c(3,4,4))
names(x) <- general_names[1:48]
temp.fun(x, indx_named)


# unnamed array ====
x <- -sample.int(48) |> array(dim = c(3,4,4))
temp.fun(x, indx_general)

# non-uniquely named array ====
x <- -sample.int(48) |> array(dim = c(3,4,4))
names(x) <- c(general_names[1:45], "ab", "ab", NA)
temp.fun(x, indx_named)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, i = c("ab", "ab", "ab")),
    rep(x[which(names(x) %in% "ab")], 3)
  ) |> errorfun()
}

