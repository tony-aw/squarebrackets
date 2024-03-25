
general_names <- combn(letters, 2) |> apply(2, paste, collapse = "")


# uniquely named list ====
x <- -sample.int(48) |> as.list()
names(x) <- general_names[1:48]
temp.fun(x, indx_named)


# unnamed list ====
x <- -sample.int(48) |> as.list()
temp.fun(x, indx_general)


# non-uniquely named list ====
x <- -sample.int(48) |> as.list()
names(x) <- c(general_names[1:45], "ab", "ab", NA)
temp.fun(x, indx_named)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb2_x(x, i = c("ab", "ab", "ab")),
    rep(x[which(names(x) %in% "ab")], 3)
  ) |> errorfun()
}




# uniquely named array ====
x <- -sample.int(48) |> as.list() |> array(dim = c(3,4,4))
names(x) <- general_names[1:48]
temp.fun(x, indx_named)


# unnamed array ====
x <- -sample.int(48) |> as.list() |> array(dim = c(3,4,4))
temp.fun(x, indx_general)


# non-uniquely named array ====
x <- -sample.int(48) |> as.list() |> array(dim = c(3,4,4))
names(x) <- c(general_names[1:45], "ab", "ab", NA)
temp.fun(x, indx_named)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb2_x(x, i = c("ab", "ab", "ab")),
    rep(x[which(names(x) %in% "ab")], 3)
  ) |> errorfun()
}

