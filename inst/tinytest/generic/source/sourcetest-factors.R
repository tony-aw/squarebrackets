
# uniquely named factor ====
x <- factor(month.abb[1:10])
names(x) <- letters[1:10]
temp.fun(x, indx_named)


# unnamed factor ====
x <- factor(month.abb[1:10])
temp.fun(x, indx_general)


# non-uniquely named factor ====
x <- factor(month.abb[1:10])
names(x) <- c(letters[1:8], "a", NA)
temp.fun(x, indx_named)


if(isTRUE(test_allow_duplicates)) {
  expect_equal(sb_x(x, "a"), x[which(names(x) == "a")])
  expect_equal(sb_x(x, c("a", "a", "a")), rep(x[which(names(x) == "a")], 3))
  
  expect_equal(
    sb_x(x, lvl = c("Jan", "Jan", "Jan")),
    rep(x[x == "Jan"], 3)
  )
  expect_equal(
    sb_x(x, lvl = c("Jan", "Jan", "Jan"), drop = TRUE),
    rep(x[x == "Jan", drop = TRUE], 3)
  )
}



