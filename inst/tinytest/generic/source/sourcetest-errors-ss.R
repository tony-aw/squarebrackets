

# dimensions ==== 
x <- as.mutatomic(array(1:27, c(3,3,3)))
expect_error(
  sb_test(x, s = list(1:10, 2:5), use =c(1:3)),
  pattern = "`length(s)` must be 1 or equal to `length(use)`",
  fixed = TRUE
) |> errorfun()
expect_error(
  sb_test(x, s = list(1:3, 1:3), use =c(1,6)),
  pattern = "`use` out of range",
  fixed = TRUE
) |> errorfun()
expect_error(
  sb_test(x, s = list(-1:-5), use =1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, s = list(c(0,0)), use =1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list(1000), use =1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, list(sample(c(TRUE, FALSE), size = nrow(x) - 1, replace = TRUE)), use =1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x,list(sample(c(TRUE, FALSE), size = nrow(x) + 1, replace = TRUE)), use =1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list("a"), use =1),
  pattern = "no names present",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1




# duplicates ====
if(!test_allow_duplicates) {
  x <- as.mutatomic(matrix(1:10, ncol=2))
  names(x) <- letters[1:10]
  
  expect_error(
    sb_test(x, n(c(1,1,1)), 2L, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, n(c(1,1,1)), 1L, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- as.mutatomic(array(1:27, c(3,3,3)))
  rownames(x) <- c("a", "a", "b")
  expect_error(
    sb_test(x, list(c(1,1,1)), use =1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, list(c("a", "a")), use =1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
}


# generic dots ====
if(test_allow_duplicates) {
  
  x <- matrix(1:20, nrow = 5)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
  x <- array(1:27, c(3,3,3))
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
}

x <- mutatomic(1:20, dim = c(4,5))
expect_error(
  sb_test(x, foo = TRUE),
  pattern = "unknown arguments given"
) |> errorfun()

x <- mutatomic(1:27, dim = c(3,3,3))
expect_error(
  sb_test(x, foo = TRUE),
  pattern = "unknown arguments given"
) |> errorfun()


