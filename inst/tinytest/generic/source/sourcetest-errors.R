
# i ====
if(!test_PassByReference){
  xlist <- list(
    1:10,
    matrix(1:10, ncol=2),
    array(1:27, dim = c(3,3,3))
  )
}
if(test_PassByReference) {
  xlist <- list(
    1:10,
    matrix(1:10, ncol=2),
    array(1:27, dim = c(3,3,3))
  )
  xlist <- lapply(xlist, \(x) if(is.atomic(x)){as.mutable_atomic(x)} else{x})
}



for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = -1:-10),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = 0),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = 1000),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = sample(c(TRUE, FALSE), size = length(xlist[[i]]) - 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = sample(c(TRUE, FALSE), size = length(xlist[[i]]) + 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}


if(!test_PassByReference){
  xlist <- list(
    1:10,
    matrix(1:10, ncol=2),
    array(1:27, dim = c(3,3,3))
  )
}
if(test_PassByReference) {
  xlist <- list(
    1:10,
    matrix(1:10, ncol=2),
    array(1:27, dim = c(3,3,3))
  )
  xlist <- lapply(xlist, \(x) if(is.atomic(x)){as.mutable_atomic(x)} else{x})
}


for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = "a"),
    pattern = "no names present",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}



# dimensions ==== 
x <- as.mutable_atomic(array(1:27, c(3,3,3)))
expect_error(
  sb_test(x, s = list(1:10, 2:5), d = c(1:3)),
  pattern = "if `s` is a list, `length(s)` must equal `length(d)`",
  fixed = TRUE
)
expect_error(
  sb_test(x, s = list(1:3, 1:3), d = c(1,6)),
  pattern = "`d` out of range",
  fixed = TRUE
)
expect_error(
  sb_test(x, s = list(-1:-5), d = 1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, s = list(0), d = 1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list(1000), d = 1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, list(sample(c(TRUE, FALSE), size = nrow(x) - 1, replace = TRUE)), d = 1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x,list(sample(c(TRUE, FALSE), size = nrow(x) + 1, replace = TRUE)), d = 1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list("a"), d = 1),
  pattern = "no names present",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1




# duplicates ====
if(!test_allow_duplicates) {
  x <- as.mutable_atomic(1:10)
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a"), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- as.mutable_atomic(matrix(1:10, ncol=2))
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a"), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  expect_error(
    sb_test(x, c(1,1,1), 2L, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, c(1,1,1), 1L, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- as.mutable_atomic(array(1:27, c(3,3,3)))
  rownames(x) <- c("a", "a", "b")
  expect_error(
    sb_test(x, list(c(1,1,1)), d = 1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  )
  expect_error(
    sb_test(x, list(c("a", "a")), d = 1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  )
  
}


# generic dots ====
if(test_allow_duplicates) {
  x <- 1:10
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- matrix(1:20, nrow = 5)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- array(1:27, c(3,3,3))
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
}

x <- mutable_atomic(1:10)
expect_error(
  sb_test(x, foo = TRUE),
  pattern = "unknown arguments given"
)

x <- mutable_atomic(1:20, dim = c(4,5))
expect_error(
  sb_test(x, foo = TRUE),
  pattern = "unknown arguments given"
)

x <- mutable_atomic(1:27, dim = c(3,3,3))
expect_error(
  sb_test(x, foo = TRUE),
  pattern = "unknown arguments given"
)


