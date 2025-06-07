
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
  xlist <- lapply(xlist, \(x) if(is.atomic(x)){as.mutatomic(x)} else{x})
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
  xlist <- lapply(xlist, \(x) if(is.atomic(x)){as.mutatomic(x)} else{x})
}


for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = "a"),
    pattern = "no names present",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}




# duplicates ====
if(!test_allow_duplicates) {
  x <- as.mutatomic(1:10)
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a"), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- as.mutatomic(matrix(1:10, ncol=2))
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a"), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
}


# generic dots ====
if(test_allow_duplicates) {
  x <- 1:10
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
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

x <- mutatomic(1:10)
expect_error(
  sb_test(x, foo = TRUE),
  pattern = "unknown arguments given"
) |> errorfun()

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


