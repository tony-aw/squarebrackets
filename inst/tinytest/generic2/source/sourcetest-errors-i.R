
# i ====

if(!test_PassByReference) {
  
  xlist <- list(
    as.list(1:10),
    array(as.list(1:27), dim = c(3,3,3))
  )
  
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
      sb_test(xlist[[i]], i = c(0, 0)),
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
  
  
  xlist <- list(
    as.list(1:10),
    array(as.list(1:27), dim = c(3,3,3))
  )
  
  
  for(i in 1:length(xlist)) {
    expect_error(
      sb_test(xlist[[i]], i = "a"),
      pattern = "no names present",
      fixed = TRUE
    )|> errorfun()
    enumerate <- enumerate + 1
  }
  
}



# multi ====
if(test_PassByReference) {
  xlist <- list(
    dt = data.table::data.table(a = 1:26, b = letters),
    tt = tidytable::tidytable(a = 1:26, b = letters)
  )
}
if(!test_PassByReference) {
  xlist <- list(
    df = data.frame(a = 1:26, b = letters),
    dt = data.table::data.table(a = 1:26, b = letters),
    tb = tibble::tibble(a = 1:26, b = letters),
    tt = tidytable::tidytable(a = 1:26, b = letters)
  )
}


# duplicates ====
if(!test_allow_duplicates && !test_PassByReference) {
  x <- as.list(1:10)
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a"), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- matrix(as.list(1:10), ncol=2)
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
  x <- as.list(1:10)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
  x <- matrix(as.list(1:20), nrow = 5)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
  x <- array(as.list(1:27), c(3,3,3))
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
}


if(!test_PassByReference) {
  x <- as.list(1:10)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
  x <- matrix(as.list(1:20), nrow = 5)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
  x <- array(as.list(1:27), c(3,3,3))
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
}



