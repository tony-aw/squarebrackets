

# dimensions ==== 
if(!test_PassByReference) {
  x <- array(as.list(1:27), c(3,3,3))
  expect_error(
    sb_test(x, s = list(1:10, 2:5), d = c(1:3)),
    pattern = "`length(s)` must equal `length(d)`",
    fixed = TRUE
  ) |> errorfun()
  
  expect_error(
    sb_test(x, s = list(-1:-5), d = 1),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
  
  
  
  expect_error(
    sb_test(x, s = list(c(0, 0)), d = 1),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
  
  
  expect_error(
    sb_test(x, list(1000), d = 1),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
  
  
  
  expect_error(
    sb_test(x, list(sample(c(TRUE, FALSE), size = nrow(x) - 1, replace = TRUE)), d = 1),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
  
  
  
  expect_error(
    sb_test(x,list(sample(c(TRUE, FALSE), size = nrow(x) + 1, replace = TRUE)), d = 1),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
  
  
  expect_error(
    sb_test(x, list("a"), d = 1),
    pattern = "no names present",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 1
  
}



# data.frame-like objects ====
if(test_PassByReference) {
  if(requireNamespace("tidytable")) {
    xlist <- list(
      dt = data.table::data.table(a = 1:26, b = letters),
      tt = tidytable::tidytable(a = 1:26, b = letters)
    )
  } else {
    xlist <- list(
      dt = data.table::data.table(a = 1:26, b = letters)
    )
  }
  
}
if(!test_PassByReference) {
  if(requireNamespace("tibble") && requireNamespace("tidytable")) {
    xlist <- list(
      df = data.frame(a = 1:26, b = letters),
      dt = data.table::data.table(a = 1:26, b = letters),
      tb = tibble::tibble(a = 1:26, b = letters),
      tt = tidytable::tidytable(a = 1:26, b = letters)
    )
  } else {
    xlist <- list(
      df = data.frame(a = 1:26, b = letters),
      dt = data.table::data.table(a = 1:26, b = letters),
    )
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
  
  x <- array(as.list(1:27), c(3,3,3))
  rownames(x) <- c("a", "a", "b")
  expect_error(
    sb_test(x, list(c(1,1,1)), d = 1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, list(c("a", "a")), d = 1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
}




# generic dots ====
if(test_allow_duplicates) {
  
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
  
  x <- data.frame(a = letters[1:10], b = 1:10)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
  
}


if(!test_PassByReference) {
  
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
  
  x <- data.frame(a = letters[1:10], b = 1:10)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
}

if(test_PassByReference) {
  x <- data.table::data.table(a = letters[1:10], b = 1:10)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  ) |> errorfun()
}

