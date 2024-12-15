
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




# row ====
xlist <- list(
  data.table::data.table(a = 1:10, b = 1:10)
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = -1:-5),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = 0),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = 1000),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = sample(c(TRUE, FALSE), size = nrow(xlist[[i]]) - 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = sample(c(TRUE, FALSE), size = nrow(xlist[[i]]) + 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}


# col ====
xlist <- list(
  data.table::data.table(a = 1:10, b = 1:10, c=1:10, d=1:10, e=1:10)
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = -1:-5),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = 0),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = 1000),
    pattern = "integers must be >= 1 and <= bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = sample(c(TRUE, FALSE), size = ncol(xlist[[i]]) - 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = sample(c(TRUE, FALSE), size = ncol(xlist[[i]]) + 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}


# dimensions ==== 
if(!test_PassByReference) {
  x <- array(as.list(1:27), c(3,3,3))
  expect_error(
    sb_test(x, s = list(1:10, 2:5), d = c(1:3)),
    pattern = "if `s` is a list, `length(s)` must equal `length(d)`",
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

for(i in 1:length(xlist)) {
  x <- xlist[[i]]
  expect_error(
    sb_test(x, filter = "foo"),
    pattern = "`filter` must be a formula"
  ) |> errorfun()
  expect_error(
    sb_test(x, filter = ~ mean(a)),
    pattern = "invalid formula given"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = "is.numeric"),
    pattern = "`vars` must be a function"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = "is.numeric"),
    pattern = "`vars` must be a function"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = mean),
    pattern = "values must be type 'logical'"
  ) |> errorfun()
  enumerate <- enumerate + 5
}

for (i in 1:length(xlist)) {
  x <- xlist[[i]]
  colnames(x) <- c("a", "a")
  expect_error(
    sb_test(x, col=1),
    pattern = "`x` does not have unique variable names for all columns; \n fix this before subsetting"
  ) |> errorfun()
  enumerate <- enumerate + 1
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

for(i in 1:length(xlist)) {
  x <- xlist[[i]]
  expect_error(
    sb_test(x, row = 1:2, filter = ~ a>2),
    pattern = "cannot specify both `filter` and `row`"
  ) |> errorfun()
  expect_error(
    sb_test(x, col = 1:2, vars = is.numeric),
    pattern = "cannot specify both `vars` and `col`"
  ) |> errorfun()
  enumerate <- enumerate + 1
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
  
  x <- array(as.list(1:27), c(3,3,3))
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
  x <- as.list(1:10)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- matrix(as.list(1:20), nrow = 5)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- array(as.list(1:27), c(3,3,3))
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- data.frame(a = letters[1:10], b = 1:10)
  expect_error(
    sb_test(x, inv = TRUE),
    pattern = "unknown arguments given"
  )
  
}


if(!test_PassByReference) {
  x <- as.list(1:10)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- matrix(as.list(1:20), nrow = 5)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- array(as.list(1:27), c(3,3,3))
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  )
  
  x <- data.frame(a = letters[1:10], b = 1:10)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  )
}

if(test_PassByReference) {
  x <- data.table::data.table(a = letters[1:10], b = 1:10)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  )
}
