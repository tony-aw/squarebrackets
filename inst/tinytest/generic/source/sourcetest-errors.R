
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
    pattern = "`x` has no names; fix this before subsetting",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

if(!test_PassByReference) {
  # lvl ====
  if(test_use_factors) {
    x <- factor(month.abb[1:10])
    names(x) <- letters[1:10]
    # expect_error(
    #   sb_test(x, lvl = "1"),
    #   pattern = "unknown level given"
    # ) |> errorfun()
    expect_error(
      sb_test(x, lvl = "1", i = 1),
      pattern = "cannot specify both elements and levels"
    ) |> errorfun()
    enumerate <- enumerate + 2
  }
  
  
}


# row ====
xlist <- list(
  as.mutable_atomic(matrix(1:10, ncol=2))
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


xlist <- list(
  as.mutable_atomic(matrix(1:10, ncol=2))
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = "a"),
    pattern = "`x` has no names; fix this before subsetting",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

# col ====
xlist <- list(
  as.mutable_atomic(matrix(1:10, ncol=5))
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

xlist <- list(
  as.mutable_atomic(matrix(1:10, ncol=2))
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = "a"),
    pattern =  "`x` has no names; fix this before subsetting",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

# dimensions ==== 
x <- as.mutable_atomic(array(1:27, c(3,3,3)))
expect_error(
  sb_test(x, sub = 1:10, dims = c(1,3)),
  pattern = "`sub` must be a list, and `dims` must be a integer vector",
  fixed = TRUE
)
expect_error(
  sb_test(x, sub = list(1:10), dims = c(1,3)),
  pattern = "`length(sub) != length(dims)`",
  fixed = TRUE
)
expect_error(
  sb_test(x, sub = list(1:3, 1:3), dims = c(1,6)),
  pattern = "`dims` out of range",
  fixed = TRUE
)
expect_error(
  sb_test(x, sub = list(-1:-5), dim = 1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, sub = list(0), dim = 1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list(1000), dim = 1),
  pattern = "integers must be >= 1 and <= bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, list(sample(c(TRUE, FALSE), size = nrow(x) - 1, replace = TRUE)), dim = 1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x,list(sample(c(TRUE, FALSE), size = nrow(x) + 1, replace = TRUE)), dim = 1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list("a"), dim = 1),
  pattern = "`x` has no names; fix this before subsetting",
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
  
  rownames(x) <- letters[1:5]
  colnames(x) <- letters[1:2]
  row <- list(NULL, c(1,1,1), c("a", "a", "a"))
  col <- list(NULL, c(1,1), c("a", "a"))
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      if(!is.null(row[[i]]) || !is.null(col[[j]])) {
        expect_error(
          sb_test(x, row = row[[i]], col = col[[j]], chkdup = TRUE),
          pattern = "duplicate integers or names not allowed"
        ) |> errorfun()
        enumerate <- enumerate + 1
      }
    }
  }
  
  expect_error(
    sb_test(x, col = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, row = c(1,1,1), col = c(1,1,1), chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- as.mutable_atomic(array(1:27, c(3,3,3)))
  rownames(x) <- c("a", "a", "b")
  expect_error(
    sb_test(x, list(c(1,1,1)), dim = 1, chkdup = TRUE),
    pattern = "duplicate integers or names not allowed"
  )
  expect_error(
    sb_test(x, list(c("a", "a")), dim = 1, chkdup = TRUE),
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
  
  x <- factor(letters)
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

if(test_use_factors) {
  x <- factor(letters)
  expect_error(
    sb_test(x, foo = TRUE),
    pattern = "unknown arguments given"
  )
}


