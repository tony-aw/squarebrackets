
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
.abind <- squarebrackets:::.abind
.asub <- squarebrackets:::.asub

# one-dimensional objects ====
y <- 1:10
new <- 11:20
as_funs_lst <- list(
  as.vector,
  as.factor
)

for(i in seq_len(length(as_funs_lst))) {
  as.fun <- as_funs_lst[[i]]
  expect_equivalent(
    sb_before(as.fun(y), as.fun(new)),
    c(as.fun(new), as.fun(y))
  ) |> errorfun()
  expect_equivalent(
    sb_after(as.fun(y), as.fun(new)),
    c(as.fun(y), as.fun(new))
  ) |> errorfun()
  expect_equivalent(
    sb_before(as.fun(y), as.fun(new), 9),
    c(as.fun(y)[1:8], as.fun(new), as.fun(y)[9:10])
  ) |> errorfun()
  expect_equivalent(
    sb_after(as.fun(y), as.fun(new), 2),
    c(as.fun(y)[1:2], as.fun(new), as.fun(y)[3:10])
  ) |> errorfun()
  enumerate <- enumerate + 4
}


# dimensions ====
y <- matrix(1:20, ncol = 5)
new <- matrix(21:40, ncol = 5)
for(margin in 1:2) {
  expect_equivalent(
    sb_before(y, new, margin),
    .abind(new, y, ._along = margin)
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin),
    .abind(y, new, ._along = margin)
  ) |> errorfun()
  expect_equivalent(
    sb_before(y, new, margin, 4),
    .abind(
      .asub(y, 1:3, dims = margin),
      new,
      .asub(y, 4:dim(y)[margin], dims = margin), ._along = margin
    )
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin, 2),
    .abind(
      .asub(y, 1:2, dims = margin),
      new,
      .asub(y, 3:dim(y)[margin], dims = margin), ._along = margin
    )
  ) |> errorfun()
  enumerate <- enumerate + 4
}

y <- array(1:64, c(4,4,4))
new <- matrix(1:16, ncol = 4)
for(margin in 1:3) {
  expect_equivalent(
    sb_before(y, new, margin),
    .abind(new, y, ._along = margin)
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin),
    .abind(y, new, ._along = margin)
  ) |> errorfun()
  expect_equivalent(
    sb_before(y, new, margin, 4),
    .abind(
      .asub(y, 1:3, dims = margin),
      new,
      .asub(y, 4:dim(y)[margin], dims = margin), ._along = margin
    )
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin, 2),
    .abind(
      .asub(y, 1:2, dims = margin),
      new,
      .asub(y, 3:dim(y)[margin], dims = margin), ._along = margin
    )
  ) |> errorfun()
  enumerate <- enumerate + 4
}
