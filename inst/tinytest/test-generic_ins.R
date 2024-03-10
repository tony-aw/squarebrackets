
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
  as.factor,
  as.list
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

# data.frames - rows ====
x. <- data.frame(a = 1:5, b = letters[1:5])
new. <- data.frame(a = 10:11, b = letters[10:11])
as_funs_lst <- list(
  as.data.frame,
  data.table::as.data.table,
  tibble::as_tibble,
  tidytable::as_tidytable
)
margin <- 1
for(i in 1:length(as_funs_lst)) {
  y <- as_funs_lst[[i]](x.)
  new <- as_funs_lst[[i]](new.)
  expect_equivalent(
    sb_before(y, new, margin),
    rbind(new, y)
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin),
    rbind(y, new)
  ) |> errorfun()
  expect_equivalent(
    sb_before(y, new, margin, 4),
    rbind(
      y[1:3, ],
      new,
      y[4:5, ]
    )
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin, 2),
    rbind(
      y[1:2, ],
      new,
      y[3:5, ]
    )
  ) |> errorfun()
  enumerate <- enumerate + 4
}

# data.frames - cols ====
x. <- data.frame(a = 1:5, b = letters[1:5], c = 6:10, d = letters[6:10], e = 101:105)
new. <- data.frame(f = 106:110)
as_funs_lst <- list(
  as.data.frame,
  data.table::as.data.table,
  tibble::as_tibble,
  tidytable::as_tidytable
)

margin <- 2
for(i in 1:length(as_funs_lst)) {
  y <- as_funs_lst[[i]](x.)
  new <- as_funs_lst[[i]](new.)
  expect_equivalent(
    sb_before(y, new, margin),
    cbind(new, y)
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin),
    cbind(y, new)
  ) |> errorfun()
  expect_equivalent(
    sb_before(y, new, margin, 4),
    cbind(
      y[, 1:3],
      new,
      y[, 4:5]
    )
  ) |> errorfun()
  expect_equivalent(
    sb_after(y, new, margin, 2),
    cbind(
      y[, 1:2],
      new,
      y[, 3:5]
    )
  ) |> errorfun()
  enumerate <- enumerate + 4
}

