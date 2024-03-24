
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
.abind <- squarebrackets:::.abind
.asub <- squarebrackets:::.asub

# list ====
y <- as.list(1:10)
new <- as.list(11:20)

expect_equivalent(
  sb2_before(y, new),
  c(new, y)
) |> errorfun()
expect_equivalent(
  sb2_after(y, new),
  c(y, new)
) |> errorfun()
expect_equivalent(
  sb2_before(y, new, 9),
  c(y[1:8], new, y[9:10])
) |> errorfun()
expect_equivalent(
  sb2_after(y, new, 2),
  c(y[1:2], new, y[3:10])
) |> errorfun()
enumerate <- enumerate + 4


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
    sb2_before(y, new, margin),
    rbind(new, y)
  ) |> errorfun()
  expect_equivalent(
    sb2_after(y, new, margin),
    rbind(y, new)
  ) |> errorfun()
  expect_equivalent(
    sb2_before(y, new, margin, 4),
    rbind(
      y[1:3, ],
      new,
      y[4:5, ]
    )
  ) |> errorfun()
  expect_equivalent(
    sb2_after(y, new, margin, 2),
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
    sb2_before(y, new, margin),
    cbind(new, y)
  ) |> errorfun()
  expect_equivalent(
    sb2_after(y, new, margin),
    cbind(y, new)
  ) |> errorfun()
  expect_equivalent(
    sb2_before(y, new, margin, 4),
    cbind(
      y[, 1:3],
      new,
      y[, 4:5]
    )
  ) |> errorfun()
  expect_equivalent(
    sb2_after(y, new, margin, 2),
    cbind(
      y[, 1:2],
      new,
      y[, 3:5]
    )
  ) |> errorfun()
  enumerate <- enumerate + 4
}

